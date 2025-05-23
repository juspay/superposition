import {
    EvaluationContext,
    Provider,
    JsonValue,
    ResolutionDetails,
    ProviderStatus,
    ProviderEvents,
    OpenFeatureEventEmitter,
    ErrorCode,
    ProviderMetadata,
    AnyProviderEvent,
    Hook,
    EventDetails,
    ProviderEventEmitter
} from '@openfeature/server-sdk';

import { ConfigurationClient } from './configuration-client';
import { SuperpositionConfig, CACClientOptions } from './types';
import { NativeResolver } from './native-resolver';

export interface SuperpositionProviderConfig {
    /**
     * Superposition service configuration
     */
    superpositionConfig: SuperpositionConfig;

    /**
     * Client options for caching and defaults
     */
    clientOptions?: CACClientOptions;

    /**
     * Polling configuration for real-time updates
     */
    pollingOpts?: {
        enabled?: boolean;          // Enable polling (default: true)
        interval?: number;          // Polling interval in milliseconds (default: 60000)
        onError?: 'stale' | 'error'; // Behavior on polling error (default: 'stale')
    };

    /**
     * Initialization options
     */
    initOpts?: {
        timeout?: number;           // Initialization timeout in milliseconds (default: 5000)
    };
}

export class SuperpositionProvider implements Provider {
    readonly metadata: ProviderMetadata = {
        name: 'SuperpositionProvider',
        slug: 'superposition-provider'
    };

    events?: ProviderEventEmitter<AnyProviderEvent> | undefined = new OpenFeatureEventEmitter();
    readonly hooks: Hook[] = [];

    private client: ConfigurationClient;
    status: ProviderStatus = ProviderStatus.NOT_READY;
    private pollingInterval?: NodeJS.Timeout;
    private config: SuperpositionProviderConfig;
    private cachedConfig: Record<string, any> = {};

    constructor(config: SuperpositionProviderConfig) {
        this.config = {
            clientOptions: {},
            pollingOpts: {
                enabled: true,
                interval: 60000,
                onError: 'stale'
            },
            initOpts: {
                timeout: 5000
            },
            ...config
        };

        this.client = new ConfigurationClient(
            this.config.superpositionConfig,
            new NativeResolver(),
            this.config.clientOptions
        );
    }

    /**
     * Initialize the provider
     */
    async initialize(context?: EvaluationContext): Promise<void> {
        try {
            this.status = ProviderStatus.NOT_READY;

            // Set up initialization timeout
            const initPromise = this.doInitialize(context);
            const timeoutPromise = new Promise<never>((_, reject) => {
                setTimeout(() => {
                    reject(new Error(`Provider initialization timed out after ${this.config.initOpts?.timeout}ms`));
                }, this.config.initOpts?.timeout);
            });

            await Promise.race([initPromise, timeoutPromise]);

            this.status = ProviderStatus.READY;
            this.events?.emit(ProviderEvents.Ready, {
                message: 'Provider initialized successfully'
            });

            // Start polling if enabled
            if (this.config.pollingOpts?.enabled) {
                this.startPolling();
            }

        } catch (error) {
            this.status = ProviderStatus.ERROR;
            const errorMessage = error instanceof Error ? error.message : 'Unknown initialization error';
            this.events?.emit(ProviderEvents.Error, {
                message: errorMessage,
                errorCode: ErrorCode.PROVIDER_NOT_READY
            });
            throw error;
        }
    }

    private async doInitialize(context?: EvaluationContext): Promise<void> {
        try {
            // Test connection by fetching initial configuration
            const testContext = context || { test: true };
            this.cachedConfig = await this.client.eval(testContext);
        } catch (error) {
            throw new Error(`Failed to initialize provider: ${error instanceof Error ? error.message : 'Unknown error'}`);
        }
    }

    /**
     * Shutdown the provider
     */
    async onClose(): Promise<void> {
        try {
            this.stopPolling();
            this.status = ProviderStatus.NOT_READY;
            this.cachedConfig = {};
        } catch (error) {
            console.warn('Error during provider shutdown:', error);
        }
    }

    /**
     * Handle context changes (for client-side providers)
     */
    async onContextChange?(oldContext: EvaluationContext, newContext: EvaluationContext): Promise<void> {
        try {
            // Refresh configuration with new context
            await this.refreshConfiguration(newContext);
            this.events?.emit(ProviderEvents.ConfigurationChanged, {
                message: 'Configuration refreshed due to context change'
            });
        } catch (error) {
            this.handleError(error, 'Context change failed');
        }
    }

    // Flag evaluation methods using ConfigurationClient's OpenFeature-compatible methods
    async resolveBooleanEvaluation(
        flagKey: string,
        defaultValue: boolean,
        context: EvaluationContext
    ): Promise<ResolutionDetails<boolean>> {
        return this.resolveEvaluation(
            () => this.client.getBooleanValue(flagKey, defaultValue, context),
            flagKey,
            defaultValue,
            'boolean'
        );
    }

    async resolveStringEvaluation(
        flagKey: string,
        defaultValue: string,
        context: EvaluationContext
    ): Promise<ResolutionDetails<string>> {
        return this.resolveEvaluation(
            () => this.client.getStringValue(flagKey, defaultValue, context),
            flagKey,
            defaultValue,
            'string'
        );
    }

    async resolveNumberEvaluation(
        flagKey: string,
        defaultValue: number,
        context: EvaluationContext
    ): Promise<ResolutionDetails<number>> {
        return this.resolveEvaluation(
            () => this.client.getIntegerValue(flagKey, defaultValue, context),
            flagKey,
            defaultValue,
            'number'
        );
    }

    async resolveObjectEvaluation<T extends JsonValue>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext
    ): Promise<ResolutionDetails<T>> {
        return this.resolveEvaluation(
            () => this.client.getObjectValue(flagKey, defaultValue, context),
            flagKey,
            defaultValue,
            'object'
        );
    }

    private async resolveEvaluation<T>(
        evaluationFn: () => Promise<T>,
        flagKey: string,
        defaultValue: T,
        expectedType: string
    ): Promise<ResolutionDetails<T>> {
        try {
            // Check provider status
            if (this.status === ProviderStatus.FATAL) {
                return {
                    value: defaultValue,
                    reason: 'ERROR',
                    errorCode: ErrorCode.PROVIDER_FATAL,
                    errorMessage: 'Provider is in FATAL state'
                };
            }

            if (this.status === ProviderStatus.NOT_READY) {
                return {
                    value: defaultValue,
                    reason: 'ERROR',
                    errorCode: ErrorCode.PROVIDER_NOT_READY,
                    errorMessage: 'Provider is not ready'
                };
            }

            // Use ConfigurationClient's methods which handle caching and evaluation
            const value = await evaluationFn();

            return {
                value,
                reason: this.status === ProviderStatus.STALE ? 'STALE' : 'TARGETING_MATCH',
                variant: typeof value === 'string' ? value : undefined
            };

        } catch (error) {
            this.handleError(error, `Flag evaluation failed for '${flagKey}'`);

            return {
                value: defaultValue,
                reason: 'ERROR',
                errorCode: ErrorCode.GENERAL,
                errorMessage: error instanceof Error ? error.message : 'Unknown evaluation error'
            };
        }
    }

    private startPolling(): void {
        if (this.pollingInterval) {
            clearInterval(this.pollingInterval);
        }

        const interval = this.config.pollingOpts?.interval || 60000;
        this.pollingInterval = setInterval(async () => {
            try {
                await this.refreshConfiguration();
            } catch (error) {
                this.handlePollingError(error);
            }
        }, interval);
    }

    private stopPolling(): void {
        if (this.pollingInterval) {
            clearInterval(this.pollingInterval);
            this.pollingInterval = undefined;
        }
    }

    private async refreshConfiguration(context: EvaluationContext = {}): Promise<void> {
        try {
            const oldConfig = { ...this.cachedConfig };

            // Use ConfigurationClient's refreshCache method
            await this.client.refreshCache();
            const newConfig = await this.client.eval(context);

            // Check if configuration actually changed
            const configChanged = JSON.stringify(oldConfig) !== JSON.stringify(newConfig);

            if (configChanged) {
                this.cachedConfig = newConfig;

                // Emit configuration changed event
                this.events?.emit(ProviderEvents.ConfigurationChanged, {
                    message: 'Configuration updated',
                    flagsChanged: this.getChangedFlags(oldConfig, newConfig)
                });

                // If we were in STALE state, move back to READY
                if (this.status === ProviderStatus.STALE) {
                    this.status = ProviderStatus.READY;
                    this.events?.emit(ProviderEvents.Ready, {
                        message: 'Provider recovered from STALE state'
                    });
                }
            }
        } catch (error) {
            throw error; // Let the caller handle the error
        }
    }

    private handlePollingError(error: unknown): void {
        const errorMessage = error instanceof Error ? error.message : 'Unknown polling error';
        console.warn('Polling update failed:', errorMessage);

        const onError = this.config.pollingOpts?.onError || 'stale';

        if (onError === 'stale' && this.status === ProviderStatus.READY) {
            this.status = ProviderStatus.STALE;
            this.events?.emit(ProviderEvents.Stale, {
                message: 'Provider is using cached data due to polling failure',
                errorMessage
            });
        } else if (onError === 'error') {
            this.status = ProviderStatus.ERROR;
            this.events?.emit(ProviderEvents.Error, {
                message: `Polling failed: ${errorMessage}`,
                errorCode: ErrorCode.GENERAL
            });
        }
    }

    private getChangedFlags(oldConfig: Record<string, any>, newConfig: Record<string, any>): string[] {
        const allKeys = new Set([...Object.keys(oldConfig), ...Object.keys(newConfig)]);
        const changedFlags: string[] = [];

        for (const key of allKeys) {
            if (JSON.stringify(oldConfig[key]) !== JSON.stringify(newConfig[key])) {
                changedFlags.push(key);
            }
        }

        return changedFlags;
    }

    private handleError(error: unknown, context: string): void {
        const errorMessage = error instanceof Error ? error.message : 'Unknown error';
        console.error(`${context}:`, errorMessage);

        // Determine if this is a fatal error
        const isFatal = this.isFatalError(error);

        if (isFatal && this.status !== ProviderStatus.FATAL) {
            this.status = ProviderStatus.FATAL;
            this.events?.emit(ProviderEvents.Error, {
                message: `Fatal error: ${errorMessage}`,
                errorCode: ErrorCode.PROVIDER_FATAL
            });
        } else if (!isFatal && this.status === ProviderStatus.READY) {
            this.status = ProviderStatus.ERROR;
            this.events?.emit(ProviderEvents.Error, {
                message: errorMessage,
                errorCode: ErrorCode.GENERAL
            });
        }
    }

    private isFatalError(error: unknown): boolean {
        if (error instanceof Error) {
            // Consider authentication/authorization errors as fatal
            return error.message.includes('401') ||
                error.message.includes('403') ||
                error.message.includes('Unauthorized') ||
                error.message.includes('Forbidden');
        }
        return false;
    }

    /**
     * Get current provider status
     */
    getStatus(): ProviderStatus {
        return this.status;
    }

    /**
     * Force refresh configuration
     */
    async forceRefresh(context: EvaluationContext = {}): Promise<void> {
        await this.refreshConfiguration(context);
    }

    /**
     * Get the underlying ConfigurationClient for direct access
     */
    getConfigurationClient(): ConfigurationClient {
        return this.client;
    }
}