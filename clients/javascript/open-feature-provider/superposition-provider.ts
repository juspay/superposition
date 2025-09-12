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
    OpenFeature,
    Hook,
    Logger,
} from '@openfeature/server-sdk';

import { SuperpositionClient } from './superposition-client';
import { convertToBoolean, convertToString, convertToNumber, convertToObject, getNestedValue, isNetworkError, categorizeError } from './utils';
import {
    SuperpositionOptions,
    EvaluationCacheOptions,
    RefreshStrategy,
    ConfigData,
    ExperimentationOptions,
} from './types';
import { NativeResolver } from 'superposition-bindings';

export interface SuperpositionProviderOptions {
    endpoint: string;
    token: string;
    org_id: string;
    workspace_id: string;
    httpClient?: any;
    fallbackConfig?: ConfigData;
    evaluationCache?: EvaluationCacheOptions;
    refreshStrategy?: RefreshStrategy;
    experimentationOptions?: ExperimentationOptions;
}

type ConverterFunction<T> = (value: any, defaultValue: T) => T;

const TYPE_CONVERTERS: Record<string, ConverterFunction<any>> = {
    boolean: convertToBoolean as ConverterFunction<boolean>,
    string: convertToString as ConverterFunction<string>,
    number: convertToNumber as ConverterFunction<number>,
    object: convertToObject as ConverterFunction<any>,
};

type ValueType = keyof typeof TYPE_CONVERTERS;

export class SuperpositionProvider implements Provider {
    readonly metadata: ProviderMetadata = {
        name: 'SuperpositionProvider',
        slug: 'superposition-provider'
    };

    events = new OpenFeatureEventEmitter();
    readonly hooks: Hook[] = [];

    private client: SuperpositionClient;
    status: ProviderStatus = ProviderStatus.NOT_READY;
    private lastError: string | null = null;
    private connectionRetryCount = 0;
    private maxRetries = 3;

    // Cache for processed contexts
    private processedContextCache = new WeakMap<EvaluationContext, Record<string, any>>();

    constructor(private config: SuperpositionProviderOptions) {
        this.client = new SuperpositionClient(
            {
                endpoint: config.endpoint,
                token: config.token,
                org_id: config.org_id,
                workspace_id: config.workspace_id,
            },
            new NativeResolver(),
            {
                fallbackConfig: config.fallbackConfig,
                evaluationCache: config.evaluationCache,
                refreshStrategy: config.refreshStrategy,
            },
            config.experimentationOptions
        );
    }

    async initialize(context?: EvaluationContext): Promise<void> {
        this.status = ProviderStatus.NOT_READY;
        try {
            await this.client.initialize();
            await this.client.eval(context || {});
            this.status = ProviderStatus.READY;
            this.connectionRetryCount = 0;
            this.lastError = null;
            this.events.emit(ProviderEvents.Ready, { message: 'Provider ready' });
        } catch (error) {
            this.handleInitializationError(error);
            throw error;
        }
    }

    private handleInitializationError(error: unknown): void {
        const errorMessage = categorizeError(error, this.config.endpoint);
        this.lastError = errorMessage;
        
        console.warn(`SuperpositionProvider: ${errorMessage}`);

        // Set appropriate status based on error type
        if (isNetworkError(error)) {
            this.status = ProviderStatus.ERROR;
            this.events.emit(ProviderEvents.Error, { 
                message: errorMessage, 
                errorCode: ErrorCode.PROVIDER_NOT_READY 
            });
        } else {
            this.status = ProviderStatus.FATAL;
            this.events.emit(ProviderEvents.Error, { 
                message: errorMessage, 
                errorCode: ErrorCode.PROVIDER_FATAL 
            });
        }
    }

    async onClose(): Promise<void> {
        this.status = ProviderStatus.NOT_READY;
        this.processedContextCache = new WeakMap();
        await this.client.close();
    }

    private async evaluateFlag<T>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext,
        type: ValueType
    ): Promise<T> {
        let processedContext = this.processedContextCache.get(context);

        if (!processedContext) {
            processedContext = this.filterContext(context);
            this.processedContextCache.set(context, processedContext);
        }

        try {
            const config = await this.client.eval(processedContext);
            const value = getNestedValue(config, flagKey);
            const converter = TYPE_CONVERTERS[type] as ConverterFunction<T>;
            return converter(value, defaultValue);
        } catch (error) {
            // Handle server connectivity issues gracefully
            if (isNetworkError(error) && this.connectionRetryCount < this.maxRetries) {
                this.connectionRetryCount++;
                
                // Set status to STALE if we have fallback data
                if (this.config.fallbackConfig) {
                    this.status = ProviderStatus.STALE;
                }
            }
            
            throw error;
        }
    }

    private filterContext(context: EvaluationContext): Record<string, any> {
        const filtered: Record<string, any> = {};

        for (const [key, value] of Object.entries(context)) {
            if (key.startsWith('__') || key === 'targetingKey' || key === 'timestamp') {
                continue;
            }

            // Only include simple, serializable types
            if (typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') {
                filtered[key] = value;
            } else if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
                try {
                    const serialized = JSON.stringify(value);
                    if (serialized.length < 1000 && Object.keys(value).length < 10) {
                        filtered[key] = value;
                    }
                } catch {
                    // Skip non-serializable objects
                }
            }
        }

        return filtered;
    }

    private createResolver<T>(type: ValueType) {
        return async (flagKey: string, defaultValue: T, context: EvaluationContext): Promise<ResolutionDetails<T>> => {
            if (this.status !== ProviderStatus.READY && this.status !== ProviderStatus.STALE) {
                return {
                    value: defaultValue,
                    reason: 'ERROR',
                    errorCode: this.status === ProviderStatus.FATAL ? ErrorCode.PROVIDER_FATAL : ErrorCode.PROVIDER_NOT_READY,
                    errorMessage: this.lastError || `Provider status: ${this.status}`
                };
            }

            try {
                const value = await this.evaluateFlag(flagKey, defaultValue, context, type);
                
                // Reset retry count on successful evaluation
                if (this.connectionRetryCount > 0) {
                    this.connectionRetryCount = 0;
                    if (this.status === ProviderStatus.STALE) {
                        this.status = ProviderStatus.READY;
                    }
                }
                
                return {
                    value,
                    reason: this.status === ProviderStatus.STALE ? 'STALE' : 'TARGETING_MATCH',
                };
            } catch (error) {
                const errorMessage = categorizeError(error, this.config.endpoint);
                
                // Only log network errors once to avoid spam
                if (isNetworkError(error) && this.connectionRetryCount === 1) {
                    console.warn(`SuperpositionProvider: ${errorMessage}. Using default values.`);
                }
                
                return {
                    value: defaultValue,
                    reason: 'ERROR',
                    errorCode: isNetworkError(error) ? ErrorCode.PROVIDER_NOT_READY : ErrorCode.GENERAL,
                    errorMessage
                };
            }
        };
    }

    async resolveBooleanEvaluation(
        flagKey: string,
        defaultValue: boolean,
        context: EvaluationContext,
    ): Promise<ResolutionDetails<boolean>> {
        return this.createResolver<boolean>('boolean')(flagKey, defaultValue, context);
    }

    async resolveStringEvaluation(
        flagKey: string,
        defaultValue: string,
        context: EvaluationContext,
    ): Promise<ResolutionDetails<string>> {
        return this.createResolver<string>('string')(flagKey, defaultValue, context);
    }

    async resolveNumberEvaluation(
        flagKey: string,
        defaultValue: number,
        context: EvaluationContext,
    ): Promise<ResolutionDetails<number>> {
        return this.createResolver<number>('number')(flagKey, defaultValue, context);
    }

    async resolveObjectEvaluation<T extends JsonValue>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext,
        logger?: Logger
    ): Promise<ResolutionDetails<T>> {
        return this.createResolver<T>('object')(flagKey, defaultValue, context);
    }

    async resolveAllConfigDetails(
        defaultValue: Record<string, any>,
        context: EvaluationContext
    ): Promise<Record<string, any>> {
        if (this.status !== ProviderStatus.READY && this.status !== ProviderStatus.STALE) {
            return defaultValue;
        }

        try {
            const processedContext = this.processedContextCache.get(context) || this.filterContext(context);
            if (!this.processedContextCache.has(context)) {
                this.processedContextCache.set(context, processedContext);
            }

            const targetingKey = context.targetingKey;
            return await this.client.getAllConfigValue(defaultValue, processedContext, targetingKey);
        } catch (error) {
            const errorMessage = categorizeError(error, this.config.endpoint);
            
            if (isNetworkError(error)) {
                console.warn(`SuperpositionProvider: ${errorMessage}. Using default configuration.`);
            }
            
            return defaultValue;
        }
    }

    getStatus(): ProviderStatus { return this.status; }
    getSuperpositionClient(): SuperpositionClient { return this.client; }
    getLastError(): string | null { return this.lastError; }
    
    // Expose getApplicableVariants on the provider layer
    async getApplicableVariants(
        queryData: Record<string, any>,
        identifier: string,
        filterPrefixes?: string[]
    ): Promise<string[]> {
        try {
            return await this.client.getApplicableVariants(queryData, identifier, filterPrefixes);
        } catch (error) {
            if (isNetworkError(error)) {
                console.warn('SuperpositionProvider: Unable to fetch applicable variants. Server may be down.');
            }
            return [];
        }
    }
}