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
import { convertToBoolean, convertToString, convertToNumber, convertToObject, getNestedValue } from './utils';
import {
    SuperpositionOptions,
    EvaluationCacheOptions,
    RefreshStrategy,
    ConfigData,
    ExperimentationOptions,
} from './types';
import { NativeResolver } from '@juspay/superposition-bindings';

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
    enableDetailedErrorLogging?: boolean; // New option to control error verbosity
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
    private enableDetailedErrorLogging: boolean;

    // Cache for processed contexts
    private processedContextCache = new WeakMap<EvaluationContext, Record<string, any>>();

    constructor(private config: SuperpositionProviderOptions) {
        this.enableDetailedErrorLogging = config.enableDetailedErrorLogging ?? false;
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
        const errorMessage = this.categorizeError(error);
        this.lastError = errorMessage;
        
        // Only log detailed errors if explicitly enabled
        if (this.enableDetailedErrorLogging) {
            console.error('SuperpositionProvider initialization failed:', error);
        } else {
            console.warn(`SuperpositionProvider: ${errorMessage}`);
        }

        // Set appropriate status based on error type
        if (this.isNetworkError(error)) {
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

    private categorizeError(error: unknown): string {
        if (!error) return 'Unknown error occurred';
        
        const errorMessage = error instanceof Error ? error.message : String(error);
        
        // Bad Request errors (400) - Configuration issues
        if (errorMessage.includes('Bad Request') || errorMessage.includes('parameter org id is required')) {
            return 'Bad Request: Missing or invalid org_id parameter. Please provide a valid org_id in your configuration';
        }
        
        if (errorMessage.includes('parameter workspace_id is required') || errorMessage.includes('parameter workspace id is required')) {
            return 'Bad Request: Missing or invalid workspace_id parameter. Please provide a valid workspace_id in your configuration';
        }
        
        if (errorMessage.includes('not found in organization')) {
            return 'Bad Request: The specified workspace_id does not exist in the given organization. Please check your workspace_id and org_id combination';
        }
        
        // Server configuration issues
        if (errorMessage.includes('Something went wrong')) {
            return 'Server configuration issue: The server responded but encountered an internal error. Check server logs for details';
        }
        
        // Network-related errors
        if (this.isNetworkError(error)) {
            return `Server connection failed. Please check if the Superposition server is running at ${this.config.endpoint}`;
        }
        
        // Authentication errors
        if (errorMessage.includes('401') || errorMessage.includes('403') || errorMessage.includes('unauthorized')) {
            return 'Authentication failed. Please verify your token and credentials';
        }
        
        // Not Found errors (404)
        if (errorMessage.includes('404') || errorMessage.includes('not found')) {
            return 'Resource not found. Please verify your org_id and workspace_id exist on the server';
        }
        
        // Generic error
        return `Initialization failed: ${errorMessage}`;
    }

    private isNetworkError(error: unknown): boolean {
        if (!error) return false;
        
        const errorMessage = error instanceof Error ? error.message : String(error);
        
        return errorMessage.includes('ECONNREFUSED') ||
               errorMessage.includes('ENOTFOUND') ||
               errorMessage.includes('ECONNRESET') ||
               errorMessage.includes('ETIMEDOUT') ||
               errorMessage.includes('fetch failed') ||
               errorMessage.includes('network') ||
               errorMessage.includes('connection');
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
            if (this.isNetworkError(error) && this.connectionRetryCount < this.maxRetries) {
                this.connectionRetryCount++;
                
                if (this.enableDetailedErrorLogging) {
                    console.warn(`SuperpositionProvider: Connection attempt ${this.connectionRetryCount}/${this.maxRetries} failed`);
                }
                
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
                const errorMessage = this.categorizeError(error);
                
                // Only log detailed errors if enabled
                if (this.enableDetailedErrorLogging) {
                    console.error(`SuperpositionProvider evaluation failed for flag "${flagKey}":`, error);
                } else if (this.isNetworkError(error)) {
                    // Only log network errors once to avoid spam
                    if (this.connectionRetryCount === 1) {
                        console.warn(`SuperpositionProvider: ${errorMessage}. Using default values.`);
                    }
                }
                
                return {
                    value: defaultValue,
                    reason: 'ERROR',
                    errorCode: this.isNetworkError(error) ? ErrorCode.PROVIDER_NOT_READY : ErrorCode.GENERAL,
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
            const errorMessage = this.categorizeError(error);
            
            if (this.enableDetailedErrorLogging) {
                console.error('Error resolving all config details:', error);
            } else if (this.isNetworkError(error)) {
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
            if (this.enableDetailedErrorLogging) {
                console.error('Error getting applicable variants:', error);
            } else if (this.isNetworkError(error)) {
                console.warn('SuperpositionProvider: Unable to fetch applicable variants. Server may be down.');
            }
            return [];
        }
    }
}