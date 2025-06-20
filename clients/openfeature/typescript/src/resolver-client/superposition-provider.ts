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
    Hook,
    Logger,
} from '@openfeature/server-sdk';

import { ConfigurationClient } from './configuration-client';
import { ExperimentationClient } from './experiment-client';
import { convertToBoolean, convertToString, convertToNumber, convertToObject, getNestedValue } from './utils';
import {
    SuperpositionOptions,
    EvaluationCacheOptions,
    ExperimentationOptions,
    RefreshStrategy,
    ConfigData
} from './types';
import { NativeResolver } from './native-resolver';

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

interface EvaluationResult<T> {
    value: T;
    isFromExperiment: boolean;
}

export class SuperpositionProvider implements Provider {
    readonly metadata: ProviderMetadata = {
        name: 'SuperpositionProvider',
        slug: 'superposition-provider'
    };

    events = new OpenFeatureEventEmitter();
    readonly hooks: Hook[] = [];

    private client: ConfigurationClient;
    private experimentationClient?: ExperimentationClient;
    private defaultToss = Math.random() * 1000000;
    status: ProviderStatus = ProviderStatus.NOT_READY;

    // Cache for processed contexts (includes enrichment + filtering)
    private processedContextCache = new WeakMap<EvaluationContext, { filtered: Record<string, any>, overrides?: Record<string, any> }>();

    constructor(private config: SuperpositionProviderOptions) {
        this.client = new ConfigurationClient(
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
            }
        );

        if (config.experimentationOptions) {
            this.experimentationClient = new ExperimentationClient(new NativeResolver());
        }
    }

    async initialize(context?: EvaluationContext): Promise<void> {
        this.status = ProviderStatus.NOT_READY;
        try {
            await this.client.eval(context || {});
            await this.experimentationClient?.refreshCache();
            this.status = ProviderStatus.READY;
            this.events.emit(ProviderEvents.Ready, { message: 'Provider ready' });
        } catch (error) {
            this.status = ProviderStatus.ERROR;
            const message = error instanceof Error ? error.message : 'Initialization failed';
            this.events.emit(ProviderEvents.Error, { message, errorCode: ErrorCode.PROVIDER_NOT_READY });
            throw error;
        }
    }

    async onClose(): Promise<void> {
        this.status = ProviderStatus.NOT_READY;
        this.processedContextCache = new WeakMap();
    }

    private async evaluateFlag<T>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext,
        type: ValueType
    ): Promise<EvaluationResult<T>> {

        let processed = this.processedContextCache.get(context);

        if (!processed) {
            processed = await this.processContext(context);
            this.processedContextCache.set(context, processed);
        }

        if (processed.overrides?.[flagKey] !== undefined) {
            const converter = TYPE_CONVERTERS[type] as ConverterFunction<T>;
            return {
                value: converter(processed.overrides[flagKey], defaultValue),
                isFromExperiment: true
            };
        }

        const config = await this.client.eval(processed.filtered);
        const value = getNestedValue(config, flagKey);
        const converter = TYPE_CONVERTERS[type] as ConverterFunction<T>;

        return {
            value: converter(value, defaultValue),
            isFromExperiment: false
        };
    }

    private async processContext(context: EvaluationContext): Promise<{ filtered: Record<string, any>, overrides?: Record<string, any> }> {
        let overrides: Record<string, any> | undefined;

        if (this.experimentationClient && !(context as any).__experiment_enabled) {
            try {

                const toss = convertToNumber(context.toss, this.defaultToss);
                const result = this.experimentationClient.evaluateExperiments(
                    context,
                    toss,
                    { context_eval: 'Full', variant_selection: 'Simple', include_reasoning: false }
                );
                overrides = result.overrides || {};
            } catch (error) {
                console.warn('Experiment context failed:', error);
            }
        } else if ((context as any).__experiment_overrides) {
            overrides = (context as any).__experiment_overrides;
        }

        const filtered: Record<string, any> = {};
        const SKIP_KEYS = new Set(['__', 'targetingKey', 'timestamp']);

        for (const [key, value] of Object.entries(context)) {
            if (SKIP_KEYS.has(key) || key.startsWith('__')) continue;

            const type = typeof value;
            if (type === 'string' || type === 'number' || type === 'boolean') {
                filtered[key] = value;
            } else if (type === 'object' && value && !Array.isArray(value)) {
                try {
                    const str = JSON.stringify(value);
                    if (str.length < 1000 && Object.keys(value).length < 10) {
                        filtered[key] = value;
                    }
                } catch { }
            }
        }

        return { filtered, overrides };
    }

    private createResolver<T>(type: ValueType) {
        return async (flagKey: string, defaultValue: T, context: EvaluationContext): Promise<ResolutionDetails<T>> => {
            if (this.status !== ProviderStatus.READY && this.status !== ProviderStatus.STALE) {
                return {
                    value: defaultValue,
                    reason: 'ERROR',
                    errorCode: this.status === ProviderStatus.FATAL ? ErrorCode.PROVIDER_FATAL : ErrorCode.PROVIDER_NOT_READY,
                    errorMessage: `Provider status: ${this.status}`
                };
            }

            try {
                const result = await this.evaluateFlag(flagKey, defaultValue, context, type);
                return {
                    value: result.value,
                    reason: this.status === ProviderStatus.STALE ? 'STALE' : 'TARGETING_MATCH',
                    variant: result.isFromExperiment ? 'experiment' : 'config'
                };
            } catch (error) {
                return {
                    value: defaultValue,
                    reason: 'ERROR',
                    errorCode: ErrorCode.GENERAL,
                    errorMessage: error instanceof Error ? error.message : 'Evaluation failed'
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

    getStatus(): ProviderStatus { return this.status; }
    getConfigurationClient(): ConfigurationClient { return this.client; }
    getExperimentationClient(): ExperimentationClient | undefined { return this.experimentationClient; }
    isExperimentationEnabled(): boolean { return !!this.experimentationClient; }
}

export { SuperpositionOptions };