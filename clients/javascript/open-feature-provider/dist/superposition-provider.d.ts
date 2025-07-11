import { EvaluationContext, Provider, JsonValue, ResolutionDetails, ProviderStatus, OpenFeatureEventEmitter, ProviderMetadata, Hook, Logger } from '@openfeature/server-sdk';
import { ConfigurationClient } from './configuration-client';
import { SuperpositionOptions, EvaluationCacheOptions, RefreshStrategy, ConfigData, ExperimentationOptions } from './types';
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
export declare class SuperpositionProvider implements Provider {
    private config;
    readonly metadata: ProviderMetadata;
    events: OpenFeatureEventEmitter;
    readonly hooks: Hook[];
    private client;
    status: ProviderStatus;
    private processedContextCache;
    constructor(config: SuperpositionProviderOptions);
    initialize(context?: EvaluationContext): Promise<void>;
    onClose(): Promise<void>;
    private evaluateFlag;
    private filterContext;
    private createResolver;
    resolveBooleanEvaluation(flagKey: string, defaultValue: boolean, context: EvaluationContext): Promise<ResolutionDetails<boolean>>;
    resolveStringEvaluation(flagKey: string, defaultValue: string, context: EvaluationContext): Promise<ResolutionDetails<string>>;
    resolveNumberEvaluation(flagKey: string, defaultValue: number, context: EvaluationContext): Promise<ResolutionDetails<number>>;
    resolveObjectEvaluation<T extends JsonValue>(flagKey: string, defaultValue: T, context: EvaluationContext, logger?: Logger): Promise<ResolutionDetails<T>>;
    resolveAllConfigDetails(defaultValue: Record<string, any>, context: EvaluationContext): Promise<Record<string, any>>;
    getStatus(): ProviderStatus;
    getConfigurationClient(): ConfigurationClient;
}
export { SuperpositionOptions };
