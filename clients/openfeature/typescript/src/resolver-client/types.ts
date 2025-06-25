export interface SuperpositionOptions {
    endpoint: string;
    token: string;
    org_id?: string;
    workspace_id?: string;
    httpClient?: any;
}

// Cache configuration interfaces
export interface CacheOptions {
    ttl?: number;
    size?: number;
}

export interface EvaluationCacheOptions {
    ttl?: number;
    size?: number;
}

export interface PollingStrategy {
    interval: number;
    timeout?: number;
}

export interface OnDemandStrategy {
    ttl: number;
    timeout?: number;
}

export type RefreshStrategy = PollingStrategy | OnDemandStrategy;

export interface ExperimentationOptions {
    refreshStrategy: RefreshStrategy;
    evaluationCache?: EvaluationCacheOptions;
}

export interface ConfigOptions {
    fallbackConfig?: Record<string, any>;
    evaluationCache?: EvaluationCacheOptions;
    refreshStrategy?: RefreshStrategy;
}

export interface ConfigData {
    default_configs: Record<string, any>;
    contexts: any[];
    overrides: Record<string, Record<string, any>>;
}