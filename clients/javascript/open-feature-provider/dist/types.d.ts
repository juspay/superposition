export interface SuperpositionOptions {
    endpoint: string;
    token: string;
    org_id?: string;
    workspace_id?: string;
    httpClient?: any;
}
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
    use_stale_on_error?: boolean;
}
export type RefreshStrategy = PollingStrategy | OnDemandStrategy;
export interface ExperimentationOptions {
    refreshStrategy: RefreshStrategy;
    evaluationCache?: EvaluationCacheOptions;
    defaultToss?: number;
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
export interface FfiExperiment {
    id: string;
    context: Record<string, string>;
    variants: Variant[];
    traffic_percentage: number;
}
export interface Variant {
    id: string;
    variant_type: 'CONTROL' | 'EXPERIMENTAL';
    context_id?: string;
    override_id?: string;
    overrides: Record<string, string>;
}
export interface ExperimentationArgs {
    experiments: FfiExperiment[];
    targeting_key: string;
}
