// The native FFI shapes live in the bindings package (the JS analog of Python/Java's UniFFI-generated
// types). Import them for local use (ExperimentationArgs below) and re-export them so existing
// `./types` importers keep resolving them from one source of truth.
import type {
    Config,
    Variant,
    FfiExperiment,
    Bucket,
    FfiExperimentGroup,
    ExperimentConfig,
} from "superposition-bindings";

export type {
    Config,
    Variant,
    FfiExperiment,
    Bucket,
    FfiExperimentGroup,
    ExperimentConfig,
};

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
    use_stale_on_error?: boolean;
}

export type RefreshStrategy = PollingStrategy | OnDemandStrategy;

export interface ExperimentationOptions {
    refreshStrategy: RefreshStrategy;
    evaluationCache?: EvaluationCacheOptions;
    defaultIdentifier?: string;
}

export interface ConfigOptions {
    fallbackConfig?: Record<string, any>;
    evaluationCache?: EvaluationCacheOptions;
    refreshStrategy?: RefreshStrategy;
}

export interface ExperimentationArgs {
    experiments: FfiExperiment[];
    experiment_groups: FfiExperimentGroup[];
    targeting_key: string;
}
