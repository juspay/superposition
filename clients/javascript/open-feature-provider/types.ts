import { EvaluationContext } from "@openfeature/server-sdk";

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

export interface ConfigData {
    default_configs: Record<string, any>;
    contexts: any[];
    overrides: Record<string, Record<string, any>>;
    dimensions: Record<string, Record<string, any>>;
    fetched_at: Date;
}

/**
 * Experiment data fetched from a source
 */
export interface ExperimentData {
    experiments: any[];
    experiment_groups: any[];
    fetched_at: Date;
}

/**
 * Interface for bulk configuration resolution
 */
export interface AllFeatureProvider {
    /**
     * Resolve all features for the given evaluation context
     */
    resolveAllFeatures(context: EvaluationContext): Promise<Record<string, any>>;

    /**
     * Resolve features matching prefix filters
     */
    resolveAllFeaturesWithFilter(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): Promise<Record<string, any>>;
}

/**
 * Interface for experiment metadata and variant resolution
 */
export interface FeatureExperimentMeta {
    /**
     * Get applicable variant IDs for the given context
     */
    getApplicableVariants(context: EvaluationContext): Promise<string[]>;
}

/**
 * Interface for abstracting data sources
 */
export interface SuperpositionDataSource {
    /**
     * Fetch the latest configuration from the data source
     */
    fetchConfig(): Promise<ConfigData>;

    /**
     * Fetch configuration with context/prefix filters
     */
    fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ConfigData>;

    /**
     * Fetch all active experiment data
     */
    fetchActiveExperiments(): Promise<ExperimentData | null>;

    /**
     * Fetch active experiments filtered with partial context matching
     */
    fetchCandidateActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ExperimentData | null>;

    /**
     * Fetch active experiments filtered with exact context matching
     */
    fetchMatchingActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ExperimentData | null>;

    /**
     * Check if this data source supports experiments
     */
    supportsExperiments(): boolean;

    /**
     * Close and cleanup resources
     */
    close(): Promise<void>;
}

export interface FfiExperiment {
    id: string;
    context: Record<string, string>;
    variants: Variant[];
    traffic_percentage: number;
}

export interface Variant {
    id: string;
    variant_type: "CONTROL" | "EXPERIMENTAL";
    context_id?: string;
    override_id?: string;
    overrides: Record<string, string>;
}

export interface FfiExperimentGroup {
    id: string;
    context: Record<string, string>;
    traffic_percentage: number;
    member_experiment_ids: string[];
    group_type: "SYSTEM_GENERATED" | "USER_CREATED";
    buckets: Bucket[];
}

export interface Bucket {
    variant_id: string;
    experiment_id: string;
}

export interface ExperimentationArgs {
    experiments: FfiExperiment[];
    experiment_groups: FfiExperimentGroup[];
    targeting_key: string;
}
