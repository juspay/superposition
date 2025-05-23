export interface SuperpositionConfig {
    endpoint: string;
    token?: {
        token: string;
    };
    org_id?: string;
    workspace_id?: string;
}

export interface CACClientOptions {
    defaults?: any; // default config to use if remote fetch fails
    cache?: {
        // how long to cache a particular config (full) fetch response, -1 for infinite caching
        ttl?: number;
    };
    evaluationCache?: {
        // configures evaluation caching
        ttl?: number;
        size?: number;
    }
}

export interface ExperimentationOptions {
    defaultToss?: number;
    filterPrefixes?: string[];
}

export interface ConfigData {
    default_configs: Record<string, any>;
    contexts: any[];
    overrides: Record<string, Record<string, any>>;
}

export interface EvalOptions {
    refresh?: boolean;
    filterKeys?: string[];
    mergeStrategy?: 'merge' | 'replace';
}