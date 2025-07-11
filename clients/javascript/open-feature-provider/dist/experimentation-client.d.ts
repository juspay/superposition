import { SuperpositionOptions, ExperimentationOptions } from './types';
export interface Variant {
    id: string;
    variant_type: 'CONTROL' | 'EXPERIMENTAL';
    context_id?: string;
    override_id?: string;
    overrides: Record<string, string>;
}
export interface Experiment {
    id: string;
    context: Record<string, string>;
    variants: Variant[];
    traffic_percentage: number;
}
export declare class ExperimentationClient {
    private superpositionOptions;
    private smithyClient;
    private options;
    private cachedExperiments;
    private lastUpdated;
    private evaluationCache;
    private pollingInterval?;
    constructor(superpositionOptions: SuperpositionOptions, experimentOptions: ExperimentationOptions);
    initialize(): Promise<void>;
    private startPolling;
    private fetchExperiments;
    /**
     * Normalize any value to a Record<string, string> format
     * This ensures compatibility with the expected interface
     */
    private normalizeToStringRecord;
    getExperiments(): Promise<Experiment[]>;
    generateCacheKey(queryData: Record<string, any>): string;
    getFromEvalCache(key: string): any | undefined;
    setEvalCache(key: string, value: any): void;
    clearEvalCache(): void;
    close(): Promise<void>;
}
