// experimentation-client.ts
import { SuperpositionConfig, CACClientOptions } from './types';
import { NativeResolver } from './native-resolver';

export interface ExperimentationOptions {
    defaultToss?: number;
    filterPrefixes?: string[];
}
export interface ExperimentResponse {
    id: string;
    name: string;
    traffic_percentage: number;
    context: Record<string, any>;
    variants: VariantResponse[];
    override_keys: string[];
    status: string;
}

export interface VariantResponse {
    id: string;
    experiment_id?: string;
    context: Record<string, any>;
    overrides: Record<string, any>;
    variant_type?: string;
}

export interface ExperimentData {
    experiments: any[];
    variants: any[];
    overrides: Record<string, Record<string, any>>;
}

export class ExperimentationClient {
    private config: SuperpositionConfig;
    private resolver: NativeResolver;
    private options: CACClientOptions;
    private cachedExperimentData: any = null;
    private lastFetchTime: number = 0;
    private evaluationCache = new Map<string, { value: any, timestamp: number }>();

    constructor(
        config: SuperpositionConfig,
        resolver: NativeResolver,
        options: CACClientOptions = {}
    ) {
        this.config = config;
        this.resolver = resolver;
        this.options = options;
    }

    // Main evaluation method (similar to ConfigurationClient.eval)
    async eval(
        userContext: Record<string, any>,
        experimentationOptions: ExperimentationOptions = {}
    ): Promise<any> {
        try {
            // Check evaluation cache first
            const cacheKey = this.generateEvaluationCacheKey(userContext, experimentationOptions);
            const cached = this.getFromEvaluationCache(cacheKey);

            if (cached) {
                console.log('Using cached experiment evaluation result');
                return cached;
            }

            // Get experiment data from server
            const experimentData = await this.getExperimentData();

            console.log('Experiment data received:', {
                experimentsCount: experimentData.experiments?.length || 0,
                variantsCount: experimentData.variants?.length || 0,
                overridesCount: Object.keys(experimentData.overrides || {}).length
            });

            // Call NativeResolver (same pattern as ConfigurationClient)
            const result = this.resolver.evaluateExperiments(
                experimentData.experiments || [],
                experimentData.variants || [],
                experimentData.overrides || {},
                userContext,
                experimentationOptions.defaultToss ?? -1,
                experimentationOptions.filterPrefixes || []
            );

            console.log('Experiment evaluation result:', result);

            // Cache the evaluation result
            this.setEvaluationCache(cacheKey, result);

            return result;
        } catch (error) {
            console.error('Experiment evaluation failed:', error);
            throw error;
        }
    }

    // Get experiment data (same pattern as ConfigurationClient.getConfigData)
    private async getExperimentData(forceRefresh = false): Promise<any> {
        const cacheTtl = this.options.cache?.ttl || 60000;
        const now = Date.now();

        if (!forceRefresh && this.cachedExperimentData && now - this.lastFetchTime < cacheTtl) {
            console.log('Using cached experiment data');
            return this.cachedExperimentData;
        }

        console.log('Fetching fresh experiment data from server');
        return this.fetchExperimentData();
    }

    private async fetchExperimentData(): Promise<any> {
        const headers: Record<string, string> = {
            'Content-Type': 'application/json',
            'Accept': 'application/json'
        };

        if (this.config.token?.token) {
            headers['Authorization'] = `Bearer ${this.config.token.token}`;
        }

        if (this.config.org_id) {
            headers['x-org-id'] = this.config.org_id;
        }

        if (this.config.workspace_id) {
            headers['x-tenant'] = this.config.workspace_id;
        }

        console.log('Fetching from:', `${this.config.endpoint}/experiments`);

        const response = await fetch(`${this.config.endpoint}/experiments`, {
            method: 'GET',
            headers
        });

        if (!response.ok) {
            const errorText = await response.text();
            throw new Error(`Failed to fetch experiments: ${response.status} ${response.statusText}`);
        }

        const data = await response.json();

        // Transform API response to format expected by FFI
        const experimentData = this.transformExperimentData(data);

        this.cachedExperimentData = experimentData;
        this.lastFetchTime = Date.now();
        this.clearEvaluationCache();

        return experimentData;
    }

    private transformExperimentData(apiResponse: any): any {
        // Transform API response to match FFI expected format
        const experiments = Array.isArray(apiResponse) ? apiResponse : (apiResponse.data || []);

        return {
            experiments: experiments.map((exp: ExperimentResponse) => ({
                id: exp.id,
                name: exp.name,
                traffic_percentage: exp.traffic_percentage,
                context: exp.context || {},
                variants: exp.variants?.map((v: VariantResponse) => v.id) || [],
                override_keys: exp.override_keys || [],
                status: exp.status
            })),
            variants: experiments.flatMap((exp: ExperimentResponse) =>
                (exp.variants || []).map(variant => ({
                    id: variant.id,
                    experiment_id: exp.id,
                    context: variant.context || {},
                    override_keys: Object.keys(variant.overrides || {})
                }))
            ),
            overrides: this.extractOverrides(experiments)
        };
    }

    private extractOverrides(experiments: any[]): Record<string, Record<string, any>> {
        const overrides: Record<string, Record<string, any>> = {};

        experiments.forEach(exp => {
            (exp.variants || []).forEach((variant: VariantResponse) => {
                if (variant.overrides) {
                    overrides[variant.id] = variant.overrides;
                }
            });
        });

        return overrides;
    }

    // Same caching pattern as ConfigurationClient
    private generateEvaluationCacheKey(userContext: Record<string, any>, options: ExperimentationOptions): string {
        return JSON.stringify({ userContext, options });
    }

    private getFromEvaluationCache(key: string): any | null {
        const cached = this.evaluationCache.get(key);
        if (!cached) return null;

        const ttl = this.options.evaluationCache?.ttl || 30000;
        const now = Date.now();

        if (now - cached.timestamp > ttl) {
            this.evaluationCache.delete(key);
            return null;
        }

        return cached.value;
    }

    private setEvaluationCache(key: string, value: any): void {
        const maxSize = this.options.evaluationCache?.size || 100;

        if (this.evaluationCache.size >= maxSize) {
            const oldestKey = this.evaluationCache.keys().next().value;
            if (oldestKey !== undefined) {
                this.evaluationCache.delete(oldestKey);
            }
        }

        this.evaluationCache.set(key, {
            value,
            timestamp: Date.now()
        });
    }

    private clearEvaluationCache(): void {
        this.evaluationCache.clear();
    }

    async refreshCache(): Promise<void> {
        await this.getExperimentData(true);
    }
}