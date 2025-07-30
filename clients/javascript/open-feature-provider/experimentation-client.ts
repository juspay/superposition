import { SuperpositionClient, ListExperimentCommand, ListExperimentCommandInput, ListExperimentGroupsCommandInput, ListExperimentGroupsCommand, GroupType } from '@juspay/superposition-sdk';
import { SuperpositionOptions, ExperimentationOptions, PollingStrategy, OnDemandStrategy } from './types';

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

export interface ExperimentGroup {
    id: string;
    context: Record<string, string>;
    member_experiment_ids: string[];
    traffic_percentage: number;
    group_type: GroupType;
}

export class ExperimentationClient {
    private smithyClient: SuperpositionClient;
    private options: ExperimentationOptions;
    private cachedExperiments: Experiment[] | null = null;
    private cachedExperimentGroups: ExperimentGroup[] | null = null;
    private lastUpdated: Date | null = null;
    private evaluationCache: Map<string, any> = new Map();
    private pollingInterval?: NodeJS.Timeout;

    constructor(
        private superpositionOptions: SuperpositionOptions,
        experimentOptions: ExperimentationOptions
    ) {
        this.options = experimentOptions;
        this.smithyClient = new SuperpositionClient({
            endpoint: superpositionOptions.endpoint,
            token: { token: superpositionOptions.token },
        });
    }

    async initialize(): Promise<void> {
        // Fetch initial experiments
        const experiments = await this.fetchExperiments();
        const experimentgroups = await this.fetchExperimentGroups();
        if (experiments && experimentgroups) {
            this.cachedExperiments = experiments;
            this.cachedExperimentGroups = experimentgroups;
            this.lastUpdated = new Date();
            console.log('Experiments and Experiment Groups fetched successfully.');
        }

        // Set up refresh strategy
        if (this.options.refreshStrategy && 'interval' in this.options.refreshStrategy) {
            const strategy = this.options.refreshStrategy as PollingStrategy;
            this.startPolling(strategy.interval);
        }
    }

    private startPolling(interval: number): void {
        this.pollingInterval = setInterval(async () => {
            try {
                const experiments = await this.fetchExperiments();
                const experimentGroups = await this.fetchExperimentGroups();
                if (experiments && experimentGroups) {
                    this.cachedExperiments = experiments;
                    this.cachedExperimentGroups = experimentGroups;
                    this.lastUpdated = new Date();
                    console.log('Experiments and Experiment Groups refreshed successfully.');
                }
            } catch (error) {
                console.error('Polling error:', error);
            }
        }, interval);
    }

    private async fetchExperiments(): Promise<Experiment[] | null> {
        try {
            const commandInput: ListExperimentCommandInput = {
                workspace_id: this.superpositionOptions.workspace_id,
                org_id: this.superpositionOptions.org_id,
                all: true
            };

            const command = new ListExperimentCommand(commandInput);
            const response = await this.smithyClient.send(command);

            if (!response.data) {
                return null;
            }

            // Convert response to internal format with proper type checking
            const experiments: Experiment[] = [];

            for (const exp of response.data) {
                // Skip experiments without required fields
                if (!exp.id) {
                    console.warn('Skipping experiment without ID');
                    continue;
                }

                const variants: Variant[] = [];

                if (exp.variants) {
                    for (const variant of exp.variants) {
                        // Skip variants without required fields
                        if (!variant.id) {
                            console.warn(`Skipping variant without ID in experiment ${exp.id}`);
                            continue;
                        }

                        const variantType = variant.variant_type as 'CONTROL' | 'EXPERIMENTAL';
                        if (variantType !== 'CONTROL' && variantType !== 'EXPERIMENTAL') {
                            console.warn(`Invalid variant type: ${variant.variant_type}`);
                            continue;
                        }

                        variants.push({
                            id: variant.id,
                            variant_type: variantType,
                            context_id: variant.context_id,
                            override_id: variant.override_id,
                            overrides: this.normalizeToStringRecord(variant.overrides)
                        });
                    }
                }

                experiments.push({
                    id: exp.id,
                    context: this.normalizeToStringRecord(exp.context),
                    variants: variants,
                    traffic_percentage: exp.traffic_percentage || 100
                });
            }

            return experiments;
        } catch (error) {
            console.error('Error fetching experiments from Superposition:', error);
            return null;
        }
    }

    private async fetchExperimentGroups(): Promise<ExperimentGroup[] | null> {
        try {
            const commandInput: ListExperimentGroupsCommandInput = {
                workspace_id: this.superpositionOptions.workspace_id,
                org_id: this.superpositionOptions.org_id,
                all: true
            };

            const command = new ListExperimentGroupsCommand(commandInput);
            const response = await this.smithyClient.send(command);

            if (!response.data) {
                return null;
            }

            // Convert response to internal format with proper type checking
            const experimentGroups: ExperimentGroup[] = [];

            for (const exp_group of response.data) {
                // Skip experiment groups without required fields
                if (!exp_group.id) {
                    console.warn('Skipping experiment group without ID');
                    continue;
                }

                experimentGroups.push({
                    id: exp_group.id,
                    context: this.normalizeToStringRecord(exp_group.context),
                    traffic_percentage: exp_group.traffic_percentage || 100,
                    member_experiment_ids: exp_group.member_experiment_ids || [],
                    group_type: exp_group.group_type as GroupType || GroupType.USER_CREATED
                });
            }

            return experimentGroups;
        } catch (error) {
            console.error('Error fetching experiment groups from Superposition:', error);
            return null;
        }
    }

    /**
     * Normalize any value to a Record<string, string> format
     * This ensures compatibility with the expected interface
     */
    private normalizeToStringRecord(value: any): Record<string, string> {
        const result: Record<string, string> = {};

        // Handle null or undefined
        if (value == null) {
            return result;
        }

        // If it's already a record/object
        if (typeof value === 'object' && !Array.isArray(value)) {
            for (const [key, val] of Object.entries(value)) {
                if (typeof val === 'string') {
                    result[key] = val;
                } else if (val != null) {
                    // Convert non-string values to JSON strings
                    result[key] = JSON.stringify(val);
                }
            }
            return result;
        }

        // If it's not an object, return empty record
        console.warn(`Expected object for conversion, got ${typeof value}`);
        return result;
    }

    async getExperiments(): Promise<Experiment[]> {
        if (this.options.refreshStrategy && 'ttl' in this.options.refreshStrategy) {
            const strategy = this.options.refreshStrategy as OnDemandStrategy;
            const now = new Date();
            const shouldRefresh = !this.lastUpdated ||
                (now.getTime() - this.lastUpdated.getTime()) > strategy.ttl * 1000;

            if (shouldRefresh) {
                try {
                    console.log('TTL expired. Fetching experiments on-demand.');
                    const experiments = await this.fetchExperiments();
                    if (experiments) {
                        this.cachedExperiments = experiments;
                        this.lastUpdated = new Date();
                    }
                } catch (error) {
                    console.warn('On-demand fetch failed:', error);
                    if (!strategy.use_stale_on_error || !this.cachedExperiments) {
                        throw error;
                    }
                    console.log('Using stale experiments due to error.');
                }
            }
        }

        return this.cachedExperiments || [];
    }

    async getExperimentGroups(): Promise<ExperimentGroup[]> {
        if (this.options.refreshStrategy && 'ttl' in this.options.refreshStrategy) {
            const strategy = this.options.refreshStrategy as OnDemandStrategy;
            const now = new Date();
            const shouldRefresh = !this.lastUpdated ||
                (now.getTime() - this.lastUpdated.getTime()) > strategy.ttl * 1000;

            if (shouldRefresh) {
                try {
                    console.log('TTL expired. Fetching experiment groups on-demand.');
                    const experimentGroups = await this.fetchExperimentGroups();
                    if (experimentGroups) {
                        this.cachedExperimentGroups = experimentGroups;
                        this.lastUpdated = new Date();
                    }
                } catch (error) {
                    console.warn('On-demand fetch failed:', error);
                    if (!strategy.use_stale_on_error || !this.cachedExperimentGroups) {
                        throw error;
                    }
                    console.log('Using stale experiment groups due to error.');
                }
            }
        }

        return this.cachedExperimentGroups || [];
    }

    generateCacheKey(queryData: Record<string, any>): string {
        return JSON.stringify(queryData, Object.keys(queryData).sort());
    }

    getFromEvalCache(key: string): any | undefined {
        return this.evaluationCache.get(key);
    }

    setEvalCache(key: string, value: any): void {
        this.evaluationCache.set(key, value);
    }

    clearEvalCache(): void {
        this.evaluationCache.clear();
    }

    async close(): Promise<void> {
        try {
            if (this.pollingInterval) {
                clearInterval(this.pollingInterval);
                console.log('Polling stopped successfully');
            }

            this.clearEvalCache();
            this.cachedExperiments = null;
            this.lastUpdated = null;

            console.log('ExperimentationClient closed successfully');
        } catch (error) {
            console.error('Error during ExperimentationClient cleanup:', error);
            throw error;
        }
    }
}