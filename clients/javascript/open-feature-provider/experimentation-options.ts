import { SuperpositionClient, ListExperimentCommand, ListExperimentCommandInput, ListExperimentGroupsCommandInput, ListExperimentGroupsCommand, GroupType as SdkGroupType } from '@juspay/superposition-sdk';
import { SuperpositionOptions, ExperimentationOptions, PollingStrategy, OnDemandStrategy } from './types';

export interface Variant {
    id: string;
    variant_type: 'CONTROL' | 'EXPERIMENTAL';
    context_id?: string;
    override_id?: string;
    overrides: Record<string, string>;
}

export type GroupType = 'USER_CREATED' | 'SYSTEM_GENERATED';

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

export class ExperimentationConfig {
    private smithyClient: SuperpositionClient;
    private options: ExperimentationOptions;
    private cachedExperiments: Experiment[] | null = null;
    private cachedExperimentGroups: ExperimentGroup[] | null = null;
    private lastUpdated: Date | null = null;
    private evaluationCache: Map<string, any> = new Map();
    private pollingInterval?: NodeJS.Timeout;
    private enableDetailedErrorLogging: boolean = false;
    private lastErrorMessage: string | null = null;

    constructor(
        private superpositionOptions: SuperpositionOptions,
        experimentOptions: ExperimentationOptions
    ) {
        this.options = experimentOptions;
        this.enableDetailedErrorLogging = experimentOptions.enableDetailedErrorLogging ?? false;
        this.smithyClient = new SuperpositionClient({
            endpoint: superpositionOptions.endpoint,
            token: { token: superpositionOptions.token },
        });
    }

    private categorizeError(error: unknown): string {
        if (!error) return 'Unknown error occurred';
        
        const errorMessage = error instanceof Error ? error.message : String(error);
        
        // Network-related errors
        if (this.isNetworkError(error)) {
            return `Experimentation service unavailable`;
        }
        
        // Authentication errors
        if (errorMessage.includes('401') || errorMessage.includes('403')) {
            return 'Authentication failed for experimentation service';
        }
        
        // Configuration errors
        if (errorMessage.includes('404')) {
            return 'Experiments not found - check workspace and org settings';
        }
        
        return 'Failed to fetch experiments';
    }

    private isNetworkError(error: unknown): boolean {
        if (!error) return false;
        
        const errorMessage = error instanceof Error ? error.message : String(error);
        
        return errorMessage.includes('ECONNREFUSED') ||
               errorMessage.includes('ENOTFOUND') ||
               errorMessage.includes('ECONNRESET') ||
               errorMessage.includes('ETIMEDOUT') ||
               errorMessage.includes('fetch failed') ||
               errorMessage.includes('network') ||
               errorMessage.includes('connection');
    }

    async initialize(): Promise<void> {
        try {
            // Fetch initial experiments
            const experiments = await this.fetchExperiments();
            const experimentgroups = await this.fetchExperimentGroups();
            if (experiments && experimentgroups) {
                this.cachedExperiments = experiments;
                this.cachedExperimentGroups = experimentgroups;
                this.lastUpdated = new Date();
                this.lastErrorMessage = null;
                if (this.enableDetailedErrorLogging) {
                    console.log('Experiments and Experiment Groups fetched successfully.');
                }
            }
        } catch (error) {
            const errorMessage = this.categorizeError(error);
            this.lastErrorMessage = errorMessage;
            
            if (this.enableDetailedErrorLogging) {
                console.error('ExperimentationConfig initialization failed:', error);
            } else if (this.isNetworkError(error)) {
                console.warn(`SuperpositionProvider: ${errorMessage}. Experiments will be unavailable.`);
            }
            
            // Don't throw - allow the provider to work without experiments
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
                    this.lastErrorMessage = null;
                    if (this.enableDetailedErrorLogging) {
                        console.log('Experiments and Experiment Groups refreshed successfully.');
                    }
                }
            } catch (error) {
                const errorMessage = this.categorizeError(error);
                this.lastErrorMessage = errorMessage;
                
                if (this.enableDetailedErrorLogging) {
                    console.error('Polling error:', error);
                } else if (this.isNetworkError(error)) {
                    // Only log polling errors occasionally to avoid spam
                    const now = Date.now();
                    const lastLogTime = parseInt(localStorage?.getItem('lastPollingErrorLog') || '0');
                    if (now - lastLogTime > 300000) { // Log at most once every 5 minutes
                        console.warn(`SuperpositionProvider: ${errorMessage}. Will retry on next poll.`);
                        localStorage?.setItem('lastPollingErrorLog', now.toString());
                    }
                }
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
                    if (this.enableDetailedErrorLogging) {
                        console.warn('Skipping experiment without ID');
                    }
                    continue;
                }

                const variants: Variant[] = [];

                if (exp.variants) {
                    for (const variant of exp.variants) {
                        // Skip variants without required fields
                        if (!variant.id) {
                            if (this.enableDetailedErrorLogging) {
                                console.warn(`Skipping variant without ID in experiment ${exp.id}`);
                            }
                            continue;
                        }

                        const variantType = variant.variant_type as 'CONTROL' | 'EXPERIMENTAL';
                        if (variantType !== 'CONTROL' && variantType !== 'EXPERIMENTAL') {
                            if (this.enableDetailedErrorLogging) {
                                console.warn(`Invalid variant type: ${variant.variant_type}`);
                            }
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
            if (this.enableDetailedErrorLogging) {
                console.error('Error fetching experiments from Superposition:', error);
            }
            throw error; // Let the caller handle the categorization
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
                if (exp_group.id) {
                    experimentGroups.push({
                        id: exp_group.id,
                        context: this.normalizeToStringRecord(exp_group.context),
                        traffic_percentage: exp_group.traffic_percentage || 100,
                        member_experiment_ids: exp_group.member_experiment_ids || [],
                        group_type: (exp_group.group_type as GroupType) || 'USER_CREATED'
                    });
                }
            }

            return experimentGroups;
        } catch (error) {
            if (this.enableDetailedErrorLogging) {
                console.error('Error fetching experiment groups from Superposition:', error);
            }
            throw error; // Let the caller handle the categorization
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
                    if (this.enableDetailedErrorLogging) {
                        console.log('TTL expired. Fetching experiments on-demand.');
                    }
                    const experiments = await this.fetchExperiments();
                    if (experiments) {
                        this.cachedExperiments = experiments;
                        this.lastUpdated = new Date();
                        this.lastErrorMessage = null;
                    }
                } catch (error) {
                    const errorMessage = this.categorizeError(error);
                    this.lastErrorMessage = errorMessage;
                    
                    if (this.enableDetailedErrorLogging) {
                        console.warn('On-demand fetch failed:', error);
                    } else if (this.isNetworkError(error)) {
                        console.warn(`SuperpositionProvider: ${errorMessage}. Using cached experiments.`);
                    }
                    
                    if (!strategy.use_stale_on_error || !this.cachedExperiments) {
                        return []; // Return empty array instead of throwing
                    }
                    
                    if (this.enableDetailedErrorLogging) {
                        console.log('Using stale experiments due to error.');
                    }
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
                    if (this.enableDetailedErrorLogging) {
                        console.log('TTL expired. Fetching experiment groups on-demand.');
                    }
                    const experimentGroups = await this.fetchExperimentGroups();
                    if (experimentGroups) {
                        this.cachedExperimentGroups = experimentGroups;
                        this.lastUpdated = new Date();
                        this.lastErrorMessage = null;
                    }
                } catch (error) {
                    const errorMessage = this.categorizeError(error);
                    this.lastErrorMessage = errorMessage;
                    
                    if (this.enableDetailedErrorLogging) {
                        console.warn('On-demand fetch failed:', error);
                    } else if (this.isNetworkError(error)) {
                        console.warn(`SuperpositionProvider: ${errorMessage}. Using cached experiment groups.`);
                    }
                    
                    if (!strategy.use_stale_on_error || !this.cachedExperimentGroups) {
                        return []; // Return empty array instead of throwing
                    }
                    
                    if (this.enableDetailedErrorLogging) {
                        console.log('Using stale experiment groups due to error.');
                    }
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
                if (this.enableDetailedErrorLogging) {
                    console.log('Polling stopped successfully');
                }
            }

            this.clearEvalCache();
            this.cachedExperiments = null;
            this.cachedExperimentGroups = null;
            this.lastUpdated = null;
            this.lastErrorMessage = null;

            if (this.enableDetailedErrorLogging) {
                console.log('ExperimentationConfig closed successfully');
            }
        } catch (error) {
            if (this.enableDetailedErrorLogging) {
                console.error('Error during ExperimentationConfig cleanup:', error);
            }
            // Don't throw during cleanup
        }
    }

    getLastError(): string | null {
        return this.lastErrorMessage;
    }
}