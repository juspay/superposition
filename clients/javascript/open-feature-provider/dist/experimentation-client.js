"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ExperimentationClient = void 0;
const superposition_sdk_1 = require("superposition-sdk");
class ExperimentationClient {
    constructor(superpositionOptions, experimentOptions) {
        this.superpositionOptions = superpositionOptions;
        this.cachedExperiments = null;
        this.lastUpdated = null;
        this.evaluationCache = new Map();
        this.options = experimentOptions;
        this.smithyClient = new superposition_sdk_1.SuperpositionClient({
            endpoint: superpositionOptions.endpoint,
            token: { token: superpositionOptions.token },
        });
    }
    async initialize() {
        // Fetch initial experiments
        const experiments = await this.fetchExperiments();
        if (experiments) {
            this.cachedExperiments = experiments;
            this.lastUpdated = new Date();
            console.log('Experiments fetched successfully.');
        }
        // Set up refresh strategy
        if (this.options.refreshStrategy && 'interval' in this.options.refreshStrategy) {
            const strategy = this.options.refreshStrategy;
            this.startPolling(strategy.interval);
        }
    }
    startPolling(interval) {
        this.pollingInterval = setInterval(async () => {
            try {
                const experiments = await this.fetchExperiments();
                if (experiments) {
                    this.cachedExperiments = experiments;
                    this.lastUpdated = new Date();
                    console.log('Experiments refreshed successfully.');
                }
            }
            catch (error) {
                console.error('Polling error:', error);
            }
        }, interval);
    }
    async fetchExperiments() {
        try {
            const commandInput = {
                workspace_id: this.superpositionOptions.workspace_id,
                org_id: this.superpositionOptions.org_id,
                all: true
            };
            const command = new superposition_sdk_1.ListExperimentCommand(commandInput);
            const response = await this.smithyClient.send(command);
            if (!response.data) {
                return null;
            }
            // Convert response to internal format with proper type checking
            const experiments = [];
            for (const exp of response.data) {
                // Skip experiments without required fields
                if (!exp.id) {
                    console.warn('Skipping experiment without ID');
                    continue;
                }
                const variants = [];
                if (exp.variants) {
                    for (const variant of exp.variants) {
                        // Skip variants without required fields
                        if (!variant.id) {
                            console.warn(`Skipping variant without ID in experiment ${exp.id}`);
                            continue;
                        }
                        const variantType = variant.variant_type;
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
        }
        catch (error) {
            console.error('Error fetching experiments from Superposition:', error);
            return null;
        }
    }
    /**
     * Normalize any value to a Record<string, string> format
     * This ensures compatibility with the expected interface
     */
    normalizeToStringRecord(value) {
        const result = {};
        // Handle null or undefined
        if (value == null) {
            return result;
        }
        // If it's already a record/object
        if (typeof value === 'object' && !Array.isArray(value)) {
            for (const [key, val] of Object.entries(value)) {
                if (typeof val === 'string') {
                    result[key] = val;
                }
                else if (val != null) {
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
    async getExperiments() {
        if (this.options.refreshStrategy && 'ttl' in this.options.refreshStrategy) {
            const strategy = this.options.refreshStrategy;
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
                }
                catch (error) {
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
    generateCacheKey(queryData) {
        return JSON.stringify(queryData, Object.keys(queryData).sort());
    }
    getFromEvalCache(key) {
        return this.evaluationCache.get(key);
    }
    setEvalCache(key, value) {
        this.evaluationCache.set(key, value);
    }
    clearEvalCache() {
        this.evaluationCache.clear();
    }
    async close() {
        try {
            if (this.pollingInterval) {
                clearInterval(this.pollingInterval);
                console.log('Polling stopped successfully');
            }
            this.clearEvalCache();
            this.cachedExperiments = null;
            this.lastUpdated = null;
            console.log('ExperimentationClient closed successfully');
        }
        catch (error) {
            console.error('Error during ExperimentationClient cleanup:', error);
            throw error;
        }
    }
}
exports.ExperimentationClient = ExperimentationClient;
