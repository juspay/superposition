"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ConfigurationClient = void 0;
const superposition_sdk_1 = require("superposition-sdk");
const experimentation_client_1 = require("./experimentation-client");
class ConfigurationClient {
    constructor(config, resolver, options = {}, experimentationOptions) {
        this.currentConfigData = null;
        this.defaults = null;
        this.config = config;
        this.resolver = resolver;
        this.options = options;
        if (this.options.refreshStrategy && 'interval' in this.options.refreshStrategy) {
            const strategy = this.options.refreshStrategy;
            setInterval(async () => {
                try {
                    const refreshedConfig = await this.fetchConfigData();
                    this.currentConfigData = refreshedConfig;
                    console.log("Configuration refreshed successfully.");
                }
                catch (error) {
                    console.error("Failed to refresh configuration. Will continue to use the last known good configuration.", error);
                }
            }, strategy.interval);
            if (experimentationOptions) {
                this.experimentationOptions = experimentationOptions;
                this.experimentationClient = new experimentation_client_1.ExperimentationClient(config, experimentationOptions);
            }
        }
        this.smithyClient = new superposition_sdk_1.SuperpositionClient({
            endpoint: this.config.endpoint,
            token: { token: this.config.token },
        });
    }
    async initialize() {
        // Initialize experimentation client if present
        if (this.experimentationClient) {
            await this.experimentationClient.initialize();
        }
    }
    async eval(queryData, filterPrefixes, targetingKey) {
        try {
            const configData = await this.fetchConfigData();
            let experimentationArgs;
            if (this.experimentationClient && targetingKey) {
                const experiments = await this.experimentationClient.getExperiments();
                experimentationArgs = {
                    experiments,
                    targeting_key: targetingKey
                };
            }
            const result = this.resolver.resolveConfig(configData.default_configs || {}, configData.contexts || [], configData.overrides || {}, queryData, 'merge', filterPrefixes, experimentationArgs);
            return result;
        }
        catch (error) {
            if (this.defaults) {
                console.log('Falling back to defaults');
                return this.resolver.resolveConfig(this.defaults.default_configs || {}, this.defaults.contexts || [], this.defaults.overrides || {}, queryData, 'merge', filterPrefixes);
            }
            throw error;
        }
    }
    setDefault(defaults) {
        this.defaults = defaults;
    }
    async fetchConfigData() {
        const commandInput = {
            workspace_id: this.config.workspace_id,
            org_id: this.config.org_id,
            context: {},
        };
        const command = new superposition_sdk_1.GetConfigCommand(commandInput);
        try {
            const response = await this.smithyClient.send(command);
            this.currentConfigData = {
                default_configs: response.default_configs || {},
                contexts: response.contexts || [],
                overrides: response.overrides || {}
            };
            return this.currentConfigData;
        }
        catch (error) {
            console.error('SuperpositionClient GetConfigCommand failed:', error);
            const errorMessage = error instanceof Error ? error.message : String(error);
            throw new Error(`Failed to fetch configuration: ${errorMessage}`);
        }
    }
    async getAllConfigValue(defaultValue, context, targetingKey) {
        try {
            const configData = await this.fetchConfigData();
            // Prepare query data with experiment variants if applicable
            let queryData = { ...context };
            if (this.experimentationClient && targetingKey) {
                const experiments = await this.experimentationClient.getExperiments();
                const toss = this.experimentationOptions?.defaultToss ||
                    (parseInt(targetingKey) || Math.floor(Math.random() * 100)) % 100;
                const variantIds = await this.getApplicableVariants(experiments, queryData, toss);
                if (variantIds.length > 0) {
                    queryData.variantIds = variantIds;
                }
            }
            const result = this.resolver.resolveConfig(configData.default_configs || {}, configData.contexts || [], configData.overrides || {}, queryData, 'merge');
            return result;
        }
        catch (error) {
            if (this.defaults) {
                return this.resolver.resolveConfig(this.defaults.default_configs || {}, this.defaults.contexts || [], this.defaults.overrides || {}, context, 'merge');
            }
            throw error;
        }
    }
    // Add method to get applicable variants
    async getApplicableVariants(experiments, queryData, toss, filterPrefixes) {
        // This would use the native resolver's getApplicableVariants method
        return this.resolver.getApplicableVariants(experiments, queryData, toss, filterPrefixes || []);
    }
    // Add method to close and cleanup
    async close() {
        if (this.experimentationClient) {
            await this.experimentationClient.close();
        }
    }
}
exports.ConfigurationClient = ConfigurationClient;
