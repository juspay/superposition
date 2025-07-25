import { SuperpositionOptions, ConfigOptions, ConfigData, PollingStrategy, ExperimentationArgs } from './types';
import { NativeResolver } from '@juspay/superposition-bindings';
import { SuperpositionClient, GetConfigCommand, GetConfigCommandInput } from '@juspay/superposition-sdk';
import { ExperimentationClient, Experiment } from './experimentation-client';
import { ExperimentationOptions } from './types';

export class ConfigurationClient {
    private config: SuperpositionOptions;
    private resolver: NativeResolver;
    private options: ConfigOptions;
    private currentConfigData: ConfigData | null = null;
    private experimentationClient?: ExperimentationClient;
    private experimentationOptions?: ExperimentationOptions;

    private defaults: ConfigData | null = null;
    private smithyClient: SuperpositionClient;

    constructor(
        config: SuperpositionOptions,
        resolver: NativeResolver,
        options: ConfigOptions = {},
        experimentationOptions?: ExperimentationOptions
    ) {
        this.config = config;
        this.resolver = resolver;
        this.options = options;

        if (this.options.refreshStrategy && 'interval' in this.options.refreshStrategy) {
            const strategy = this.options.refreshStrategy as PollingStrategy;

            setInterval(async () => {
                try {
                    const refreshedConfig = await this.fetchConfigData();
                    this.currentConfigData = refreshedConfig;
                    console.log("Configuration refreshed successfully.");
                } catch (error) {
                    console.error("Failed to refresh configuration. Will continue to use the last known good configuration.", error);
                }

            }, strategy.interval);

            if (experimentationOptions) {
                this.experimentationOptions = experimentationOptions;
                this.experimentationClient = new ExperimentationClient(config, experimentationOptions);
            }

        }

        this.smithyClient = new SuperpositionClient({
            endpoint: this.config.endpoint,
            token: { token: this.config.token },
        });
    }

    async initialize(): Promise<void> {
        // Initialize experimentation client if present
        if (this.experimentationClient) {
            await this.experimentationClient.initialize();
        }
    }

    async eval(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<any>;
    async eval<T>(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<T>;
    async eval(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<any> {
        try {

            const configData = await this.fetchConfigData();

            let experimentationArgs: ExperimentationArgs | undefined;

            if (this.experimentationClient && targetingKey) {
                const experiments = await this.experimentationClient.getExperiments();
                experimentationArgs = {
                    experiments,
                    targeting_key: targetingKey
                };
            }

            const result = this.resolver.resolveConfig(
                configData.default_configs || {},
                configData.contexts || [],
                configData.overrides || {},
                queryData,
                'merge',
                filterPrefixes,
                experimentationArgs
            );
            return result;

        } catch (error) {
            if (this.defaults) {
                console.log('Falling back to defaults');
                return this.resolver.resolveConfig(
                    this.defaults.default_configs || {},
                    this.defaults.contexts || [],
                    this.defaults.overrides || {},
                    queryData,
                    'merge',
                    filterPrefixes
                );
            }
            throw error;
        }
    }

    setDefault(defaults: ConfigData): void {
        this.defaults = defaults;
    }

    private async fetchConfigData(): Promise<ConfigData> {
        const commandInput: GetConfigCommandInput = {
            workspace_id: this.config.workspace_id,
            org_id: this.config.org_id,
            context: {},
        };
        const command = new GetConfigCommand(commandInput);

        try {

            const response = await this.smithyClient.send(command);
            this.currentConfigData = {
                default_configs: response.default_configs || {},
                contexts: response.contexts || [],
                overrides: response.overrides || {}
            };

            return this.currentConfigData;

        } catch (error) {
            console.error('SuperpositionClient GetConfigCommand failed:', error);
            const errorMessage = error instanceof Error ? error.message : String(error);
            throw new Error(`Failed to fetch configuration: ${errorMessage}`);
        }
    }

    async getAllConfigValue(
        defaultValue: Record<string, any>,
        context: Record<string, any>,
        targetingKey?: string
    ): Promise<Record<string, any>> {
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

            const result = this.resolver.resolveConfig(
                configData.default_configs || {},
                configData.contexts || [],
                configData.overrides || {},
                queryData,
                'merge'
            );

            return result;
        } catch (error) {
            if (this.defaults) {
                return this.resolver.resolveConfig(
                    this.defaults.default_configs || {},
                    this.defaults.contexts || [],
                    this.defaults.overrides || {},
                    context,
                    'merge'
                );
            }
            throw error;
        }
    }

    // Add method to get applicable variants
    private async getApplicableVariants(
        experiments: Experiment[],
        queryData: Record<string, any>,
        toss: number,
        filterPrefixes?: string[]
    ): Promise<string[]> {
        // This would use the native resolver's getApplicableVariants method
        return this.resolver.getApplicableVariants(
            experiments,
            queryData,
            toss,
            filterPrefixes || []
        );
    }

    // Add method to close and cleanup
    async close(): Promise<void> {
        if (this.experimentationClient) {
            await this.experimentationClient.close();
        }
    }
}