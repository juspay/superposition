import { SuperpositionOptions, ConfigOptions, ConfigData, PollingStrategy, ExperimentationArgs } from './types';
import { NativeResolver } from 'superposition-bindings';
import { SuperpositionClient, GetConfigCommand, GetConfigCommandInput } from 'superposition-sdk';
import { ExperimentationConfig, Experiment } from './experimentation-options';
import { ExperimentationOptions } from './types';
import { isNetworkError, categorizeError } from './utils';

export class CacConfig {
    private config: SuperpositionOptions;
    private resolver: NativeResolver;
    private options: ConfigOptions;
    private currentConfigData: ConfigData | null = null;
    private experimentationConfig?: ExperimentationConfig;
    private experimentationOptions?: ExperimentationOptions;
    private lastErrorMessage: string | null = null;

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
                    this.lastErrorMessage = null; // Clear error on successful refresh
                } catch (error) {
                    const errorMessage = categorizeError(error, this.config.endpoint);
                    this.lastErrorMessage = errorMessage;
                    console.warn(`SuperpositionProvider: ${errorMessage}. Will continue with cached configuration.`);
                }
            }, strategy.interval);

            if (experimentationOptions) {
                this.experimentationOptions = experimentationOptions;
                this.experimentationConfig = new ExperimentationConfig(config, experimentationOptions);
            }
        }

        this.smithyClient = new SuperpositionClient({
            endpoint: this.config.endpoint,
            token: { token: this.config.token },
        });
    }

    async initialize(): Promise<void> {
        // Initialize experimentation client if present
        if (this.experimentationConfig) {
            await this.experimentationConfig.initialize();
        }
    }

    async eval(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<any>;
    async eval<T>(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<T>;
    async eval(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<any> {
        try {
            const configData = await this.fetchConfigData();

            let experimentationArgs: ExperimentationArgs | undefined;

            if (this.experimentationConfig && targetingKey) {
                const experiments = await this.experimentationConfig.getExperiments();
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
                if (isNetworkError(error)) {
                    console.error('SuperpositionProvider: Server connection failed, using fallback configuration');
                }
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
            const errorMessage = categorizeError(error, this.config.endpoint);
            this.lastErrorMessage = errorMessage;
            throw new Error(errorMessage);
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

            if (this.experimentationConfig && targetingKey) {
                const experiments = await this.experimentationConfig.getExperiments();
                const identifier = this.experimentationOptions?.defaultIdentifier ||
                    (targetingKey ? targetingKey : 'default');

                const variantIds = await this.getApplicableVariants(experiments, queryData, identifier);
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
                if (isNetworkError(error)) {
                    console.warn('SuperpositionProvider: Using fallback configuration for getAllConfigValue');
                }
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
    async getApplicableVariants(
        experiments: Experiment[],
        queryData: Record<string, any>,
        identifier: string,
        filterPrefixes?: string[]
    ): Promise<string[]> {
        // This would use the native resolver's getApplicableVariants method
        return this.resolver.getApplicableVariants(
            experiments,
            queryData,
            identifier,
            filterPrefixes || []
        );
    }

    // Add method to close and cleanup
    async close(): Promise<void> {
        if (this.experimentationConfig) {
            await this.experimentationConfig.close();
        }
    }

    getLastError(): string | null {
        return this.lastErrorMessage;
    }
}