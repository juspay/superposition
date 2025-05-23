import { SuperpositionOptions, ConfigOptions, ConfigData, PollingStrategy } from './types'; // ExperimentationOptions seems unused
import { NativeResolver } from './native-resolver';
import { SuperpositionClient, GetConfigCommand, GetConfigCommandInput } from '@io.juspay/superposition-sdk';

export class ConfigurationClient {
    private config: SuperpositionOptions;
    private resolver: NativeResolver;
    private options: ConfigOptions;
    private currentConfigData: ConfigData | null = null;

    private defaults: ConfigData | null = null;
    private smithyClient: SuperpositionClient;

    constructor(
        config: SuperpositionOptions,
        resolver: NativeResolver,
        options: ConfigOptions = {}
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

        }

        this.smithyClient = new SuperpositionClient({
            endpoint: this.config.endpoint,
            token: { token: this.config.token },
        });
    }

    async eval(queryData: Record<string, any>, filterPrefixes?: string[]): Promise<any>;
    async eval<T>(queryData: Record<string, any>, filterPrefixes?: string[]): Promise<T>;
    async eval(queryData: Record<string, any>, filterPrefixes?: string[]): Promise<any> {
        try {

            const configData = await this.fetchConfigData();
            const result = this.resolver.resolveConfig(
                configData.default_configs || {},
                configData.contexts || [],
                configData.overrides || {},
                queryData,
                'merge',
                filterPrefixes
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
}