import {
    SuperpositionOptions,
    ConfigOptions,
    ConfigData,
    PollingStrategy,
    ExperimentationArgs,
    MergeStrategy,
} from "./types";
import { NativeResolver } from "superposition-bindings";
import {
    SuperpositionClient,
    GetConfigCommand,
    GetConfigCommandInput,
} from "superposition-sdk";
import { ExperimentationClient } from "./experimentation-client";
import { ExperimentationOptions } from "./types";

const _cacheRegistry = new FinalizationRegistry((freeFn: () => void) => {
    setImmediate(freeFn);
});

export class ConfigurationClient {
    private config: SuperpositionOptions;
    private resolver: NativeResolver;
    private options: ConfigOptions;
    /** Learned from the config response; defaults to merge until one arrives. */
    private mergeStrategy: MergeStrategy = "merge";
    private currentConfigData: ConfigData | null = null;
    private experimentationClient?: ExperimentationClient;
    private experimentationOptions?: ExperimentationOptions;
    private providerCache: ReturnType<
        NativeResolver["createProviderCache"]
    > | null = null;
    private refreshInterval: ReturnType<typeof setTimeout> | null = null;

    private defaults: ConfigData | null = null;
    private smithyClient: SuperpositionClient;

    constructor(
        config: SuperpositionOptions,
        resolver: NativeResolver,
        options: ConfigOptions = {},
        experimentationOptions?: ExperimentationOptions,
    ) {
        this.config = config;
        this.resolver = resolver;
        this.options = options;
        this.providerCache = resolver.createProviderCache();
        const cache = this.providerCache;
        _cacheRegistry.register(
            this,
            () => {
                cache.free();
            },
            this,
        );

        if (
            this.options.refreshStrategy &&
            "interval" in this.options.refreshStrategy
        ) {
            const strategy = this.options.refreshStrategy as PollingStrategy;

            const weakSelf = new WeakRef(this);
            const poll = async () => {
                const self = weakSelf.deref();
                if (!self) return;
                try {
                    const refreshedConfig = await self.fetchConfigData();
                    self.currentConfigData = refreshedConfig;
                    self.providerCache?.initConfig(
                        refreshedConfig.default_configs,
                        refreshedConfig.contexts,
                        refreshedConfig.overrides,
                        refreshedConfig.dimensions,
                    );
                    console.log("Configuration refreshed successfully.");
                } catch (error) {
                    console.error(
                        "Failed to refresh configuration. Will continue to use the last known good configuration.",
                        error,
                    );
                }
                self.refreshInterval = setTimeout(poll, strategy.interval);
            };
            this.refreshInterval = setTimeout(poll, strategy.interval);
        }

        if (experimentationOptions) {
            this.experimentationOptions = experimentationOptions;
            this.experimentationClient = new ExperimentationClient(
                config,
                experimentationOptions,
                this.reinitExperimentsCache.bind(this),
            );
        }

        this.smithyClient = new SuperpositionClient({
            endpoint: this.config.endpoint,
            token: { token: this.config.token },
            requestHandler: this.config.httpClient,
        });
    }

    async initialize(): Promise<void> {
        const configData = await this.fetchConfigData();
        this.providerCache!.initConfig(
            configData.default_configs,
            configData.contexts,
            configData.overrides,
            configData.dimensions,
        );

        if (this.experimentationClient) {
            await this.experimentationClient.initialize();
            this.reinitExperimentsCache();
        }
    }

    private reinitExperimentsCache(): void {
        if (!this.experimentationClient || !this.providerCache) return;
        const experiments = this.experimentationClient.getCachedExperiments();
        const experimentGroups =
            this.experimentationClient.getCachedExperimentGroups();
        if (experiments && experimentGroups) {
            this.providerCache.initExperiments(experiments, experimentGroups);
        }
    }

    async eval(
        queryData: Record<string, any>,
        filterPrefixes?: string[],
        filterExcludePrefixes?: string[],
        targetingKey?: string,
    ): Promise<any>;
    async eval<T>(
        queryData: Record<string, any>,
        filterPrefixes?: string[],
        filterExcludePrefixes?: string[],
        targetingKey?: string,
    ): Promise<T>;
    async eval(
        queryData: Record<string, any>,
        filterPrefixes?: string[],
        filterExcludePrefixes?: string[],
        targetingKey?: string,
    ): Promise<any> {
        try {
            if (!this.currentConfigData) {
                const configData = await this.fetchConfigData();
                this.providerCache!.initConfig(
                    configData.default_configs,
                    configData.contexts,
                    configData.overrides,
                    configData.dimensions,
                );
            }

            if (
                this.experimentationClient &&
                targetingKey &&
                !this.experimentationClient.getCachedExperiments()
            ) {
                await this.experimentationClient.getExperiments();
                await this.experimentationClient.getExperimentGroups();
                this.reinitExperimentsCache();
            }

            return this.providerCache!.evalConfig(
                queryData,
                this.mergeStrategy,
                filterPrefixes,
                filterExcludePrefixes,
                targetingKey,
            );
        } catch (error) {
            if (this.defaults) {
                console.log("Falling back to defaults");
                return this.resolver.resolveConfig(
                    this.defaults.default_configs || {},
                    this.defaults.contexts || [],
                    this.defaults.overrides || {},
                    this.defaults.dimensions || {},
                    queryData,
                    this.mergeStrategy,
                    filterPrefixes,
                    filterExcludePrefixes,
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
            // The workspace's strategy rides on this response, so no extra call.
            if (response.merge_strategy) {
                this.mergeStrategy =
                    String(response.merge_strategy).toLowerCase() === "replace"
                        ? "replace"
                        : "merge";
            }
            this.currentConfigData = {
                default_configs: response.default_configs || {},
                contexts: response.contexts || [],
                overrides: response.overrides || {},
                dimensions: response.dimensions || {},
            };

            return this.currentConfigData;
        } catch (error) {
            console.error(
                "SuperpositionClient GetConfigCommand failed:",
                error,
            );
            const errorMessage =
                error instanceof Error ? error.message : String(error);
            throw new Error(`Failed to fetch configuration: ${errorMessage}`);
        }
    }
    // TODO: defaultValue is taken but not used. Should it be used as a fallback?
    // TODO: Remove this function all together and use eval for getAllConfigValue as well
    async getAllConfigValue(
        defaultValue: Record<string, any>,
        context: Record<string, any>,
        targetingKey?: string,
    ): Promise<Record<string, any>> {
        try {
            if (!this.currentConfigData) {
                const configData = await this.fetchConfigData();
                this.providerCache!.initConfig(
                    configData.default_configs,
                    configData.contexts,
                    configData.overrides,
                    configData.dimensions,
                );
            }

            if (
                this.experimentationClient &&
                targetingKey &&
                !this.experimentationClient.getCachedExperiments()
            ) {
                await this.experimentationClient.getExperiments();
                await this.experimentationClient.getExperimentGroups();
                this.reinitExperimentsCache();
            }

            return this.providerCache!.evalConfig(
                context,
                this.mergeStrategy,
                undefined,
                undefined,
                targetingKey,
            );
        } catch (error) {
            if (this.defaults) {
                return this.resolver.resolveConfig(
                    this.defaults.default_configs || {},
                    this.defaults.contexts || [],
                    this.defaults.overrides || {},
                    this.defaults.dimensions || {},
                    context,
                    this.mergeStrategy,
                );
            }
            throw error;
        }
    }

    // Add method to close and cleanup
    async close(): Promise<void> {
        _cacheRegistry.unregister(this);

        if (this.refreshInterval) {
            clearTimeout(this.refreshInterval);
            this.refreshInterval = null;
        }

        if (this.experimentationClient) {
            await this.experimentationClient.close();
        }
        if (this.providerCache) {
            this.providerCache.free();
            this.providerCache = null;
        }
    }
}
