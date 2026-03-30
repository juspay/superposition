import {
    SuperpositionOptions,
    ConfigOptions,
    ConfigData,
    PollingStrategy,
    ExperimentationArgs,
} from "./types";
import { NativeResolver } from "superposition-bindings";
import {
    SuperpositionClient,
    GetConfigCommand,
    GetConfigCommandInput,
} from "superposition-sdk";
import {
    ExperimentationClient,
    Experiment,
    ExperimentGroup,
} from "./experimentation-client";
import { ExperimentationOptions } from "./types";

// Auto-free Rust ProviderCache when ConfigurationClient is GC'd.
// setImmediate defers the koffi call to the event loop — safe to call koffi there,
// unlike inside the GC callback itself where V8 internals are mid-cleanup.
const _cacheRegistry = new FinalizationRegistry((freeFn: () => void) => {
    setImmediate(freeFn);
});

export class ConfigurationClient {
    private config: SuperpositionOptions;
    private resolver: NativeResolver;
    private options: ConfigOptions;
    private currentConfigData: ConfigData | null = null;
    private experimentationClient?: ExperimentationClient;
    private experimentationOptions?: ExperimentationOptions;
    private providerCache: ReturnType<NativeResolver["createProviderCache"]> | null = null;
    private refreshInterval: ReturnType<typeof setInterval> | null = null;

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
        this.providerCache = resolver.createProviderCache();

        // Auto-free Rust ProviderCache when this instance is GC'd.
        // The registry holds 'cache' strongly so it stays valid for the deferred call.
        // close() unregisters this token to prevent double-free.
        const cache = this.providerCache;
        _cacheRegistry.register(this, () => { cache.free(); }, this);


        if (
            this.options.refreshStrategy &&
            "interval" in this.options.refreshStrategy
        ) {
            const strategy = this.options.refreshStrategy as PollingStrategy;

            // Use WeakRef so the interval closure does not hold a strong reference
            // to this instance. When the instance is GC'd, deref() returns undefined
            // and the interval clears itself.
            const weakSelf = new WeakRef(this);
            const intervalId = setInterval(async () => {
                const self = weakSelf.deref();
                if (!self) {
                    clearInterval(intervalId);
                    return;
                }
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
                        error
                    );
                }
            }, strategy.interval);
            this.refreshInterval = intervalId;
        }

        if (experimentationOptions) {
            this.experimentationOptions = experimentationOptions;
            this.experimentationClient = new ExperimentationClient(
                config,
                experimentationOptions,
                () => this.reinitExperimentsCache()
            );
        }

        this.smithyClient = new SuperpositionClient({
            endpoint: this.config.endpoint,
            token: { token: this.config.token },
            requestHandler: this.config.httpClient,
        });
    }

    async initialize(): Promise<void> {
        // Fetch config once and seed the cache
        const configData = await this.fetchConfigData();
        this.providerCache!.initConfig(
            configData.default_configs,
            configData.contexts,
            configData.overrides,
            configData.dimensions,
        );

        if (this.experimentationClient) {
            await this.experimentationClient.initialize();
            // Seed experiments into Rust cache after initial fetch
            this.reinitExperimentsCache();
        }
    }

    private reinitExperimentsCache(): void {
        if (!this.experimentationClient || !this.providerCache) return;
        const experiments = this.experimentationClient.getCachedExperiments();
        const experimentGroups = this.experimentationClient.getCachedExperimentGroups();
        if (experiments && experimentGroups) {
            this.providerCache.initExperiments(experiments, experimentGroups);
        }
    }

    async eval(
        queryData: Record<string, any>,
        filterPrefixes?: string[],
        targetingKey?: string
    ): Promise<any>;
    async eval<T>(
        queryData: Record<string, any>,
        filterPrefixes?: string[],
        targetingKey?: string
    ): Promise<T>;
    async eval(
        queryData: Record<string, any>,
        filterPrefixes?: string[],
        targetingKey?: string
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

            // For on-demand: getExperiments/getExperimentGroups handle TTL and call
            // reinitExperimentsCache via onExperimentsChange when fresh data arrives.
            // For polling: reinitExperimentsCache is called by the interval callback.
            // Here we only seed on the first call if experiments aren't in the cache yet.
            if (this.experimentationClient && targetingKey &&
                !this.experimentationClient.getCachedExperiments()) {
                await this.experimentationClient.getExperiments();
                await this.experimentationClient.getExperimentGroups();
                this.reinitExperimentsCache();
            }

            return this.providerCache!.evalConfig(queryData, "merge", filterPrefixes, targetingKey);
        } catch (error) {
            if (this.defaults) {
                console.log("Falling back to defaults");
                return this.resolver.resolveConfig(
                    this.defaults.default_configs || {},
                    this.defaults.contexts || [],
                    this.defaults.overrides || {},
                    this.defaults.dimensions || {},
                    queryData,
                    "merge",
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
                overrides: response.overrides || {},
                dimensions: response.dimensions || {},
            };

            return this.currentConfigData;
        } catch (error) {
            console.error(
                "SuperpositionClient GetConfigCommand failed:",
                error
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
        targetingKey?: string
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

            if (this.experimentationClient && targetingKey &&
                !this.experimentationClient.getCachedExperiments()) {
                await this.experimentationClient.getExperiments();
                await this.experimentationClient.getExperimentGroups();
                this.reinitExperimentsCache();
            }

            return this.providerCache!.evalConfig(context, "merge", undefined, targetingKey);
        } catch (error) {
            if (this.defaults) {
                return this.resolver.resolveConfig(
                    this.defaults.default_configs || {},
                    this.defaults.contexts || [],
                    this.defaults.overrides || {},
                    this.defaults.dimensions || {},
                    context,
                    "merge"
                );
            }
            throw error;
        }
    }

    // Add method to get applicable variants
    private async getApplicableVariants(
        experiments: Experiment[],
        experiment_groups: ExperimentGroup[],
        dimensions: Record<string, Record<string, any>>,
        queryData: Record<string, any>,
        identifier: string,
        filterPrefixes?: string[]
    ): Promise<string[]> {
        // This would use the native resolver's getApplicableVariants method
        return this.resolver.getApplicableVariants(
            experiments,
            experiment_groups,
            dimensions,
            queryData,
            identifier,
            filterPrefixes || []
        );
    }

    // Add method to close and cleanup
    async close(): Promise<void> {
        // Prevent the FinalizationRegistry from also calling free after explicit close
        _cacheRegistry.unregister(this);

        if (this.refreshInterval) {
            clearInterval(this.refreshInterval);
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
