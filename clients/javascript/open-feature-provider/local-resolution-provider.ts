import {
    EvaluationContext,
    Provider,
    JsonValue,
    ResolutionDetails,
    ProviderStatus,
    ProviderEvents,
    OpenFeatureEventEmitter,
    ErrorCode,
    ProviderMetadata,
    StandardResolutionReasons,
} from "@openfeature/server-sdk";
import {
    SuperpositionDataSource,
    ConfigData,
    ExperimentData,
    AllFeatureProvider,
    FeatureExperimentMeta,
    RefreshStrategy,
    CacheOptions,
    OnDemandStrategy,
    PollingStrategy,
} from "./types";
import { NativeResolver } from "superposition-bindings";
import {
    getNestedValue,
    convertToBoolean,
    convertToString,
    convertToNumber,
    convertToObject,
} from "./utils";

type ConverterFunction<T> = (value: any, defaultValue: T) => T;

const TYPE_CONVERTERS: Record<string, ConverterFunction<any>> = {
    boolean: convertToBoolean as ConverterFunction<boolean>,
    string: convertToString as ConverterFunction<string>,
    number: convertToNumber as ConverterFunction<number>,
    object: convertToObject as ConverterFunction<any>,
};

type ValueType = keyof typeof TYPE_CONVERTERS;

interface EvaluationResult<T> {
    value: T;
    reason: string;
    errorCode?: ErrorCode;
    errorMessage?: string;
}

export class LocalResolutionProvider
    implements Provider, AllFeatureProvider, FeatureExperimentMeta
{
    readonly metadata: ProviderMetadata = {
        name: "LocalResolutionProvider",
    };

    events = new OpenFeatureEventEmitter();
    status: ProviderStatus = ProviderStatus.NOT_READY;

    private cachedConfig: ConfigData | null = null;
    private cachedExperiments: ExperimentData | null = null;
    private pollingInterval: NodeJS.Timeout | null = null;
    private lastFetchTime: Date | null = null;
    private resolutionCache: Map<string, any> = new Map();

    constructor(
        private primary: SuperpositionDataSource,
        private fallback: SuperpositionDataSource | undefined,
        private refreshStrategy: RefreshStrategy,
        private cacheOptions?: CacheOptions,
        private nativeResolver: NativeResolver = new NativeResolver()
    ) {}

    /**
     * Initialize the provider and fetch initial configuration
     */
    async initialize(context?: EvaluationContext): Promise<void> {
        this.status = ProviderStatus.NOT_READY;

        try {
            await this.doRefresh();

            // Start polling if configured
            if ("interval" in this.refreshStrategy) {
                const strategy = this.refreshStrategy as PollingStrategy;
                this.startPolling(strategy.interval);
            }

            this.status = ProviderStatus.READY;
            this.events.emit(ProviderEvents.Ready, {
                message: "Provider ready",
            });
        } catch (error) {
            this.status = ProviderStatus.ERROR;
            const message =
                error instanceof Error
                    ? error.message
                    : "Initialization failed";
            this.events.emit(ProviderEvents.Error, {
                message,
                errorCode: ErrorCode.PROVIDER_NOT_READY,
            });
            throw error;
        }
    }

    /**
     * Close the provider and cleanup resources
     */
    async onClose(): Promise<void> {
        this.stopPolling();

        await Promise.all([
            this.primary.close(),
            this.fallback?.close(),
        ]);

        this.cachedConfig = null;
        this.cachedExperiments = null;
        this.resolutionCache.clear();
        this.lastFetchTime = null;
        this.status = ProviderStatus.NOT_READY;
    }

    /**
     * Manually trigger a refresh of configuration data
     */
    async refresh(): Promise<void> {
        await this.doRefresh();
    }

    /**
     * Resolve all features for the given context
     */
    async resolveAllFeatures(
        context: EvaluationContext
    ): Promise<Record<string, any>> {
        return this.evalWithContext(context);
    }

    /**
     * Resolve all features matching prefix filters
     */
    async resolveAllFeaturesWithFilter(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): Promise<Record<string, any>> {
        return this.evalWithContext(context, prefixFilter);
    }

    /**
     * Get applicable variant IDs for the given context
     */
    async getApplicableVariants(context: EvaluationContext): Promise<string[]> {
        if (!this.cachedExperiments) {
            return [];
        }

        const extractedContext = this.extractContext(context);
        const targetingKey = context.targetingKey || "default";

        return this.nativeResolver.getApplicableVariants(
            this.cachedExperiments.experiments,
            this.cachedExperiments.experiment_groups,
            this.cachedConfig?.dimensions || {},
            extractedContext,
            targetingKey,
            []
        );
    }

    /**
     * Resolve boolean flag evaluation
     */
    async resolveBooleanEvaluation(
        flagKey: string,
        defaultValue: boolean,
        context: EvaluationContext
    ): Promise<ResolutionDetails<boolean>> {
        return this.resolveValue(flagKey, defaultValue, context, "boolean");
    }

    /**
     * Resolve string flag evaluation
     */
    async resolveStringEvaluation(
        flagKey: string,
        defaultValue: string,
        context: EvaluationContext
    ): Promise<ResolutionDetails<string>> {
        return this.resolveValue(flagKey, defaultValue, context, "string");
    }

    /**
     * Resolve number flag evaluation
     */
    async resolveNumberEvaluation(
        flagKey: string,
        defaultValue: number,
        context: EvaluationContext
    ): Promise<ResolutionDetails<number>> {
        return this.resolveValue(flagKey, defaultValue, context, "number");
    }

    /**
     * Resolve object flag evaluation
     */
    async resolveObjectEvaluation<T extends JsonValue>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext
    ): Promise<ResolutionDetails<T>> {
        return this.resolveValue(flagKey, defaultValue, context, "object");
    }

    /**
     * Generic value resolution logic
     */
    private async resolveValue<T>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext,
        type: ValueType
    ): Promise<ResolutionDetails<T>> {
        if (
            this.status !== ProviderStatus.READY &&
            this.status !== ProviderStatus.STALE
        ) {
            return {
                value: defaultValue,
                reason: StandardResolutionReasons.DEFAULT,
                errorCode:
                    this.status === ProviderStatus.FATAL
                        ? ErrorCode.PROVIDER_FATAL
                        : ErrorCode.PROVIDER_NOT_READY,
                errorMessage: `Provider status: ${this.status}`,
            };
        }

        try {
            const config = await this.evalWithContext(context);
            const value = getNestedValue(config, flagKey);
            const converter = TYPE_CONVERTERS[type] as ConverterFunction<T>;
            const convertedValue = converter(value, defaultValue);

            return {
                value: convertedValue,
                reason:
                    this.status === ProviderStatus.STALE
                        ? StandardResolutionReasons.STALE
                        : value !== undefined
                          ? StandardResolutionReasons.TARGETING_MATCH
                          : StandardResolutionReasons.DEFAULT,
            };
        } catch (error) {
            return {
                value: defaultValue,
                reason: StandardResolutionReasons.ERROR,
                errorCode: ErrorCode.GENERAL,
                errorMessage:
                    error instanceof Error
                        ? error.message
                        : "Evaluation failed",
            };
        }
    }

    /**
     * Core evaluation logic
     */
    private async evalWithContext(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): Promise<Record<string, any>> {
        // Check on-demand refresh if needed
        await this.checkOnDemandRefresh();

        if (!this.cachedConfig) {
            throw new Error("No configuration available");
        }

        const extractedContext = this.extractContext(context);
        const targetingKey = context.targetingKey;

        // Get applicable variants from experiments if available
        let experimentationArgs: any;
        if (this.cachedExperiments && targetingKey) {
            const variantIds = this.nativeResolver.getApplicableVariants(
                this.cachedExperiments.experiments,
                this.cachedExperiments.experiment_groups,
                this.cachedConfig.dimensions || {},
                extractedContext,
                targetingKey,
                prefixFilter || []
            );
            if (variantIds.length > 0) {
                experimentationArgs = {
                    experiments: this.cachedExperiments.experiments,
                    experiment_groups: this.cachedExperiments.experiment_groups,
                    targeting_key: targetingKey,
                };
            }
        }

        return this.nativeResolver.resolveConfig(
            this.cachedConfig.default_configs,
            this.cachedConfig.contexts,
            this.cachedConfig.overrides,
            this.cachedConfig.dimensions,
            extractedContext,
            "merge",
            prefixFilter,
            experimentationArgs
        );
    }

    /**
     * Fetch configuration from primary, with fallback on error
     */
    private async doRefresh(): Promise<void> {
        try {
            // Fetch both config and experiments from primary
            const [configData, experimentData] = await Promise.all([
                this.primary.fetchConfig(),
                this.primary.supportsExperiments()
                    ? this.primary.fetchActiveExperiments()
                    : Promise.resolve(null),
            ]);

            this.cachedConfig = configData;
            this.cachedExperiments = experimentData;
            this.lastFetchTime = new Date();
        } catch (primaryError) {
            console.warn("Primary data source failed:", primaryError);

            // Try fallback if available
            if (this.fallback) {
                try {
                    const [configData, experimentData] = await Promise.all([
                        this.fallback.fetchConfig(),
                        this.fallback.supportsExperiments()
                            ? this.fallback.fetchActiveExperiments()
                            : Promise.resolve(null),
                    ]);

                    this.cachedConfig = configData;
                    this.cachedExperiments = experimentData;
                    this.lastFetchTime = new Date();
                    this.status = ProviderStatus.STALE;
                } catch (fallbackError) {
                    console.error("Fallback data source also failed:", fallbackError);
                    throw primaryError;
                }
            } else {
                throw primaryError;
            }
        }
    }

    /**
     * Check if on-demand refresh is needed based on TTL
     */
    private async checkOnDemandRefresh(): Promise<void> {
        if (!("ttl" in this.refreshStrategy)) {
            return;
        }

        const strategy = this.refreshStrategy as OnDemandStrategy;
        const now = new Date();

        const shouldRefresh =
            !this.lastFetchTime ||
            now.getTime() - this.lastFetchTime.getTime() > strategy.ttl * 1000;

        if (!shouldRefresh) {
            return;
        }

        try {
            await this.doRefresh();
        } catch (error) {
            console.warn("On-demand refresh failed:", error);
            if (!strategy.use_stale_on_error || !this.cachedConfig) {
                throw error;
            }
            console.log("Using stale configuration due to refresh error.");
        }
    }

    /**
     * Start polling for configuration updates
     */
    private startPolling(interval: number): void {
        this.stopPolling();
        this.pollingInterval = setInterval(async () => {
            try {
                await this.doRefresh();
                console.log("Configuration refreshed via polling.");
            } catch (error) {
                console.error("Polling refresh failed:", error);
            }
        }, interval);
    }

    /**
     * Stop polling
     */
    private stopPolling(): void {
        if (this.pollingInterval) {
            clearInterval(this.pollingInterval);
            this.pollingInterval = null;
        }
    }

    /**
     * Extract context map from EvaluationContext
     */
    private extractContext(context: EvaluationContext): Record<string, any> {
        const extracted: Record<string, any> = {};

        for (const [key, value] of Object.entries(context)) {
            // Skip internal OpenFeature fields
            if (key.startsWith("__") || key === "targetingKey" || key === "timestamp") {
                continue;
            }

            // Only include simple, serializable types
            if (
                typeof value === "string" ||
                typeof value === "number" ||
                typeof value === "boolean"
            ) {
                extracted[key] = value;
            } else if (
                typeof value === "object" &&
                value !== null &&
                !Array.isArray(value)
            ) {
                try {
                    const serialized = JSON.stringify(value);
                    if (
                        serialized.length < 1000 &&
                        Object.keys(value).length < 10
                    ) {
                        extracted[key] = value;
                    }
                } catch {
                    // Skip non-serializable objects
                }
            }
        }

        return extracted;
    }
}
