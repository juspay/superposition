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
    AllFeatureProvider,
    FeatureExperimentMeta,
    EvaluationCacheOptions,
} from "./types";

export interface SuperpositionProviderOptions {
    endpoint: string;
    token: string;
    org_id: string;
    workspace_id: string;
    httpClient?: any;
    evaluationCache?: EvaluationCacheOptions;
}
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

interface CacheEntry {
    value: any;
    timestamp: number;
}

export class SuperpositionAPIProvider
    implements Provider, AllFeatureProvider, FeatureExperimentMeta
{
    readonly metadata: ProviderMetadata = {
        name: "SuperpositionAPIProvider",
    };

    events = new OpenFeatureEventEmitter();
    status: ProviderStatus = ProviderStatus.NOT_READY;

    private cache: Map<string, CacheEntry> | undefined;
    private cacheMaxSize: number;
    private cacheTtl: number;

    constructor(private options: SuperpositionProviderOptions) {
        if (options.evaluationCache) {
            this.cacheMaxSize = options.evaluationCache.size ?? 1000;
            this.cacheTtl = (options.evaluationCache.ttl ?? 300) * 1000;
            this.cache = new Map();
        } else {
            this.cacheMaxSize = 1000;
            this.cacheTtl = 300000;
        }
    }

    /**
     * Initialize the provider
     */
    async initialize(context?: EvaluationContext): Promise<void> {
        this.status = ProviderStatus.READY;
        this.events.emit(ProviderEvents.Ready, {
            message: "Provider ready",
        });
    }

    /**
     * Close the provider and cleanup resources
     */
    async onClose(): Promise<void> {
        if (this.cache) {
            this.cache.clear();
        }
        this.status = ProviderStatus.NOT_READY;
    }

    /**
     * Resolve all features for the given context
     */
    async resolveAllFeatures(
        context: EvaluationContext
    ): Promise<Record<string, any>> {
        return this.resolveRemote(context);
    }

    /**
     * Resolve all features matching prefix filters
     */
    async resolveAllFeaturesWithFilter(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): Promise<Record<string, any>> {
        return this.resolveRemote(context, prefixFilter);
    }

    /**
     * Get applicable variant IDs for the given context
     */
    async getApplicableVariants(context: EvaluationContext): Promise<string[]> {
        const cacheKey = this.buildCacheKey(context, undefined, "variants");

        const cached = this.getCached(cacheKey);
        if (cached !== undefined) {
            return cached.variants || [];
        }

        try {
            const url = this.addContextToUrl(
                `${this.options.endpoint}/applicable-variants`,
                context
            );
            const response = await this.makeRequest(url);
            const data = await response.json();

            this.setCached(cacheKey, data);

            return data.variants || [];
        } catch (error) {
            console.error("Error fetching applicable variants:", error);
            return [];
        }
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
            const config = await this.resolveRemote(context);
            const value = getNestedValue(config, flagKey);
            const converter = TYPE_CONVERTERS[type] as ConverterFunction<T>;
            const convertedValue = converter(value, defaultValue);

            return {
                value: convertedValue,
                reason:
                    value !== undefined
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
     * Core remote resolution logic
     */
    private async resolveRemote(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): Promise<Record<string, any>> {
        const cacheKey = this.buildCacheKey(context, prefixFilter);

        const cached = this.getCached(cacheKey);
        if (cached !== undefined) {
            return cached;
        }

        const url = this.addContextToUrl(
            `${this.options.endpoint}/resolve`,
            context,
            prefixFilter
        );

        const response = await this.makeRequest(url);
        const data = await response.json();

        this.setCached(cacheKey, data);

        return data;
    }

    /**
     * Create unique cache key from context and prefix filter
     */
    private buildCacheKey(
        context: EvaluationContext,
        prefixFilter?: string[],
        suffix?: string
    ): string {
        const parts: string[] = [];

        if (context.targetingKey) {
            parts.push(`targetingKey:${context.targetingKey}`);
        }

        const contextEntries = Object.entries(context)
            .filter(([key]) => key !== "targetingKey")
            .sort(([a], [b]) => a.localeCompare(b));

        for (const [key, value] of contextEntries) {
            if (
                typeof value === "string" ||
                typeof value === "number" ||
                typeof value === "boolean"
            ) {
                parts.push(`${key}:${value}`);
            }
        }

        if (prefixFilter && prefixFilter.length > 0) {
            parts.push(`prefix:${prefixFilter.sort().join(",")}`);
        }

        if (suffix) {
            parts.push(suffix);
        }

        return parts.join("|");
    }

    /**
     * Get cached value if not expired
     */
    private getCached(key: string): any {
        if (!this.cache) {
            return undefined;
        }

        const entry = this.cache.get(key);
        if (!entry) {
            return undefined;
        }

        const now = Date.now();
        if (now - entry.timestamp > this.cacheTtl) {
            this.cache.delete(key);
            return undefined;
        }

        return entry.value;
    }

    /**
     * Store value in cache with timestamp (LRU eviction)
     */
    private setCached(key: string, value: any): void {
        if (!this.cache) {
            return;
        }

        if (this.cache.size >= this.cacheMaxSize && !this.cache.has(key)) {
            let oldestKey: string | undefined;
            let oldestTime = Infinity;

            for (const [k, v] of this.cache.entries()) {
                if (v.timestamp < oldestTime) {
                    oldestTime = v.timestamp;
                    oldestKey = k;
                }
            }

            if (oldestKey) {
                this.cache.delete(oldestKey);
            }
        }

        this.cache.set(key, {
            value,
            timestamp: Date.now(),
        });
    }

    /**
     * Add context fields to URL query params
     */
    private addContextToUrl(
        baseUrl: string,
        context: EvaluationContext,
        prefixFilter?: string[]
    ): string {
        const params = new URLSearchParams();

        for (const [key, value] of Object.entries(context)) {
            if (key === "targetingKey") {
                params.set("targetingKey", String(value));
            } else if (
                typeof value === "string" ||
                typeof value === "number" ||
                typeof value === "boolean"
            ) {
                params.set(key, String(value));
            }
        }

        if (prefixFilter && prefixFilter.length > 0) {
            prefixFilter.forEach((prefix) => {
                params.append("prefix", prefix);
            });
        }

        const queryString = params.toString();
        return queryString ? `${baseUrl}?${queryString}` : baseUrl;
    }

    /**
     * Make HTTP request with auth headers
     */
    private async makeRequest(url: string): Promise<Response> {
        const headers: Record<string, string> = {
            "Content-Type": "application/json",
        };

        if (this.options.token) {
            headers["Authorization"] = `Bearer ${this.options.token}`;
        }

        if (this.options.org_id) {
            headers["x-org-id"] = this.options.org_id;
        }

        if (this.options.workspace_id) {
            headers["x-workspace"] = this.options.workspace_id;
        }

        let response: Response;

        if (this.options.httpClient) {
            response = await this.options.httpClient({
                url,
                headers,
                method: "GET",
            });
        } else {
            response = await fetch(url, {
                method: "GET",
                headers,
            });
        }

        if (!response.ok) {
            const errorText = await response.text();
            throw new Error(
                `HTTP ${response.status}: ${errorText || response.statusText}`
            );
        }

        return response;
    }
}
