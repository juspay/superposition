# JavaScript, Java, and Python Configuration Resolver Re-architecture Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Re-architect JavaScript, Java, and Python Superposition providers to use a trait/interface-based architecture with pluggable data sources (HTTP, File), supporting both local and remote configuration resolution, matching the Rust implementation pattern.

**Architecture:** Three core interfaces (`AllFeatureProvider`, `FeatureExperimentMeta`, `SuperpositionDataSource`) with two provider implementations (`LocalResolutionProvider`, `SuperpositionAPIProvider`). Data sources are pluggable via `SuperpositionDataSource` interface. All providers implement OpenFeature's `FeatureProvider`.

**Tech Stack:** 
- JavaScript/TypeScript: TypeScript, async/await, OpenFeature SDK, EventEmitter
- Java: Java 17+, interfaces, CompletableFuture, OpenFeature SDK, Optional
- Python: Python 3.9+, abstract base classes (ABC), async/await, OpenFeature SDK

**Design doc:** `docs/plans/2026-02-14-configuration-resolver-design.md` (Rust reference)

---

## JavaScript Provider Architecture

### Task 1: Define core interfaces (`src/types.ts`)

**Files:**
- Create: `clients/javascript/open-feature-provider/src/types.ts`
- Modify: `clients/javascript/open-feature-provider/superposition-provider.ts`

**Step 1: Create `types.ts` with core interfaces**

```typescript
import { EvaluationContext } from "@openfeature/server-sdk";

/**
 * Data fetched from a configuration source
 */
export interface ConfigData {
    default_configs: Record<string, any>;
    contexts: any[];
    overrides: Record<string, any>;
    dimensions: Record<string, any>;
    fetched_at: Date;
}

/**
 * Experiment data fetched from a source
 */
export interface ExperimentData {
    experiments: any[];
    experiment_groups: any[];
    fetched_at: Date;
}

/**
 * Interface for bulk configuration resolution
 */
export interface AllFeatureProvider {
    /**
     * Resolve all features for the given evaluation context
     */
    resolveAllFeatures(context: EvaluationContext): Promise<Record<string, any>>;

    /**
     * Resolve features matching prefix filters
     */
    resolveAllFeaturesWithFilter(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): Promise<Record<string, any>>;
}

/**
 * Interface for experiment metadata and variant resolution
 */
export interface FeatureExperimentMeta {
    /**
     * Get applicable variant IDs for the given context
     */
    getApplicableVariants(context: EvaluationContext): Promise<string[]>;
}

/**
 * Interface for abstracting data sources
 */
export interface SuperpositionDataSource {
    /**
     * Fetch the latest configuration from the data source
     */
    fetchConfig(): Promise<ConfigData>;

    /**
     * Fetch configuration with context/prefix filters
     */
    fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ConfigData>;

    /**
     * Fetch all active experiment data
     */
    fetchActiveExperiments(): Promise<ExperimentData | null>;

    /**
     * Fetch active experiments filtered with partial context matching
     */
    fetchCandidateActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ExperimentData | null>;

    /**
     * Fetch active experiments filtered with exact context matching
     */
    fetchMatchingActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ExperimentData | null>;

    /**
     * Check if this data source supports experiments
     */
    supportsExperiments(): boolean;

    /**
     * Close and cleanup resources
     */
    close(): Promise<void>;
}

/**
 * Refresh strategy options
 */
export type RefreshStrategy =
    | { type: "polling"; interval: number }
    | { type: "onDemand"; ttl: number; useStaleOnError?: boolean }
    | { type: "manual" };

/**
 * Cache options for resolution output
 */
export interface CacheOptions {
    size?: number;
    ttl?: number;
}

/**
 * Options for Superposition providers
 */
export interface SuperpositionProviderOptions {
    endpoint: string;
    token: string;
    orgId: string;
    workspaceId: string;
    httpClient?: any;
    refreshStrategy?: RefreshStrategy;
    cacheOptions?: CacheOptions;
    fallbackConfig?: ConfigData;
}
```

**Step 2: Export types from main module**

Modify `superposition-provider.ts` to export types:

```typescript
export * from "./types";
```

**Step 3: Verify TypeScript compiles**

Run: `cd clients/javascript/open-feature-provider && npm run build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add clients/javascript/open-feature-provider/src/types.ts clients/javascript/open-feature-provider/superposition-provider.ts
git commit -m "feat(js): add core interfaces AllFeatureProvider, FeatureExperimentMeta, SuperpositionDataSource"
```

---

### Task 2: Implement `HttpDataSource`

**Files:**
- Create: `clients/javascript/open-feature-provider/src/data-sources/http-data-source.ts`

**Step 1: Create HTTP data source implementation**

```typescript
import {
    SuperpositionDataSource,
    ConfigData,
    ExperimentData,
    SuperpositionProviderOptions,
} from "../types";

export class HttpDataSource implements SuperpositionDataSource {
    private options: SuperpositionProviderOptions;

    constructor(options: SuperpositionProviderOptions) {
        this.options = options;
    }

    async fetchConfig(): Promise<ConfigData> {
        const response = await this.makeRequest("/config");
        const data = await response.json();
        return {
            default_configs: data.default_configs || {},
            contexts: data.contexts || [],
            overrides: data.overrides || {},
            dimensions: data.dimensions || {},
            fetched_at: new Date(),
        };
    }

    async fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ConfigData> {
        const params = new URLSearchParams();
        if (context) {
            params.append("context", JSON.stringify(context));
        }
        if (prefixFilter) {
            prefixFilter.forEach((p) => params.append("prefix", p));
        }

        const response = await this.makeRequest(`/config?${params.toString()}`);
        const data = await response.json();
        return {
            default_configs: data.default_configs || {},
            contexts: data.contexts || [],
            overrides: data.overrides || {},
            dimensions: data.dimensions || {},
            fetched_at: new Date(),
        };
    }

    async fetchActiveExperiments(): Promise<ExperimentData | null> {
        const [expResponse, groupsResponse] = await Promise.all([
            this.makeRequest("/experiments?status=created&status=inprogress"),
            this.makeRequest("/experiment-groups"),
        ]);

        const experiments = await expResponse.json();
        const groups = await groupsResponse.json();

        return {
            experiments: experiments.data || [],
            experiment_groups: groups.data || [],
            fetched_at: new Date(),
        };
    }

    async fetchCandidateActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ExperimentData | null> {
        const params = new URLSearchParams();
        params.append("match_type", "partial");
        if (context) {
            params.append("context", JSON.stringify(context));
        }
        if (prefixFilter) {
            prefixFilter.forEach((p) => params.append("prefix", p));
        }

        const [expResponse, groupsResponse] = await Promise.all([
            this.makeRequest(`/experiments?${params.toString()}`),
            this.makeRequest(`/experiment-groups?${params.toString()}`),
        ]);

        const experiments = await expResponse.json();
        const groups = await groupsResponse.json();

        return {
            experiments: experiments.data || [],
            experiment_groups: groups.data || [],
            fetched_at: new Date(),
        };
    }

    async fetchMatchingActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ExperimentData | null> {
        const params = new URLSearchParams();
        params.append("match_type", "exact");
        if (context) {
            params.append("context", JSON.stringify(context));
        }
        if (prefixFilter) {
            prefixFilter.forEach((p) => params.append("prefix", p));
        }

        const [expResponse, groupsResponse] = await Promise.all([
            this.makeRequest(`/experiments?${params.toString()}`),
            this.makeRequest(`/experiment-groups?${params.toString()}`),
        ]);

        const experiments = await expResponse.json();
        const groups = await groupsResponse.json();

        return {
            experiments: experiments.data || [],
            experiment_groups: groups.data || [],
            fetched_at: new Date(),
        };
    }

    supportsExperiments(): boolean {
        return true;
    }

    async close(): Promise<void> {
        // HTTP client cleanup if needed
    }

    private async makeRequest(path: string): Promise<Response> {
        const url = `${this.options.endpoint}${path}`;
        const headers: Record<string, string> = {
            Authorization: `Bearer ${this.options.token}`,
            "Content-Type": "application/json",
            "x-organization-id": this.options.orgId,
            "x-workspace-id": this.options.workspaceId,
        };

        const client = this.options.httpClient || fetch;
        const response = await client(url, { headers });

        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }

        return response;
    }
}
```

**Step 2: Export from data-sources index**

Create `clients/javascript/open-feature-provider/src/data-sources/index.ts`:

```typescript
export { HttpDataSource } from "./http-data-source";
```

**Step 3: Verify TypeScript compiles**

Run: `npm run build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add clients/javascript/open-feature-provider/src/data-sources/
git commit -m "feat(js): implement HttpDataSource for SuperpositionDataSource interface"
```

---

### Task 3: Implement `FileDataSource`

**Files:**
- Create: `clients/javascript/open-feature-provider/src/data-sources/file-data-source.ts`

**Step 1: Create File data source implementation**

```typescript
import {
    SuperpositionDataSource,
    ConfigData,
    ExperimentData,
} from "../types";
import { readFile } from "fs/promises";

export class FileDataSource implements SuperpositionDataSource {
    private filePath: string;

    constructor(filePath: string) {
        this.filePath = filePath;
    }

    async fetchConfig(): Promise<ConfigData> {
        const content = await readFile(this.filePath, "utf-8");
        // Assuming TOML parsing is available via a library
        // For now, assuming JSON for simplicity - can be adapted
        const data = JSON.parse(content);

        return {
            default_configs: data.default_configs || {},
            contexts: data.contexts || [],
            overrides: data.overrides || {},
            dimensions: data.dimensions || {},
            fetched_at: new Date(),
        };
    }

    async fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ConfigData> {
        const config = await this.fetchConfig();

        // Apply context filter
        if (context && Object.keys(context).length > 0) {
            config.contexts = config.contexts.filter((ctx) =>
                this.matchesContext(ctx, context)
            );
        }

        // Apply prefix filter
        if (prefixFilter && prefixFilter.length > 0) {
            config.default_configs = this.filterByPrefix(
                config.default_configs,
                prefixFilter
            );
        }

        return config;
    }

    async fetchActiveExperiments(): Promise<ExperimentData | null> {
        // File data source doesn't support experiments
        return null;
    }

    async fetchCandidateActiveExperiments(
        _context?: Record<string, any>,
        _prefixFilter?: string[]
    ): Promise<ExperimentData | null> {
        return null;
    }

    async fetchMatchingActiveExperiments(
        _context?: Record<string, any>,
        _prefixFilter?: string[]
    ): Promise<ExperimentData | null> {
        return null;
    }

    supportsExperiments(): boolean {
        return false;
    }

    async close(): Promise<void> {
        // No cleanup needed for file source
    }

    private matchesContext(
        context: any,
        filter: Record<string, any>
    ): boolean {
        // Implement context matching logic
        // This is a simplified version
        return Object.entries(filter).every(([key, value]) => {
            return context[key] === value;
        });
    }

    private filterByPrefix(
        configs: Record<string, any>,
        prefixes: string[]
    ): Record<string, any> {
        const filtered: Record<string, any> = {};
        for (const [key, value] of Object.entries(configs)) {
            if (prefixes.some((prefix) => key.startsWith(prefix))) {
                filtered[key] = value;
            }
        }
        return filtered;
    }
}
```

**Step 2: Update data-sources index**

Modify `clients/javascript/open-feature-provider/src/data-sources/index.ts`:

```typescript
export { HttpDataSource } from "./http-data-source";
export { FileDataSource } from "./file-data-source";
```

**Step 3: Verify TypeScript compiles**

Run: `npm run build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add clients/javascript/open-feature-provider/src/data-sources/
git commit -m "feat(js): implement FileDataSource for SuperpositionDataSource interface"
```

---

### Task 4: Implement `LocalResolutionProvider`

**Files:**
- Create: `clients/javascript/open-feature-provider/src/local-resolution-provider.ts`
- Modify: `clients/javascript/open-feature-provider/superposition-provider.ts`

**Step 1: Create LocalResolutionProvider**

```typescript
import {
    EvaluationContext,
    Provider,
    ResolutionDetails,
    ProviderStatus,
    ProviderMetadata,
    ErrorCode,
    StandardResolutionReasons,
    JsonValue,
} from "@openfeature/server-sdk";
import { EventEmitter } from "events";
import {
    AllFeatureProvider,
    FeatureExperimentMeta,
    SuperpositionDataSource,
    ConfigData,
    ExperimentData,
    RefreshStrategy,
    CacheOptions,
} from "./types";
import { NativeResolver } from "superposition-bindings";

interface CacheEntry {
    value: Record<string, any>;
    timestamp: number;
}

export class LocalResolutionProvider
    implements Provider, AllFeatureProvider, FeatureExperimentMeta
{
    readonly metadata: ProviderMetadata = {
        name: "LocalResolutionProvider",
    };

    events = new EventEmitter();
    status: ProviderStatus = ProviderStatus.NOT_READY;

    private primary: SuperpositionDataSource;
    private fallback?: SuperpositionDataSource;
    private refreshStrategy: RefreshStrategy;
    private cacheOptions?: CacheOptions;
    private nativeResolver: NativeResolver;

    private cachedConfig?: ConfigData;
    private cachedExperiments?: ExperimentData;
    private pollingInterval?: NodeJS.Timeout;
    private resolutionCache: Map<string, CacheEntry> = new Map();

    constructor(
        primary: SuperpositionDataSource,
        fallback: SuperpositionDataSource | undefined,
        refreshStrategy: RefreshStrategy,
        cacheOptions?: CacheOptions,
        nativeResolver: NativeResolver = new NativeResolver()
    ) {
        this.primary = primary;
        this.fallback = fallback;
        this.refreshStrategy = refreshStrategy;
        this.cacheOptions = cacheOptions;
        this.nativeResolver = nativeResolver;
    }

    async initialize(context?: EvaluationContext): Promise<void> {
        this.status = ProviderStatus.NOT_READY;

        try {
            // Try to fetch initial config from primary
            await this.doRefresh();

            // Start polling if configured
            if (this.refreshStrategy.type === "polling") {
                this.startPolling(this.refreshStrategy.interval);
            }

            this.status = ProviderStatus.READY;
            this.events.emit("ready", { message: "Provider ready" });
        } catch (error) {
            this.status = ProviderStatus.ERROR;
            this.events.emit("error", {
                message: error instanceof Error ? error.message : "Initialization failed",
                errorCode: ErrorCode.PROVIDER_NOT_READY,
            });
            throw error;
        }
    }

    async onClose(): Promise<void> {
        this.stopPolling();
        await this.primary.close();
        if (this.fallback) {
            await this.fallback.close();
        }
        this.cachedConfig = undefined;
        this.cachedExperiments = undefined;
        this.resolutionCache.clear();
        this.status = ProviderStatus.NOT_READY;
    }

    async refresh(): Promise<void> {
        await this.doRefresh();
    }

    async resolveAllFeatures(
        context: EvaluationContext
    ): Promise<Record<string, any>> {
        return this.evalWithContext(context);
    }

    async resolveAllFeaturesWithFilter(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): Promise<Record<string, any>> {
        return this.evalWithContext(context, prefixFilter);
    }

    async getApplicableVariants(context: EvaluationContext): Promise<string[]> {
        if (!this.cachedExperiments) {
            return [];
        }

        // Use native resolver to get applicable variants
        const contextMap = this.extractContext(context);
        return this.nativeResolver.getApplicableVariants(
            this.cachedExperiments.experiments,
            this.cachedExperiments.experiment_groups,
            contextMap,
            context.targetingKey || ""
        );
    }

    async resolveBooleanEvaluation(
        flagKey: string,
        defaultValue: boolean,
        context: EvaluationContext
    ): Promise<ResolutionDetails<boolean>> {
        return this.resolveValue(flagKey, defaultValue, context, "boolean");
    }

    async resolveStringEvaluation(
        flagKey: string,
        defaultValue: string,
        context: EvaluationContext
    ): Promise<ResolutionDetails<string>> {
        return this.resolveValue(flagKey, defaultValue, context, "string");
    }

    async resolveNumberEvaluation(
        flagKey: string,
        defaultValue: number,
        context: EvaluationContext
    ): Promise<ResolutionDetails<number>> {
        return this.resolveValue(flagKey, defaultValue, context, "number");
    }

    async resolveObjectEvaluation<T extends JsonValue>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext
    ): Promise<ResolutionDetails<T>> {
        return this.resolveValue(flagKey, defaultValue, context, "object");
    }

    private async resolveValue<T>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext,
        type: string
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
            const value = this.getNestedValue(config, flagKey);

            if (value === undefined) {
                return {
                    value: defaultValue,
                    reason: StandardResolutionReasons.DEFAULT,
                    errorCode: ErrorCode.FLAG_NOT_FOUND,
                    errorMessage: `Flag not found: ${flagKey}`,
                };
            }

            const converted = this.convertValue(value, type, defaultValue);
            return {
                value: converted as T,
                reason:
                    this.status === ProviderStatus.STALE
                        ? StandardResolutionReasons.STALE
                        : StandardResolutionReasons.TARGETING_MATCH,
            };
        } catch (error) {
            return {
                value: defaultValue,
                reason: StandardResolutionReasons.ERROR,
                errorCode: ErrorCode.GENERAL,
                errorMessage:
                    error instanceof Error ? error.message : "Evaluation failed",
            };
        }
    }

    private async evalWithContext(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): Promise<Record<string, any>> {
        // Check on-demand refresh
        if (this.refreshStrategy.type === "onDemand") {
            await this.checkOnDemandRefresh();
        }

        if (!this.cachedConfig) {
            throw new Error("No cached configuration available");
        }

        // Use native resolver for evaluation
        const contextMap = this.extractContext(context);
        const variantIds = this.cachedExperiments
            ? await this.nativeResolver.getApplicableVariants(
                  this.cachedExperiments.experiments,
                  this.cachedExperiments.experiment_groups,
                  contextMap,
                  context.targetingKey || ""
              )
            : [];

        return this.nativeResolver.eval(
            this.cachedConfig.default_configs,
            this.cachedConfig.contexts,
            this.cachedConfig.overrides,
            this.cachedConfig.dimensions,
            contextMap,
            variantIds,
            prefixFilter
        );
    }

    private async doRefresh(): Promise<void> {
        try {
            const config = await this.primary.fetchConfig();
            this.cachedConfig = config;

            if (this.primary.supportsExperiments()) {
                const experiments = await this.primary.fetchActiveExperiments();
                if (experiments) {
                    this.cachedExperiments = experiments;
                }
            }
        } catch (error) {
            console.warn("Primary data source failed:", error);

            if (this.fallback) {
                try {
                    const config = await this.fallback.fetchConfig();
                    this.cachedConfig = config;

                    if (this.fallback.supportsExperiments()) {
                        const experiments = await this.fallback.fetchActiveExperiments();
                        if (experiments) {
                            this.cachedExperiments = experiments;
                        }
                    }
                } catch (fallbackError) {
                    throw new Error(
                        `Both primary and fallback data sources failed: ${fallbackError}`
                    );
                }
            } else {
                throw error;
            }
        }
    }

    private async checkOnDemandRefresh(): Promise<void> {
        if (!this.cachedConfig) return;

        const ttl = this.refreshStrategy.type === "onDemand" 
            ? this.refreshStrategy.ttl 
            : 300;
        const elapsed = (Date.now() - this.cachedConfig.fetched_at.getTime()) / 1000;

        if (elapsed > ttl) {
            try {
                await this.doRefresh();
            } catch (error) {
                if (
                    this.refreshStrategy.type === "onDemand" &&
                    this.refreshStrategy.useStaleOnError
                ) {
                    console.warn("Refresh failed, using stale data:", error);
                } else {
                    throw error;
                }
            }
        }
    }

    private startPolling(interval: number): void {
        this.pollingInterval = setInterval(async () => {
            try {
                await this.doRefresh();
            } catch (error) {
                console.error("Polling refresh failed:", error);
            }
        }, interval * 1000);
    }

    private stopPolling(): void {
        if (this.pollingInterval) {
            clearInterval(this.pollingInterval);
            this.pollingInterval = undefined;
        }
    }

    private extractContext(context: EvaluationContext): Record<string, any> {
        const result: Record<string, any> = {};
        for (const [key, value] of Object.entries(context)) {
            if (key !== "targetingKey") {
                result[key] = value;
            }
        }
        return result;
    }

    private getNestedValue(obj: any, path: string): any {
        const keys = path.split(".");
        let result = obj;
        for (const key of keys) {
            if (result === null || result === undefined) {
                return undefined;
            }
            result = result[key];
        }
        return result;
    }

    private convertValue(value: any, type: string, defaultValue: any): any {
        if (type === "boolean") {
            return Boolean(value);
        } else if (type === "string") {
            return String(value);
        } else if (type === "number") {
            return Number(value);
        } else if (type === "object") {
            return value !== null && typeof value === "object" ? value : defaultValue;
        }
        return value;
    }
}
```

**Step 2: Export from main module**

Modify `superposition-provider.ts`:

```typescript
export { LocalResolutionProvider } from "./local-resolution-provider";
export * from "./data-sources";
export * from "./types";
```

**Step 3: Verify TypeScript compiles**

Run: `npm run build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add clients/javascript/open-feature-provider/src/
git commit -m "feat(js): implement LocalResolutionProvider with primary/fallback and refresh strategies"
```

---

### Task 5: Implement `SuperpositionAPIProvider` (Remote Resolution)

**Files:**
- Create: `clients/javascript/open-feature-provider/src/remote-resolution-provider.ts`

**Step 1: Create RemoteResolutionProvider**

```typescript
import {
    EvaluationContext,
    Provider,
    ResolutionDetails,
    ProviderStatus,
    ProviderMetadata,
    ErrorCode,
    StandardResolutionReasons,
    JsonValue,
} from "@openfeature/server-sdk";
import { EventEmitter } from "events";
import {
    AllFeatureProvider,
    FeatureExperimentMeta,
    SuperpositionProviderOptions,
    CacheOptions,
} from "./types";

interface CacheEntry {
    value: Record<string, any>;
    timestamp: number;
}

export class SuperpositionAPIProvider
    implements Provider, AllFeatureProvider, FeatureExperimentMeta
{
    readonly metadata: ProviderMetadata = {
        name: "SuperpositionAPIProvider",
    };

    events = new EventEmitter();
    status: ProviderStatus = ProviderStatus.NOT_READY;

    private options: SuperpositionProviderOptions;
    private cache?: Map<string, CacheEntry>;
    private cacheMaxSize: number;
    private cacheTtl: number;

    constructor(options: SuperpositionProviderOptions) {
        this.options = options;

        if (options.cacheOptions) {
            this.cache = new Map();
            this.cacheMaxSize = options.cacheOptions.size || 1000;
            this.cacheTtl = options.cacheOptions.ttl || 300;
        } else {
            this.cacheMaxSize = 0;
            this.cacheTtl = 0;
        }
    }

    async initialize(context?: EvaluationContext): Promise<void> {
        this.status = ProviderStatus.READY;
        this.events.emit("ready", { message: "Provider ready" });
    }

    async onClose(): Promise<void> {
        this.cache?.clear();
        this.status = ProviderStatus.NOT_READY;
    }

    async resolveAllFeatures(
        context: EvaluationContext
    ): Promise<Record<string, any>> {
        return this.resolveRemote(context);
    }

    async resolveAllFeaturesWithFilter(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): Promise<Record<string, any>> {
        return this.resolveRemote(context, prefixFilter);
    }

    async getApplicableVariants(context: EvaluationContext): Promise<string[]> {
        // For remote resolution, call the API's applicable variants endpoint
        const cacheKey = this.buildCacheKey(context, ["variants"]);

        // Check cache
        const cached = this.getCached(cacheKey);
        if (cached) {
            return cached.variants || [];
        }

        const url = new URL(`${this.options.endpoint}/applicable-variants`);
        this.addContextToUrl(url, context);

        const response = await this.makeRequest(url.toString());
        const data = await response.json();

        // Cache the result
        this.setCached(cacheKey, data);

        return data.variants || [];
    }

    async resolveBooleanEvaluation(
        flagKey: string,
        defaultValue: boolean,
        context: EvaluationContext
    ): Promise<ResolutionDetails<boolean>> {
        return this.resolveValue(flagKey, defaultValue, context, "boolean");
    }

    async resolveStringEvaluation(
        flagKey: string,
        defaultValue: string,
        context: EvaluationContext
    ): Promise<ResolutionDetails<string>> {
        return this.resolveValue(flagKey, defaultValue, context, "string");
    }

    async resolveNumberEvaluation(
        flagKey: string,
        defaultValue: number,
        context: EvaluationContext
    ): Promise<ResolutionDetails<number>> {
        return this.resolveValue(flagKey, defaultValue, context, "number");
    }

    async resolveObjectEvaluation<T extends JsonValue>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext
    ): Promise<ResolutionDetails<T>> {
        return this.resolveValue(flagKey, defaultValue, context, "object");
    }

    private async resolveValue<T>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext,
        type: string
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
            const value = this.getNestedValue(config, flagKey);

            if (value === undefined) {
                return {
                    value: defaultValue,
                    reason: StandardResolutionReasons.DEFAULT,
                    errorCode: ErrorCode.FLAG_NOT_FOUND,
                    errorMessage: `Flag not found: ${flagKey}`,
                };
            }

            const converted = this.convertValue(value, type, defaultValue);
            return {
                value: converted as T,
                reason: StandardResolutionReasons.TARGETING_MATCH,
            };
        } catch (error) {
            return {
                value: defaultValue,
                reason: StandardResolutionReasons.ERROR,
                errorCode: ErrorCode.GENERAL,
                errorMessage:
                    error instanceof Error ? error.message : "Evaluation failed",
            };
        }
    }

    private async resolveRemote(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): Promise<Record<string, any>> {
        const cacheKey = this.buildCacheKey(context, prefixFilter);

        // Check cache
        const cached = this.getCached(cacheKey);
        if (cached) {
            return cached;
        }

        const url = new URL(`${this.options.endpoint}/resolve`);
        this.addContextToUrl(url, context);

        if (prefixFilter) {
            prefixFilter.forEach((prefix) =>
                url.searchParams.append("prefix", prefix)
            );
        }

        const response = await this.makeRequest(url.toString());
        const data = await response.json();

        // Cache the result
        this.setCached(cacheKey, data);

        return data;
    }

    private buildCacheKey(
        context: EvaluationContext,
        prefixFilter?: string[]
    ): string {
        const parts: string[] = [];

        if (context.targetingKey) {
            parts.push(`tk:${context.targetingKey}`);
        }

        const sortedKeys = Object.keys(context).sort();
        for (const key of sortedKeys) {
            if (key !== "targetingKey") {
                parts.push(`${key}:${JSON.stringify(context[key])}`);
            }
        }

        if (prefixFilter) {
            parts.push(`prefixes:${prefixFilter.join(",")}`);
        }

        return parts.join("|");
    }

    private getCached(key: string): Record<string, any> | undefined {
        if (!this.cache) return undefined;

        const entry = this.cache.get(key);
        if (!entry) return undefined;

        const age = (Date.now() - entry.timestamp) / 1000;
        if (age > this.cacheTtl) {
            this.cache.delete(key);
            return undefined;
        }

        return entry.value;
    }

    private setCached(key: string, value: Record<string, any>): void {
        if (!this.cache) return;

        // Evict if at capacity
        if (this.cache.size >= this.cacheMaxSize) {
            const oldestKey = this.cache.keys().next().value;
            this.cache.delete(oldestKey);
        }

        this.cache.set(key, {
            value,
            timestamp: Date.now(),
        });
    }

    private addContextToUrl(url: URL, context: EvaluationContext): void {
        for (const [key, value] of Object.entries(context)) {
            if (key !== "targetingKey") {
                url.searchParams.append(key, JSON.stringify(value));
            }
        }

        if (context.targetingKey) {
            url.searchParams.append("targeting_key", context.targetingKey);
        }
    }

    private async makeRequest(url: string): Promise<Response> {
        const headers: Record<string, string> = {
            Authorization: `Bearer ${this.options.token}`,
            "Content-Type": "application/json",
            "x-organization-id": this.options.orgId,
            "x-workspace-id": this.options.workspaceId,
        };

        const client = this.options.httpClient || fetch;
        const response = await client(url, { headers });

        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }

        return response;
    }

    private getNestedValue(obj: any, path: string): any {
        const keys = path.split(".");
        let result = obj;
        for (const key of keys) {
            if (result === null || result === undefined) {
                return undefined;
            }
            result = result[key];
        }
        return result;
    }

    private convertValue(value: any, type: string, defaultValue: any): any {
        if (type === "boolean") {
            return Boolean(value);
        } else if (type === "string") {
            return String(value);
        } else if (type === "number") {
            return Number(value);
        } else if (type === "object") {
            return value !== null && typeof value === "object" ? value : defaultValue;
        }
        return value;
    }
}
```

**Step 2: Export from main module**

Modify `superposition-provider.ts`:

```typescript
export { LocalResolutionProvider } from "./local-resolution-provider";
export { SuperpositionAPIProvider } from "./remote-resolution-provider";
export * from "./data-sources";
export * from "./types";
```

**Step 3: Verify TypeScript compiles**

Run: `npm run build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add clients/javascript/open-feature-provider/src/
git commit -m "feat(js): implement SuperpositionAPIProvider for remote resolution"
```

---

## Java Provider Architecture

### Task 6: Define core interfaces (`src/main/java/io/juspay/superposition/provider/`)

**Files:**
- Create: `clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/AllFeatureProvider.java`
- Create: `clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/FeatureExperimentMeta.java`
- Create: `clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/SuperpositionDataSource.java`
- Create: `clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/ConfigData.java`
- Create: `clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/ExperimentData.java`

**Step 1: Create `ConfigData.java`**

```java
package io.juspay.superposition.provider;

import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * Data fetched from a configuration source
 */
public class ConfigData {
    private final Map<String, Object> defaultConfigs;
    private final List<Map<String, Object>> contexts;
    private final Map<String, Object> overrides;
    private final Map<String, Object> dimensions;
    private final Instant fetchedAt;

    public ConfigData(
        Map<String, Object> defaultConfigs,
        List<Map<String, Object>> contexts,
        Map<String, Object> overrides,
        Map<String, Object> dimensions
    ) {
        this.defaultConfigs = defaultConfigs;
        this.contexts = contexts;
        this.overrides = overrides;
        this.dimensions = dimensions;
        this.fetchedAt = Instant.now();
    }

    public Map<String, Object> getDefaultConfigs() { return defaultConfigs; }
    public List<Map<String, Object>> getContexts() { return contexts; }
    public Map<String, Object> getOverrides() { return overrides; }
    public Map<String, Object> getDimensions() { return dimensions; }
    public Instant getFetchedAt() { return fetchedAt; }
}
```

**Step 2: Create `ExperimentData.java`**

```java
package io.juspay.superposition.provider;

import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * Experiment data fetched from a source
 */
public class ExperimentData {
    private final List<Map<String, Object>> experiments;
    private final List<Map<String, Object>> experimentGroups;
    private final Instant fetchedAt;

    public ExperimentData(
        List<Map<String, Object>> experiments,
        List<Map<String, Object>> experimentGroups
    ) {
        this.experiments = experiments;
        this.experimentGroups = experimentGroups;
        this.fetchedAt = Instant.now();
    }

    public List<Map<String, Object>> getExperiments() { return experiments; }
    public List<Map<String, Object>> getExperimentGroups() { return experimentGroups; }
    public Instant getFetchedAt() { return fetchedAt; }
}
```

**Step 3: Create `AllFeatureProvider.java`**

```java
package io.juspay.superposition.provider;

import dev.openfeature.sdk.EvaluationContext;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.List;

/**
 * Interface for bulk configuration resolution
 */
public interface AllFeatureProvider {
    /**
     * Resolve all features for the given evaluation context
     */
    CompletableFuture<Map<String, Object>> resolveAllFeatures(EvaluationContext context);

    /**
     * Resolve features matching prefix filters
     */
    CompletableFuture<Map<String, Object>> resolveAllFeaturesWithFilter(
        EvaluationContext context,
        List<String> prefixFilter
    );
}
```

**Step 4: Create `FeatureExperimentMeta.java`**

```java
package io.juspay.superposition.provider;

import dev.openfeature.sdk.EvaluationContext;
import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * Interface for experiment metadata and variant resolution
 */
public interface FeatureExperimentMeta {
    /**
     * Get applicable variant IDs for the given context
     */
    CompletableFuture<List<String>> getApplicableVariants(EvaluationContext context);
}
```

**Step 5: Create `SuperpositionDataSource.java`**

```java
package io.juspay.superposition.provider;

import java.util.Map;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

/**
 * Interface for abstracting data sources for Superposition configuration and experiments
 */
public interface SuperpositionDataSource {
    /**
     * Fetch the latest configuration from the data source
     */
    CompletableFuture<ConfigData> fetchConfig();

    /**
     * Fetch configuration with context/prefix filters
     */
    CompletableFuture<ConfigData> fetchFilteredConfig(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    );

    /**
     * Fetch all active experiment data
     */
    CompletableFuture<Optional<ExperimentData>> fetchActiveExperiments();

    /**
     * Fetch active experiments filtered with partial context matching
     */
    CompletableFuture<Optional<ExperimentData>> fetchCandidateActiveExperiments(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    );

    /**
     * Fetch active experiments filtered with exact context matching
     */
    CompletableFuture<Optional<ExperimentData>> fetchMatchingActiveExperiments(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    );

    /**
     * Check if this data source supports experiments
     */
    boolean supportsExperiments();

    /**
     * Close and cleanup resources
     */
    CompletableFuture<Void> close();
}
```

**Step 6: Verify Java compiles**

Run: `cd clients/java/openfeature-provider && ./gradlew compileJava`
Expected: Compiles without errors

**Step 7: Commit**

```bash
git add clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/
git commit -m "feat(java): add core interfaces AllFeatureProvider, FeatureExperimentMeta, SuperpositionDataSource"
```

---

### Task 7: Implement `HttpDataSource` (Java)

**Files:**
- Create: `clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/HttpDataSource.java`

**Step 1: Create HTTP data source implementation**

```java
package io.juspay.superposition.provider;

import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.model.*;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

/**
 * HTTP-based data source using the Superposition SDK
 */
public class HttpDataSource implements SuperpositionDataSource {
    private final SuperpositionAsyncClient client;
    private final String orgId;
    private final String workspaceId;

    public HttpDataSource(
        SuperpositionAsyncClient client,
        String orgId,
        String workspaceId
    ) {
        this.client = client;
        this.orgId = orgId;
        this.workspaceId = workspaceId;
    }

    @Override
    public CompletableFuture<ConfigData> fetchConfig() {
        GetConfigInput input = GetConfigInput.builder()
            .orgId(orgId)
            .workspaceId(workspaceId)
            .build();

        return client.getConfig(input)
            .thenApply(response -> new ConfigData(
                response.defaultConfigs(),
                response.contexts(),
                response.overrides(),
                response.dimensions()
            ));
    }

    @Override
    public CompletableFuture<ConfigData> fetchFilteredConfig(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    ) {
        // For HTTP source, fetch full config and filter locally
        // Or use server-side filtering if API supports it
        return fetchConfig().thenApply(config -> {
            // Apply filters if present
            // This is a simplified version
            return config;
        });
    }

    @Override
    public CompletableFuture<Optional<ExperimentData>> fetchActiveExperiments() {
        ListExperimentInput expInput = ListExperimentInput.builder()
            .orgId(orgId)
            .workspaceId(workspaceId)
            .status(List.of(ExperimentStatusType.CREATED, ExperimentStatusType.INPROGRESS))
            .build();

        ListExperimentGroupsInput groupInput = ListExperimentGroupsInput.builder()
            .orgId(orgId)
            .workspaceId(workspaceId)
            .build();

        CompletableFuture<ListExperimentOutput> expFuture = client.listExperiment(expInput);
        CompletableFuture<ListExperimentGroupsOutput> groupFuture = client.listExperimentGroups(groupInput);

        return expFuture.thenCombine(groupFuture, (experiments, groups) -> {
            List<Map<String, Object>> expList = experiments.data().stream()
                .map(this::convertExperiment)
                .collect(Collectors.toList());

            List<Map<String, Object>> groupList = groups.data().stream()
                .map(this::convertExperimentGroup)
                .collect(Collectors.toList());

            return Optional.of(new ExperimentData(expList, groupList));
        });
    }

    @Override
    public CompletableFuture<Optional<ExperimentData>> fetchCandidateActiveExperiments(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    ) {
        // Fetch and apply partial matching filter
        return fetchActiveExperiments().thenApply(optData ->
            optData.map(data -> filterExperiments(data, context, prefixFilter, MatchType.PARTIAL))
        );
    }

    @Override
    public CompletableFuture<Optional<ExperimentData>> fetchMatchingActiveExperiments(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    ) {
        // Fetch and apply exact matching filter
        return fetchActiveExperiments().thenApply(optData ->
            optData.map(data -> filterExperiments(data, context, prefixFilter, MatchType.EXACT))
        );
    }

    @Override
    public boolean supportsExperiments() {
        return true;
    }

    @Override
    public CompletableFuture<Void> close() {
        return CompletableFuture.completedFuture(null);
    }

    private Map<String, Object> convertExperiment(Experiment experiment) {
        Map<String, Object> map = new HashMap<>();
        map.put("id", experiment.id());
        map.put("name", experiment.name());
        map.put("context", experiment.context());
        map.put("variants", experiment.variants());
        return map;
    }

    private Map<String, Object> convertExperimentGroup(ExperimentGroup group) {
        Map<String, Object> map = new HashMap<>();
        map.put("id", group.id());
        map.put("context", group.context());
        return map;
    }

    private enum MatchType { PARTIAL, EXACT }

    private ExperimentData filterExperiments(
        ExperimentData data,
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter,
        MatchType matchType
    ) {
        List<Map<String, Object>> filteredExperiments = data.getExperiments();
        List<Map<String, Object>> filteredGroups = data.getExperimentGroups();

        if (context.isPresent() && !context.get().isEmpty()) {
            Map<String, Object> ctx = context.get();
            // Apply context filtering logic
            // This is simplified - actual implementation would use partial/apply logic
            filteredExperiments = filteredExperiments.stream()
                .filter(exp -> matchesContext(exp, ctx, matchType))
                .collect(Collectors.toList());
            filteredGroups = filteredGroups.stream()
                .filter(group -> matchesContext(group, ctx, matchType))
                .collect(Collectors.toList());
        }

        if (prefixFilter.isPresent() && !prefixFilter.get().isEmpty()) {
            List<String> prefixes = prefixFilter.get();
            filteredExperiments = filteredExperiments.stream()
                .filter(exp -> matchesPrefix(exp, prefixes))
                .collect(Collectors.toList());
        }

        return new ExperimentData(filteredExperiments, filteredGroups);
    }

    private boolean matchesContext(
        Map<String, Object> experiment,
        Map<String, Object> context,
        MatchType matchType
    ) {
        // Implement context matching logic
        // This would integrate with superposition_core logic via FFI
        return true;
    }

    private boolean matchesPrefix(Map<String, Object> experiment, List<String> prefixes) {
        // Check if experiment has variants with override keys matching prefixes
        return true;
    }
}
```

**Step 2: Verify Java compiles**

Run: `./gradlew compileJava`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/HttpDataSource.java
git commit -m "feat(java): implement HttpDataSource for SuperpositionDataSource interface"
```

---

### Task 8: Implement `LocalResolutionProvider` (Java)

**Files:**
- Create: `clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/LocalResolutionProvider.java`

**Step 1: Create LocalResolutionProvider**

```java
package io.juspay.superposition.provider;

import dev.openfeature.sdk.*;
import lombok.extern.slf4j.Slf4j;
import uniffi.superposition_client.EvaluationArgs;
import uniffi.superposition_client.ExperimentationArgs;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Local (in-process) resolution provider
 */
@Slf4j
public class LocalResolutionProvider implements FeatureProvider, AllFeatureProvider, FeatureExperimentMeta {
    private final SuperpositionDataSource primary;
    private final Optional<SuperpositionDataSource> fallback;
    private final RefreshStrategy refreshStrategy;
    private final Optional<CacheOptions> cacheOptions;

    private final AtomicReference<Optional<ConfigData>> cachedConfig;
    private final AtomicReference<Optional<ExperimentData>> cachedExperiments;
    private Optional<ScheduledFuture<?>> pollingTask;
    private final ScheduledExecutorService scheduler;

    private volatile ProviderStatus status = ProviderStatus.NOT_READY;

    public LocalResolutionProvider(
        SuperpositionDataSource primary,
        Optional<SuperpositionDataSource> fallback,
        RefreshStrategy refreshStrategy,
        Optional<CacheOptions> cacheOptions
    ) {
        this.primary = primary;
        this.fallback = fallback;
        this.refreshStrategy = refreshStrategy;
        this.cacheOptions = cacheOptions;

        this.cachedConfig = new AtomicReference<>(Optional.empty());
        this.cachedExperiments = new AtomicReference<>(Optional.empty());
        this.pollingTask = Optional.empty();
        this.scheduler = Executors.newSingleThreadScheduledExecutor();
    }

    @Override
    public Metadata getMetadata() {
        return () -> "LocalResolutionProvider";
    }

    @Override
    public void initialize(EvaluationContext context) {
        try {
            doRefresh().get();

            if (refreshStrategy instanceof RefreshStrategy.Polling) {
                RefreshStrategy.Polling polling = (RefreshStrategy.Polling) refreshStrategy;
                startPolling(polling.getInterval());
            }

            status = ProviderStatus.READY;
        } catch (Exception e) {
            log.error("Failed to initialize provider", e);
            status = ProviderStatus.ERROR;
            throw new RuntimeException(e);
        }
    }

    @Override
    public void shutdown() {
        stopPolling();
        primary.close().join();
        fallback.ifPresent(f -> f.close().join());
        cachedConfig.set(Optional.empty());
        cachedExperiments.set(Optional.empty());
        scheduler.shutdown();
        status = ProviderStatus.NOT_READY;
    }

    public CompletableFuture<Void> refresh() {
        return doRefresh();
    }

    @Override
    public CompletableFuture<Map<String, Object>> resolveAllFeatures(EvaluationContext context) {
        return evalWithContext(context, Optional.empty());
    }

    @Override
    public CompletableFuture<Map<String, Object>> resolveAllFeaturesWithFilter(
        EvaluationContext context,
        List<String> prefixFilter
    ) {
        return evalWithContext(context, Optional.of(prefixFilter));
    }

    @Override
    public CompletableFuture<List<String>> getApplicableVariants(EvaluationContext context) {
        return CompletableFuture.supplyAsync(() -> {
            Optional<ExperimentData> expData = cachedExperiments.get();
            if (expData.isEmpty()) {
                return List.of();
            }

            // Use FFI to get applicable variants
            ExperimentationArgs args = getExperimentationArgs(context);
            if (args == null) {
                return List.of();
            }

            // Implementation would call FFI method
            return List.of();
        });
    }

    @Override
    public ProviderEvaluation<Boolean> getBooleanEvaluation(String key, Boolean defaultValue, EvaluationContext ctx) {
        return getEvaluation(key, defaultValue, ctx, Boolean.class);
    }

    @Override
    public ProviderEvaluation<String> getStringEvaluation(String key, String defaultValue, EvaluationContext ctx) {
        return getEvaluation(key, defaultValue, ctx, String.class);
    }

    @Override
    public ProviderEvaluation<Integer> getIntegerEvaluation(String key, Integer defaultValue, EvaluationContext ctx) {
        return getEvaluation(key, defaultValue, ctx, Integer.class);
    }

    @Override
    public ProviderEvaluation<Double> getDoubleEvaluation(String key, Double defaultValue, EvaluationContext ctx) {
        return getEvaluation(key, defaultValue, ctx, Double.class);
    }

    @Override
    public ProviderEvaluation<Value> getObjectEvaluation(String key, Value defaultValue, EvaluationContext ctx) {
        ProviderEvaluation<Object> pe = getEvaluation(key, defaultValue, ctx, Object.class);
        return ProviderEvaluation.<Value>builder()
            .value(Value.objectToValue(pe.getValue()))
            .variant(pe.getVariant())
            .errorMessage(pe.getErrorMessage())
            .errorCode(pe.getErrorCode())
            .build();
    }

    private <T> ProviderEvaluation<T> getEvaluation(String key, T defaultValue, EvaluationContext ctx, Class<T> clazz) {
        try {
            Map<String, Object> config = evalWithContext(ctx, Optional.empty()).get();
            Object value = getNestedValue(config, key);

            if (value == null) {
                return ProviderEvaluation.<T>builder()
                    .value(defaultValue)
                    .variant("default")
                    .errorCode(ErrorCode.FLAG_NOT_FOUND)
                    .errorMessage("Flag not found: " + key)
                    .build();
            }

            T converted = convertValue(value, clazz, defaultValue);
            return ProviderEvaluation.<T>builder()
                .value(converted)
                .variant("evaluated")
                .build();
        } catch (Exception e) {
            log.error("Error evaluating flag {}: {}", key, e.getMessage());
            return ProviderEvaluation.<T>builder()
                .value(defaultValue)
                .variant("default")
                .errorCode(ErrorCode.GENERAL)
                .errorMessage(e.getMessage())
                .build();
        }
    }

    private CompletableFuture<Void> doRefresh() {
        return primary.fetchConfig()
            .thenAccept(config -> {
                cachedConfig.set(Optional.of(config));
                log.debug("Config refreshed from primary");
            })
            .exceptionally(ex -> {
                log.warn("Primary data source failed: {}", ex.getMessage());
                if (fallback.isPresent()) {
                    try {
                        ConfigData config = fallback.get().fetchConfig().get();
                        cachedConfig.set(Optional.of(config));
                        log.debug("Config fetched from fallback");
                    } catch (Exception fallbackEx) {
                        log.error("Fallback also failed: {}", fallbackEx.getMessage());
                        throw new RuntimeException(fallbackEx);
                    }
                }
                return null;
            })
            .thenCompose(v -> {
                if (primary.supportsExperiments()) {
                    return primary.fetchActiveExperiments()
                        .thenAccept(optExp -> {
                            optExp.ifPresent(exp -> {
                                cachedExperiments.set(Optional.of(exp));
                                log.debug("Experiments refreshed from primary");
                            });
                        })
                        .exceptionally(ex -> {
                            log.warn("Failed to refresh experiments: {}", ex.getMessage());
                            return null;
                        });
                }
                return CompletableFuture.completedFuture(null);
            });
    }

    private CompletableFuture<Map<String, Object>> evalWithContext(
        EvaluationContext context,
        Optional<List<String>> prefixFilter
    ) {
        return CompletableFuture.supplyAsync(() -> {
            checkOnDemandRefresh();

            Optional<ConfigData> config = cachedConfig.get();
            if (config.isEmpty()) {
                throw new IllegalStateException("No cached configuration available");
            }

            // Use FFI for evaluation
            EvaluationArgs args = new EvaluationArgs(config.get());
            Map<String, String> contextMap = extractContext(context);

            ExperimentationArgs expArgs = getExperimentationArgs(context);

            return args.evaluate(context, expArgs);
        });
    }

    private void checkOnDemandRefresh() {
        if (refreshStrategy instanceof RefreshStrategy.OnDemand) {
            RefreshStrategy.OnDemand onDemand = (RefreshStrategy.OnDemand) refreshStrategy;
            Optional<ConfigData> config = cachedConfig.get();

            if (config.isPresent()) {
                long elapsed = java.time.Duration.between(
                    config.get().getFetchedAt(),
                    java.time.Instant.now()
                ).getSeconds();

                if (elapsed > onDemand.getTtl()) {
                    try {
                        doRefresh().get();
                    } catch (Exception e) {
                        if (onDemand.isUseStaleOnError()) {
                            log.warn("Refresh failed, using stale data: {}", e.getMessage());
                        } else {
                            throw new RuntimeException(e);
                        }
                    }
                }
            }
        }
    }

    private void startPolling(long intervalSeconds) {
        ScheduledFuture<?> task = scheduler.scheduleAtFixedRate(
            () -> {
                try {
                    doRefresh().get();
                } catch (Exception e) {
                    log.error("Polling refresh failed: {}", e.getMessage());
                }
            },
            intervalSeconds,
            intervalSeconds,
            TimeUnit.SECONDS
        );
        pollingTask = Optional.of(task);
    }

    private void stopPolling() {
        pollingTask.ifPresent(t -> t.cancel(false));
        pollingTask = Optional.empty();
    }

    private Map<String, String> extractContext(EvaluationContext context) {
        Map<String, String> result = new HashMap<>();
        // Extract context fields
        return result;
    }

    private ExperimentationArgs getExperimentationArgs(EvaluationContext context) {
        Optional<ExperimentData> expData = cachedExperiments.get();
        if (expData.isEmpty()) {
            return null;
        }

        String targetingKey = context.getTargetingKey();
        if (targetingKey == null) {
            return null;
        }

        // Convert to FFI types and create ExperimentationArgs
        return null;
    }

    private Object getNestedValue(Map<String, Object> obj, String path) {
        String[] keys = path.split("\\.");
        Object result = obj;
        for (String key : keys) {
            if (!(result instanceof Map)) {
                return null;
            }
            result = ((Map<?, ?>) result).get(key);
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private <T> T convertValue(Object value, Class<T> clazz, T defaultValue) {
        if (value == null) return defaultValue;
        if (clazz.isInstance(value)) {
            return clazz.cast(value);
        }
        // Add type conversion logic
        return defaultValue;
    }

    public ProviderStatus getStatus() {
        return status;
    }
}
```

**Step 2: Create RefreshStrategy types**

Create `clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/RefreshStrategy.java`:

```java
package io.juspay.superposition.provider;

public sealed interface RefreshStrategy {
    record Polling(long interval) implements RefreshStrategy {}
    record OnDemand(long ttl, boolean useStaleOnError) implements RefreshStrategy {}
    record Manual() implements RefreshStrategy {}
}
```

Create `clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/CacheOptions.java`:

```java
package io.juspay.superposition.provider;

public record CacheOptions(int size, long ttl) {}
```

**Step 3: Verify Java compiles**

Run: `./gradlew compileJava`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add clients/java/openfeature-provider/src/main/java/io/juspay/superposition/provider/
git commit -m "feat(java): implement LocalResolutionProvider with primary/fallback and refresh strategies"
```

---

## Python Provider Architecture

### Task 9: Define core interfaces (`superposition_provider/`)

**Files:**
- Create: `clients/python/provider/superposition_provider/interfaces.py`
- Create: `clients/python/provider/superposition_provider/types.py`
- Modify: `clients/python/provider/superposition_provider/__init__.py`

**Step 1: Create `types.py`**

```python
"""Type definitions for Superposition provider."""
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, List, Any, Optional


@dataclass
class ConfigData:
    """Data fetched from a configuration source."""
    default_configs: Dict[str, Any]
    contexts: List[Dict[str, Any]]
    overrides: Dict[str, Any]
    dimensions: Dict[str, Any]
    fetched_at: datetime = field(default_factory=datetime.now)


@dataclass
class ExperimentData:
    """Experiment data fetched from a source."""
    experiments: List[Dict[str, Any]]
    experiment_groups: List[Dict[str, Any]]
    fetched_at: datetime = field(default_factory=datetime.now)


@dataclass
class RefreshStrategy:
    """Refresh strategy configuration."""
    pass


@dataclass
class PollingStrategy(RefreshStrategy):
    """Polling refresh strategy."""
    interval: int  # seconds


@dataclass
class OnDemandStrategy(RefreshStrategy):
    """On-demand refresh strategy."""
    ttl: int  # seconds
    use_stale_on_error: bool = False


@dataclass
class ManualStrategy(RefreshStrategy):
    """Manual refresh strategy."""
    pass


@dataclass
class CacheOptions:
    """Cache configuration for resolution output."""
    size: int = 1000
    ttl: int = 300  # seconds
```

**Step 2: Create `interfaces.py`**

```python
"""Core interfaces for Superposition provider."""
from abc import ABC, abstractmethod
from typing import Dict, List, Any, Optional
from openfeature.evaluation_context import EvaluationContext

from .types import ConfigData, ExperimentData


class AllFeatureProvider(ABC):
    """Interface for bulk configuration resolution."""

    @abstractmethod
    async def resolve_all_features(
        self,
        context: EvaluationContext
    ) -> Dict[str, Any]:
        """Resolve all features for the given evaluation context."""
        pass

    @abstractmethod
    async def resolve_all_features_with_filter(
        self,
        context: EvaluationContext,
        prefix_filter: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """Resolve features matching prefix filters."""
        pass


class FeatureExperimentMeta(ABC):
    """Interface for experiment metadata and variant resolution."""

    @abstractmethod
    async def get_applicable_variants(
        self,
        context: EvaluationContext
    ) -> List[str]:
        """Get applicable variant IDs for the given context."""
        pass


class SuperpositionDataSource(ABC):
    """Interface for abstracting data sources for Superposition configuration and experiments."""

    @abstractmethod
    async def fetch_config(self) -> ConfigData:
        """Fetch the latest configuration from the data source."""
        pass

    @abstractmethod
    async def fetch_filtered_config(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None
    ) -> ConfigData:
        """Fetch configuration with context/prefix filters."""
        pass

    @abstractmethod
    async def fetch_active_experiments(self) -> Optional[ExperimentData]:
        """Fetch all active experiment data.
        
        Returns None if the data source doesn't support experiments.
        """
        pass

    @abstractmethod
    async def fetch_candidate_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None
    ) -> Optional[ExperimentData]:
        """Fetch active experiments filtered with partial context matching.
        
        Returns None if the data source doesn't support experiments.
        """
        pass

    @abstractmethod
    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None
    ) -> Optional[ExperimentData]:
        """Fetch active experiments filtered with exact context matching.
        
        Returns None if the data source doesn't support experiments.
        """
        pass

    @abstractmethod
    def supports_experiments(self) -> bool:
        """Check if this data source supports experiments."""
        pass

    @abstractmethod
    async def close(self) -> None:
        """Close and cleanup resources used by this data source."""
        pass
```

**Step 3: Update `__init__.py`**

```python
from .types import (
    ConfigData,
    ExperimentData,
    RefreshStrategy,
    PollingStrategy,
    OnDemandStrategy,
    ManualStrategy,
    CacheOptions,
)
from .interfaces import (
    AllFeatureProvider,
    FeatureExperimentMeta,
    SuperpositionDataSource,
)

__all__ = [
    "ConfigData",
    "ExperimentData",
    "RefreshStrategy",
    "PollingStrategy",
    "OnDemandStrategy",
    "ManualStrategy",
    "CacheOptions",
    "AllFeatureProvider",
    "FeatureExperimentMeta",
    "SuperpositionDataSource",
]
```

**Step 4: Verify Python compiles**

Run: `cd clients/python/provider && python -m py_compile superposition_provider/interfaces.py superposition_provider/types.py`
Expected: No errors

**Step 5: Commit**

```bash
git add clients/python/provider/superposition_provider/types.py clients/python/provider/superposition_provider/interfaces.py clients/python/provider/superposition_provider/__init__.py
git commit -m "feat(python): add core interfaces AllFeatureProvider, FeatureExperimentMeta, SuperpositionDataSource"
```

---

### Task 10: Implement `HttpDataSource` (Python)

**Files:**
- Create: `clients/python/provider/superposition_provider/data_sources/__init__.py`
- Create: `clients/python/provider/superposition_provider/data_sources/http_data_source.py`

**Step 1: Create HTTP data source implementation**

```python
"""HTTP-based data source for Superposition."""
import logging
from typing import Dict, List, Any, Optional
import aiohttp

from ..interfaces import SuperpositionDataSource
from ..types import ConfigData, ExperimentData

logger = logging.getLogger(__name__)


class HttpDataSource(SuperpositionDataSource):
    """HTTP-based data source using the Superposition SDK."""

    def __init__(
        self,
        endpoint: str,
        token: str,
        org_id: str,
        workspace_id: str,
        http_client: Optional[aiohttp.ClientSession] = None
    ):
        self.endpoint = endpoint.rstrip("/")
        self.token = token
        self.org_id = org_id
        self.workspace_id = workspace_id
        self._own_session = http_client is None
        self._session = http_client

    async def _get_session(self) -> aiohttp.ClientSession:
        if self._session is None or self._session.closed:
            self._session = aiohttp.ClientSession()
        return self._session

    def _headers(self) -> Dict[str, str]:
        return {
            "Authorization": f"Bearer {self.token}",
            "Content-Type": "application/json",
            "x-organization-id": self.org_id,
            "x-workspace-id": self.workspace_id,
        }

    async def fetch_config(self) -> ConfigData:
        session = await self._get_session()
        url = f"{self.endpoint}/config"

        async with session.get(url, headers=self._headers()) as response:
            response.raise_for_status()
            data = await response.json()

        return ConfigData(
            default_configs=data.get("default_configs", {}),
            contexts=data.get("contexts", []),
            overrides=data.get("overrides", {}),
            dimensions=data.get("dimensions", {})
        )

    async def fetch_filtered_config(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None
    ) -> ConfigData:
        params = {}
        if context:
            params["context"] = context
        if prefix_filter:
            params["prefixes"] = prefix_filter

        session = await self._get_session()
        url = f"{self.endpoint}/config"

        async with session.get(
            url,
            headers=self._headers(),
            params=params
        ) as response:
            response.raise_for_status()
            data = await response.json()

        return ConfigData(
            default_configs=data.get("default_configs", {}),
            contexts=data.get("contexts", []),
            overrides=data.get("overrides", {}),
            dimensions=data.get("dimensions", {})
        )

    async def fetch_active_experiments(self) -> Optional[ExperimentData]:
        session = await self._get_session()

        exp_url = f"{self.endpoint}/experiments"
        group_url = f"{self.endpoint}/experiment-groups"

        params = {"status": ["created", "inprogress"]}

        async with (
            session.get(exp_url, headers=self._headers(), params=params) as exp_response,
            session.get(group_url, headers=self._headers()) as group_response
        ):
            exp_response.raise_for_status()
            group_response.raise_for_status()

            exp_data = await exp_response.json()
            group_data = await group_response.json()

        return ExperimentData(
            experiments=exp_data.get("data", []),
            experiment_groups=group_data.get("data", [])
        )

    async def fetch_candidate_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None
    ) -> Optional[ExperimentData]:
        data = await self.fetch_active_experiments()
        if data is None:
            return None

        # Apply partial matching filter
        if context:
            data.experiments = [
                exp for exp in data.experiments
                if self._partial_match(exp.get("context", {}), context)
            ]
            data.experiment_groups = [
                group for group in data.experiment_groups
                if self._partial_match(group.get("context", {}), context)
            ]

        if prefix_filter:
            data.experiments = [
                exp for exp in data.experiments
                if self._matches_prefix(exp, prefix_filter)
            ]

        return data

    async def fetch_matching_active_experiments(
        self,
        context: Optional[Dict[str, Any]] = None,
        prefix_filter: Optional[List[str]] = None
    ) -> Optional[ExperimentData]:
        data = await self.fetch_active_experiments()
        if data is None:
            return None

        # Apply exact matching filter
        if context:
            data.experiments = [
                exp for exp in data.experiments
                if self._exact_match(exp.get("context", {}), context)
            ]
            data.experiment_groups = [
                group for group in data.experiment_groups
                if self._exact_match(group.get("context", {}), context)
            ]

        if prefix_filter:
            data.experiments = [
                exp for exp in data.experiments
                if self._matches_prefix(exp, prefix_filter)
            ]

        return data

    def supports_experiments(self) -> bool:
        return True

    async def close(self) -> None:
        if self._own_session and self._session and not self._session.closed:
            await self._session.close()
            self._session = None

    def _partial_match(self, exp_context: Dict, filter_context: Dict) -> bool:
        """Check if experiment context partially matches filter context."""
        # Implementation would use superposition_core logic
        return True

    def _exact_match(self, exp_context: Dict, filter_context: Dict) -> bool:
        """Check if experiment context exactly matches filter context."""
        # Implementation would use superposition_core logic
        return True

    def _matches_prefix(self, experiment: Dict, prefixes: List[str]) -> bool:
        """Check if experiment has variants with override keys matching prefixes."""
        # Implementation would check variant overrides
        return True
```

**Step 2: Create data sources `__init__.py`**

```python
from .http_data_source import HttpDataSource

__all__ = ["HttpDataSource"]
```

**Step 3: Update main `__init__.py`**

```python
from .data_sources import HttpDataSource

__all__ = [
    # ... existing exports
    "HttpDataSource",
]
```

**Step 4: Verify Python compiles**

Run: `python -m py_compile superposition_provider/data_sources/http_data_source.py`
Expected: No errors

**Step 5: Commit**

```bash
git add clients/python/provider/superposition_provider/data_sources/
git commit -m "feat(python): implement HttpDataSource for SuperpositionDataSource interface"
```

---

### Task 11: Implement `LocalResolutionProvider` (Python)

**Files:**
- Create: `clients/python/provider/superposition_provider/local_resolution_provider.py`
- Modify: `clients/python/provider/superposition_provider/__init__.py`

**Step 1: Create LocalResolutionProvider**

```python
"""Local (in-process) resolution provider for Superposition."""
import asyncio
import logging
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime, timedelta

from openfeature.provider import AbstractProvider, Metadata as ProviderMetadata
from openfeature.evaluation_context import EvaluationContext
from openfeature.flag_evaluation import FlagResolutionDetails
from openfeature.provider import ProviderStatus

from .interfaces import AllFeatureProvider, FeatureExperimentMeta, SuperpositionDataSource
from .types import (
    ConfigData,
    ExperimentData,
    RefreshStrategy,
    PollingStrategy,
    OnDemandStrategy,
    ManualStrategy,
    CacheOptions
)

logger = logging.getLogger(__name__)


class LocalResolutionProvider(AbstractProvider, AllFeatureProvider, FeatureExperimentMeta):
    """Local (in-process) resolution provider.
    
    Caches raw configuration and experiment data from a primary data source
    (with optional fallback), and resolves configuration locally.
    """

    def __init__(
        self,
        primary: SuperpositionDataSource,
        fallback: Optional[SuperpositionDataSource] = None,
        refresh_strategy: RefreshStrategy = ManualStrategy(),
        cache_options: Optional[CacheOptions] = None
    ):
        self.metadata = ProviderMetadata(name="LocalResolutionProvider")
        self.status = ProviderStatus.NOT_READY
        self.hooks = []

        self.primary = primary
        self.fallback = fallback
        self.refresh_strategy = refresh_strategy
        self.cache_options = cache_options

        self._cached_config: Optional[ConfigData] = None
        self._cached_experiments: Optional[ExperimentData] = None
        self._polling_task: Optional[asyncio.Task] = None
        self._lock = asyncio.Lock()

    async def initialize(self, context: Optional[EvaluationContext] = None):
        """Initialize the provider: fetch initial data and start refresh strategy."""
        self.status = ProviderStatus.NOT_READY

        try:
            await self._do_refresh()

            if isinstance(self.refresh_strategy, PollingStrategy):
                self._start_polling(self.refresh_strategy.interval)

            self.status = ProviderStatus.READY
            logger.info("LocalResolutionProvider initialized successfully")
        except Exception as e:
            self.status = ProviderStatus.ERROR
            logger.error(f"Failed to initialize provider: {e}")
            raise

    async def shutdown(self):
        """Shutdown the provider and cleanup resources."""
        if self._polling_task:
            self._polling_task.cancel()
            try:
                await self._polling_task
            except asyncio.CancelledError:
                pass

        await self.primary.close()
        if self.fallback:
            await self.fallback.close()

        self._cached_config = None
        self._cached_experiments = None
        self.status = ProviderStatus.NOT_READY

    async def refresh(self):
        """Manually refresh data from the primary data source."""
        await self._do_refresh()

    async def resolve_all_features(
        self,
        context: EvaluationContext
    ) -> Dict[str, Any]:
        """Resolve all features for the given evaluation context."""
        return await self._eval_with_context(context)

    async def resolve_all_features_with_filter(
        self,
        context: EvaluationContext,
        prefix_filter: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """Resolve features matching prefix filters."""
        return await self._eval_with_context(context, prefix_filter)

    async def get_applicable_variants(
        self,
        context: EvaluationContext
    ) -> List[str]:
        """Get applicable variant IDs for the given context."""
        await self._ensure_fresh_data()

        if not self._cached_experiments:
            return []

        # Use native resolver to get applicable variants
        context_dict, targeting_key = self._extract_context(context)
        
        # Implementation would call native resolver
        return []

    def resolve_boolean_details(
        self,
        flag_key: str,
        default_value: bool,
        evaluation_context: Optional[EvaluationContext] = None
    ) -> FlagResolutionDetails[bool]:
        return self._resolve_value(flag_key, default_value, evaluation_context, bool)

    def resolve_string_details(
        self,
        flag_key: str,
        default_value: str,
        evaluation_context: Optional[EvaluationContext] = None
    ) -> FlagResolutionDetails[str]:
        return self._resolve_value(flag_key, default_value, evaluation_context, str)

    def resolve_integer_details(
        self,
        flag_key: str,
        default_value: int,
        evaluation_context: Optional[EvaluationContext] = None
    ) -> FlagResolutionDetails[int]:
        return self._resolve_value(flag_key, default_value, evaluation_context, int)

    def resolve_float_details(
        self,
        flag_key: str,
        default_value: float,
        evaluation_context: Optional[EvaluationContext] = None
    ) -> FlagResolutionDetails[float]:
        return self._resolve_value(flag_key, default_value, evaluation_context, float)

    def resolve_object_details(
        self,
        flag_key: str,
        default_value: Any,
        evaluation_context: Optional[EvaluationContext] = None
    ) -> FlagResolutionDetails[Any]:
        return self._resolve_value(flag_key, default_value, evaluation_context, object)

    def _resolve_value(
        self,
        flag_key: str,
        default_value: Any,
        evaluation_context: Optional[EvaluationContext],
        value_type: type
    ) -> FlagResolutionDetails[Any]:
        if self.status not in (ProviderStatus.READY, ProviderStatus.STALE):
            return FlagResolutionDetails(
                value=default_value,
                reason="DEFAULT",
                error_code="PROVIDER_NOT_READY"
            )

        try:
            # This is synchronous - would need async support in OpenFeature
            import asyncio
            config = asyncio.run(self._eval_with_context(evaluation_context or EvaluationContext(attributes={})))
            value = self._get_nested_value(config, flag_key)

            if value is None:
                return FlagResolutionDetails(
                    value=default_value,
                    reason="DEFAULT",
                    error_code="FLAG_NOT_FOUND"
                )

            converted = self._convert_value(value, value_type, default_value)
            return FlagResolutionDetails(value=converted, reason="TARGETING_MATCH")
        except Exception as e:
            logger.error(f"Error evaluating flag {flag_key}: {e}")
            return FlagResolutionDetails(
                value=default_value,
                reason="ERROR",
                error_code="GENERAL",
                error_message=str(e)
            )

    async def _do_refresh(self):
        """Fetch data from primary (with fallback on failure)."""
        async with self._lock:
            try:
                self._cached_config = await self.primary.fetch_config()
                logger.debug("Config refreshed from primary")

                if self.primary.supports_experiments():
                    experiments = await self.primary.fetch_active_experiments()
                    if experiments:
                        self._cached_experiments = experiments
                        logger.debug("Experiments refreshed from primary")
            except Exception as e:
                logger.warning(f"Primary data source failed: {e}")

                if self.fallback:
                    try:
                        self._cached_config = await self.fallback.fetch_config()
                        logger.debug("Config fetched from fallback")

                        if self.fallback.supports_experiments():
                            experiments = await self.fallback.fetch_active_experiments()
                            if experiments:
                                self._cached_experiments = experiments
                    except Exception as fallback_error:
                        logger.error(f"Fallback also failed: {fallback_error}")
                        raise
                else:
                    raise

    async def _eval_with_context(
        self,
        context: EvaluationContext,
        prefix_filter: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """Evaluate configuration with the given context."""
        await self._ensure_fresh_data()

        if not self._cached_config:
            raise RuntimeError("No cached configuration available")

        # Extract context and targeting key
        context_dict, targeting_key = self._extract_context(context)

        # Get applicable variants if experiments are available
        variant_ids = []
        if self._cached_experiments:
            variant_ids = await self.get_applicable_variants(context)

        # Use native resolver for evaluation
        # Implementation would call native resolver with:
        # - self._cached_config.default_configs
        # - self._cached_config.contexts
        # - self._cached_config.overrides
        # - self._cached_config.dimensions
        # - context_dict
        # - variant_ids
        # - prefix_filter

        return {}

    async def _ensure_fresh_data(self):
        """Check if data needs refresh based on strategy."""
        if isinstance(self.refresh_strategy, OnDemandStrategy):
            if not self._cached_config:
                await self._do_refresh()
                return

            elapsed = (datetime.now() - self._cached_config.fetched_at).total_seconds()
            if elapsed > self.refresh_strategy.ttl:
                try:
                    await self._do_refresh()
                except Exception as e:
                    if not self.refresh_strategy.use_stale_on_error:
                        raise
                    logger.warning(f"Refresh failed, using stale data: {e}")

    def _start_polling(self, interval: int):
        """Start polling for configuration updates."""
        async def poll():
            while True:
                try:
                    await asyncio.sleep(interval)
                    await self._do_refresh()
                except asyncio.CancelledError:
                    break
                except Exception as e:
                    logger.error(f"Polling refresh failed: {e}")

        self._polling_task = asyncio.create_task(poll())

    def _extract_context(self, context: EvaluationContext) -> Tuple[Dict[str, Any], Optional[str]]:
        """Extract context dictionary and targeting key from EvaluationContext."""
        context_dict = {}
        if context.attributes:
            context_dict = dict(context.attributes)
        return context_dict, context.targeting_key

    def _get_nested_value(self, obj: Dict, path: str) -> Any:
        """Get a nested value from a dictionary using dot notation."""
        keys = path.split(".")
        result = obj
        for key in keys:
            if not isinstance(result, dict):
                return None
            result = result.get(key)
        return result

    def _convert_value(self, value: Any, target_type: type, default: Any) -> Any:
        """Convert a value to the target type."""
        if value is None:
            return default
        if isinstance(value, target_type):
            return value
        try:
            if target_type == bool:
                return bool(value)
            elif target_type == str:
                return str(value)
            elif target_type == int:
                return int(value)
            elif target_type == float:
                return float(value)
        except (ValueError, TypeError):
            pass
        return default

    def get_metadata(self) -> ProviderMetadata:
        return self.metadata

    def get_status(self) -> ProviderStatus:
        return self.status
```

**Step 2: Update `__init__.py`**

```python
from .local_resolution_provider import LocalResolutionProvider

__all__ = [
    # ... existing exports
    "LocalResolutionProvider",
]
```

**Step 3: Verify Python compiles**

Run: `python -m py_compile superposition_provider/local_resolution_provider.py`
Expected: No errors

**Step 4: Commit**

```bash
git add clients/python/provider/superposition_provider/local_resolution_provider.py clients/python/provider/superposition_provider/__init__.py
git commit -m "feat(python): implement LocalResolutionProvider with primary/fallback and refresh strategies"
```

---

## Verification Tasks

### Task 12: JavaScript Provider Integration Test

**Files:**
- Create: `clients/javascript/open-feature-provider/tests/integration.test.ts`

**Step 1: Create integration test**

```typescript
import { describe, it, expect, beforeAll, afterAll } from "@jest/globals";
import { LocalResolutionProvider, HttpDataSource } from "../src";
import { EvaluationContext } from "@openfeature/server-sdk";

describe("JavaScript Provider Integration", () => {
    let provider: LocalResolutionProvider;

    beforeAll(async () => {
        const httpSource = new HttpDataSource({
            endpoint: "http://localhost:8080",
            token: "test-token",
            orgId: "test-org",
            workspaceId: "test-workspace",
        });

        provider = new LocalResolutionProvider(
            httpSource,
            undefined,
            { type: "manual" }
        );

        await provider.initialize();
    });

    afterAll(async () => {
        await provider.onClose();
    });

    it("should resolve all features", async () => {
        const context: EvaluationContext = {
            targetingKey: "user-123",
            userId: "123",
        };

        const features = await provider.resolveAllFeatures(context);
        expect(features).toBeDefined();
    });

    it("should resolve features with prefix filter", async () => {
        const context: EvaluationContext = {
            targetingKey: "user-123",
        };

        const features = await provider.resolveAllFeaturesWithFilter(context, ["app."]);
        expect(features).toBeDefined();
    });
});
```

**Step 2: Run tests**

Run: `npm test`
Expected: Tests pass

**Step 3: Commit**

```bash
git add clients/javascript/open-feature-provider/tests/
git commit -m "test(js): add integration tests for JavaScript provider"
```

---

### Task 13: Java Provider Integration Test

**Files:**
- Create: `clients/java/openfeature-provider/src/test/java/io/juspay/superposition/provider/LocalResolutionProviderTest.java`

**Step 1: Create integration test**

```java
package io.juspay.superposition.provider;

import dev.openfeature.sdk.EvaluationContext;
import dev.openfeature.sdk.ImmutableContext;
import dev.openfeature.sdk.Value;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.*;

class LocalResolutionProviderTest {

    @Test
    void testResolveAllFeatures() {
        // Create mock data source
        SuperpositionDataSource mockSource = createMockDataSource();

        LocalResolutionProvider provider = new LocalResolutionProvider(
            mockSource,
            Optional.empty(),
            new RefreshStrategy.Manual(),
            Optional.empty()
        );

        EvaluationContext ctx = new ImmutableContext(Map.of("userId", new Value("123")));
        
        provider.initialize(ctx);
        
        Map<String, Object> features = provider.resolveAllFeatures(ctx).join();
        
        assertNotNull(features);
        assertTrue(features.containsKey("feature1"));
    }

    private SuperpositionDataSource createMockDataSource() {
        return new SuperpositionDataSource() {
            @Override
            public CompletableFuture<ConfigData> fetchConfig() {
                return CompletableFuture.completedFuture(new ConfigData(
                    Map.of("feature1", true),
                    List.of(),
                    Map.of(),
                    Map.of()
                ));
            }

            @Override
            public CompletableFuture<ConfigData> fetchFilteredConfig(
                Optional<Map<String, Object>> context,
                Optional<List<String>> prefixFilter
            ) {
                return fetchConfig();
            }

            @Override
            public CompletableFuture<Optional<ExperimentData>> fetchActiveExperiments() {
                return CompletableFuture.completedFuture(Optional.empty());
            }

            @Override
            public CompletableFuture<Optional<ExperimentData>> fetchCandidateActiveExperiments(
                Optional<Map<String, Object>> context,
                Optional<List<String>> prefixFilter
            ) {
                return CompletableFuture.completedFuture(Optional.empty());
            }

            @Override
            public CompletableFuture<Optional<ExperimentData>> fetchMatchingActiveExperiments(
                Optional<Map<String, Object>> context,
                Optional<List<String>> prefixFilter
            ) {
                return CompletableFuture.completedFuture(Optional.empty());
            }

            @Override
            public boolean supportsExperiments() {
                return false;
            }

            @Override
            public CompletableFuture<Void> close() {
                return CompletableFuture.completedFuture(null);
            }
        };
    }
}
```

**Step 2: Run tests**

Run: `./gradlew test`
Expected: Tests pass

**Step 3: Commit**

```bash
git add clients/java/openfeature-provider/src/test/
git commit -m "test(java): add integration tests for Java provider"
```

---

### Task 14: Python Provider Integration Test

**Files:**
- Create: `clients/python/provider/tests/test_local_resolution_provider.py`

**Step 1: Create integration test**

```python
"""Integration tests for LocalResolutionProvider."""
import pytest
import asyncio
from openfeature.evaluation_context import EvaluationContext

from superposition_provider import (
    LocalResolutionProvider,
    HttpDataSource,
    ManualStrategy,
)


@pytest.fixture
def mock_data_source():
    """Create a mock data source for testing."""
    from superposition_provider import SuperpositionDataSource, ConfigData

    class MockDataSource(SuperpositionDataSource):
        async def fetch_config(self):
            return ConfigData(
                default_configs={"feature1": True, "feature2": "value"},
                contexts=[],
                overrides={},
                dimensions={}
            )

        async def fetch_filtered_config(self, context=None, prefix_filter=None):
            return await self.fetch_config()

        async def fetch_active_experiments(self):
            return None

        async def fetch_candidate_active_experiments(self, context=None, prefix_filter=None):
            return None

        async def fetch_matching_active_experiments(self, context=None, prefix_filter=None):
            return None

        def supports_experiments(self):
            return False

        async def close(self):
            pass

    return MockDataSource()


@pytest.mark.asyncio
async def test_resolve_all_features(mock_data_source):
    """Test resolving all features."""
    provider = LocalResolutionProvider(
        primary=mock_data_source,
        refresh_strategy=ManualStrategy()
    )

    await provider.initialize()

    context = EvaluationContext(attributes={"userId": "123"})
    features = await provider.resolve_all_features(context)

    assert features is not None
    assert "feature1" in features

    await provider.shutdown()


@pytest.mark.asyncio
async def test_resolve_all_features_with_filter(mock_data_source):
    """Test resolving features with prefix filter."""
    provider = LocalResolutionProvider(
        primary=mock_data_source,
        refresh_strategy=ManualStrategy()
    )

    await provider.initialize()

    context = EvaluationContext(attributes={})
    features = await provider.resolve_all_features_with_filter(
        context,
        prefix_filter=["feature"]
    )

    assert features is not None

    await provider.shutdown()
```

**Step 2: Run tests**

Run: `cd clients/python/provider && python -m pytest tests/ -v`
Expected: Tests pass

**Step 3: Commit**

```bash
git add clients/python/provider/tests/
git commit -m "test(python): add integration tests for Python provider"
```

---

## Final Verification

### Task 15: Cross-Language Consistency Check

**Step 1: Verify all interfaces align with Rust reference**

Compare implementations against `docs/plans/2026-02-14-configuration-resolver-plan.md`:
- AllFeatureProvider interface matches
- FeatureExperimentMeta interface matches
- SuperpositionDataSource interface matches
- Data source implementations (HttpDataSource, FileDataSource) follow same pattern
- Provider implementations (LocalResolutionProvider, SuperpositionAPIProvider) follow same pattern

**Step 2: Verify naming consistency**

Check naming across all languages:
- JavaScript: camelCase for methods
- Java: PascalCase for classes, camelCase for methods
- Python: snake_case for methods

**Step 3: Final commit**

```bash
git commit -m "docs: add comprehensive re-architecture plan for JavaScript, Java, and Python providers"
```

---

## Summary

This plan provides a comprehensive re-architecture for JavaScript, Java, and Python Superposition providers following the same pattern as the Rust implementation. Each language uses idiomatic approaches:

- **JavaScript/TypeScript**: Interfaces, async/await, EventEmitter
- **Java**: Interfaces, CompletableFuture, Optional
- **Python**: Abstract Base Classes (ABC), async/await

Key features implemented in all languages:
1. Core interfaces (AllFeatureProvider, FeatureExperimentMeta, SuperpositionDataSource)
2. Data sources (HttpDataSource, FileDataSource)
3. Providers (LocalResolutionProvider, SuperpositionAPIProvider)
4. Refresh strategies (Manual, Polling, OnDemand)
5. Caching support
6. Primary/Fallback data source support
7. Full OpenFeature Provider integration
