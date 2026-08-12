/**
 * LocalResolutionProvider — local in-process evaluation with caching and configurable refresh.
 *
 * Mirrors Rust/Python/Java `LocalResolutionProvider`:
 * - primary + fallback data sources
 * - refresh strategies (Polling / OnDemand / Watch / Manual)
 * - the OnDemand TTL driven off a local "checked-at" clock (advanced on every check, including a 304),
 *   not off the server's last-modified — so a stable config does not fire a fetch on every evaluation
 * - STALE state + events when a refresh fails, cleared when it recovers
 * - single-shot initialization (a live provider refuses re-init)
 * - `WeakRef` in the polling/watch loops and a `FinalizationRegistry` to free the native cache
 */

import {
    EvaluationContext,
    Provider,
    ProviderEvents,
    ProviderMetadata,
    ProviderStatus,
} from "@openfeature/server-sdk";

import { NativeResolver, ProviderCache } from "superposition-bindings";

import { AllFeatureProvider, FeatureExperimentMeta } from "./interfaces";
import { SuperpositionError } from "./errors";
import {
    ConfigData,
    ExperimentData,
    FetchResponse,
    SuperpositionDataSource,
} from "./data-source";
import {
    OnDemandStrategy,
    RefreshStrategy,
    defaultOnDemandStrategy,
} from "./options";

/** Extract a human-readable message from an unknown thrown value. */
function errorMessage(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

/**
 * Frees the native cache handle if a provider is dropped without `shutdown()`. Deferred to
 * `setImmediate` so the free never runs inside GC. Mirrors the legacy client's registry.
 */
const cacheRegistry = new FinalizationRegistry<ProviderCache>((cache) => {
    setImmediate(() => {
        try {
            cache.free();
        } catch {
            // best-effort; the process is likely tearing down
        }
    });
});

export class LocalResolutionProvider
    extends AllFeatureProvider
    implements Provider, FeatureExperimentMeta, SuperpositionDataSource
{
    readonly metadata: ProviderMetadata = { name: "LocalResolutionProvider" };
    status: ProviderStatus = ProviderStatus.NOT_READY;

    private readonly refreshStrategy: RefreshStrategy;
    private readonly resolver = new NativeResolver();

    private globalContext: EvaluationContext = {};
    private cachedConfig: ConfigData | null = null;
    private cachedExperiments: ExperimentData | null = null;
    private ffiCache: ProviderCache | null = null;

    // When each cache was last successfully checked against its source, by the local clock.
    // Deliberately not ConfigData.fetchedAt (the server's last-modified): driving the TTL off that
    // meant a config stable for longer than the TTL was permanently "stale", so every evaluation
    // fired a fetch and the 304 that came back left the timestamp untouched — maximum load for a
    // perfectly stable config. Advanced on every successful check, including a 304.
    private configCheckedAt: Date | null = null;
    private experimentsCheckedAt: Date | null = null;

    // Background refresh (polling/watch) control.
    private stopped = false;
    private pollTimer: ReturnType<typeof setTimeout> | null = null;
    private debounceTimer: ReturnType<typeof setTimeout> | null = null;
    private watchIterator: AsyncGenerator<string, void, unknown> | null = null;

    // Single-flight: concurrent refresh() callers (e.g. a burst of on-demand evaluations after the TTL
    // expires) share ONE in-flight refresh instead of each launching its own. Without this, N callers
    // cause N redundant re-fetches — an N× load spike on the service.
    private inFlightRefresh: Promise<void> | null = null;

    constructor(
        private readonly primarySource: SuperpositionDataSource,
        private readonly fallbackSource?: SuperpositionDataSource,
        refreshStrategy?: RefreshStrategy,
    ) {
        super();
        // Built per instance so two providers never share one strategy object.
        this.refreshStrategy = refreshStrategy ?? defaultOnDemandStrategy();
    }

    // --- Lifecycle ---

    async initialize(context?: EvaluationContext): Promise<void> {
        // Single-shot: a provider is initialized once and then served. Re-initializing a live
        // provider would orphan the running polling/watch loop, so it is refused.
        if (
            this.status === ProviderStatus.READY ||
            this.status === ProviderStatus.STALE
        ) {
            console.warn(
                "LocalResolutionProvider already initialized; ignoring initialize(). " +
                    "Providers are single-shot — build a new instance.",
            );
            return;
        }

        try {
            this.status = ProviderStatus.NOT_READY;
            this.globalContext = context ?? {};

            this.ffiCache = this.resolver.createProviderCache();
            cacheRegistry.register(this, this.ffiCache, this);

            // Fetch initial config from primary, fall back if needed. A fetch error triggers the
            // fallback; init fails if the fallback errors too — same shape as Rust.
            let configData: ConfigData | null;
            try {
                const response =
                    await this.primarySource.fetchConfig(undefined);
                configData = response.getData();
            } catch (primaryError) {
                console.warn(
                    "LocalResolutionProvider: primary config fetch failed:",
                    primaryError,
                );
                if (!this.fallbackSource) {
                    throw SuperpositionError.configError(
                        `Primary config fetch failed and no fallback configured: ${errorMessage(primaryError)}`,
                    );
                }
                try {
                    const response =
                        await this.fallbackSource.fetchConfig(undefined);
                    configData = response.getData();
                } catch (fallbackError) {
                    throw SuperpositionError.configError(
                        "Both primary and fallback config fetch failed. " +
                            `Primary: ${errorMessage(primaryError)}. Fallback: ${errorMessage(fallbackError)}`,
                    );
                }
            }
            if (configData) {
                this.cachedConfig = configData;
                this.updateConfigFfiCache();
            }
            this.configCheckedAt = new Date();

            // Fetch initial experiments. A source that doesn't support experiments simply yields none
            // (non-fatal). If the primary *does* support them, a fetch error requires an experiment-
            // capable fallback (or init fails) — same shape as Rust.
            if (this.primarySource.supportsExperiments()) {
                let experimentData: ExperimentData | null;
                try {
                    const response =
                        await this.primarySource.fetchActiveExperiments(
                            undefined,
                        );
                    experimentData = response.getData();
                } catch (primaryError) {
                    console.warn(
                        "LocalResolutionProvider: primary experiment fetch failed:",
                        primaryError,
                    );
                    if (!this.fallbackSource?.supportsExperiments()) {
                        throw SuperpositionError.configError(
                            `Primary experiment fetch failed and no experiment-capable fallback configured: ${errorMessage(primaryError)}`,
                        );
                    }
                    try {
                        const response =
                            await this.fallbackSource.fetchActiveExperiments(
                                undefined,
                            );
                        experimentData = response.getData();
                    } catch (fallbackError) {
                        throw SuperpositionError.configError(
                            "Both primary and fallback experiment fetch failed. " +
                                `Primary: ${errorMessage(primaryError)}. Fallback: ${errorMessage(fallbackError)}`,
                        );
                    }
                }
                if (experimentData) {
                    this.cachedExperiments = experimentData;
                    this.updateExperimentsFfiCache();
                }
                this.experimentsCheckedAt = new Date();
            }

            this.startRefreshStrategy();

            this.status = ProviderStatus.READY;
            this.events.emit(ProviderEvents.Ready, {
                message: "Provider ready",
            });
        } catch (error) {
            this.status = ProviderStatus.ERROR;
            this.events.emit(ProviderEvents.Error, {
                message:
                    error instanceof Error
                        ? error.message
                        : "Initialization failed",
            });
            throw error;
        }
    }

    async onClose(): Promise<void> {
        return this.shutdown();
    }

    async shutdown(): Promise<void> {
        this.stopped = true;
        if (this.pollTimer) {
            clearTimeout(this.pollTimer);
            this.pollTimer = null;
        }
        if (this.debounceTimer) {
            clearTimeout(this.debounceTimer);
            this.debounceTimer = null;
        }

        // Close sources first: a watch source's close() ends its watch generator, which lets the
        // for-await loop in startWatch finish. Doing this before return() means return() never blocks
        // waiting on a generator parked for a file event that will not come.
        try {
            await this.primarySource.close();
        } catch (e) {
            console.warn("Error closing primary data source:", e);
        }
        if (this.fallbackSource) {
            try {
                await this.fallbackSource.close();
            } catch (e) {
                console.warn("Error closing fallback data source:", e);
            }
        }

        if (this.watchIterator) {
            // The source close above already ends the generator; this is a belt-and-suspenders that
            // also runs its finally, and is safe (a done generator's return() resolves immediately).
            try {
                await this.watchIterator.return();
            } catch {
                // ignore
            }
            this.watchIterator = null;
        }

        if (this.ffiCache) {
            cacheRegistry.unregister(this);
            try {
                this.ffiCache.free();
            } catch {
                // best-effort
            }
            this.ffiCache = null;
        }

        this.cachedConfig = null;
        this.cachedExperiments = null;
        this.configCheckedAt = null;
        this.experimentsCheckedAt = null;
        this.globalContext = {};
        this.status = ProviderStatus.NOT_READY;
    }

    // --- Resolution ---

    async resolveAllFeaturesWithFilter(
        context: EvaluationContext,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
    ): Promise<Record<string, any>> {
        await this.ensureFreshData();

        if (!this.ffiCache || !this.cachedConfig) {
            throw SuperpositionError.providerError(
                "Provider not initialized: no cached config available",
            );
        }

        const [targetingKey, queryData] = this.mergeContexts(context);
        // The binding returns already-parsed, real typed values, keyed by flag.
        return this.ffiCache.evalConfig(
            queryData,
            "merge",
            prefixFilter,
            excludePrefixFilter,
            targetingKey,
        );
    }

    async getApplicableVariants(
        context: EvaluationContext,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
    ): Promise<string[]> {
        await this.ensureFreshData();

        if (!this.ffiCache || !this.cachedExperiments) {
            // No experiments cached means nothing can apply — not an error.
            return [];
        }

        const [targetingKey, queryData] = this.mergeContexts(context);

        // An absent targeting key is handed to the core as "" (which it buckets as "matches no
        // experiments") rather than short-circuited here, so the core decides which context-only
        // experiments apply — consistently with the remote provider. The cache already holds the
        // experiments (initExperiments) and dimensions (initConfig), so variant selection goes through
        // the cache method rather than the standalone resolver call.
        return this.ffiCache.getApplicableVariants(
            queryData,
            prefixFilter,
            excludePrefixFilter,
            targetingKey ?? "",
        );
    }

    /** Manually refresh both config and experiments (drives the Manual strategy). */
    async refresh(): Promise<void> {
        // Single-flight: start a refresh only if none is running; otherwise join the in-flight one so a
        // burst of callers collapses to a single fetch. The event loop is single-threaded, so this
        // check-and-set is atomic. The shared promise owns the timeout and records the outcome once
        // (see runOneRefresh); it self-clears the slot on settle so the next caller starts fresh — even
        // if the underlying fetch is still hanging in the background (JS cannot cancel it).
        if (this.inFlightRefresh === null) {
            this.inFlightRefresh = this.runOneRefresh().finally(() => {
                this.inFlightRefresh = null;
            });
        }
        await this.inFlightRefresh;
    }

    /**
     * The single coalesced refresh: enforces the strategy timeout and records the outcome (READY/STALE)
     * once, so the state transition and its event fire exactly once per real refresh regardless of how
     * many callers joined it via {@link refresh}.
     */
    private async runOneRefresh(): Promise<void> {
        let succeeded = false;
        try {
            const timeout = this.refreshTimeoutMs();
            if (timeout === undefined) {
                await this.refreshOnce();
            } else {
                await this.withTimeout(this.refreshOnce(), timeout);
            }
            succeeded = true;
        } finally {
            this.recordRefreshOutcome(succeeded);
        }
    }

    // --- Refresh internals ---

    private async refreshOnce(): Promise<void> {
        // Mirrors Rust's refresh_once (inline, no per-source helpers): refresh config and experiments
        // concurrently; a 304 just restarts the TTL clock, a failure keeps the cache but is surfaced
        // so refresh() marks the provider STALE. Config failure takes priority in what is surfaced.
        const refreshConfig = async (): Promise<void> => {
            const response = await this.primarySource.fetchConfig(
                this.cachedConfig?.fetchedAt,
            );
            // New data or a 304 — both confirm the cache is current, so the TTL clock restarts.
            this.configCheckedAt = new Date();
            const data = response.getData();
            if (data) {
                this.cachedConfig = data;
                this.updateConfigFfiCache();
            }
        };
        const refreshExperiments = async (): Promise<void> => {
            if (!this.primarySource.supportsExperiments()) {
                return;
            }
            const response = await this.primarySource.fetchActiveExperiments(
                this.cachedExperiments?.fetchedAt,
            );
            this.experimentsCheckedAt = new Date();
            const data = response.getData();
            if (data) {
                this.cachedExperiments = data;
                this.updateExperimentsFfiCache();
            }
        };

        // Run both to completion (the cache is never overwritten on failure), then surface the config
        // error first (like Rust), else the experiment error — either marks the provider STALE.
        const [configResult, expResult] = await Promise.allSettled([
            refreshConfig(),
            refreshExperiments(),
        ]);
        if (configResult.status === "rejected") {
            console.warn(
                "LocalResolutionProvider: config refresh failed, keeping last known good:",
                configResult.reason,
            );
            throw configResult.reason;
        }
        if (expResult.status === "rejected") {
            console.warn(
                "LocalResolutionProvider: experiment refresh failed, keeping last known good:",
                expResult.reason,
            );
            throw expResult.reason;
        }
    }

    /**
     * Bound a refresh by the strategy's timeout. On timeout the last known good data stays in place
     * and the caller sees a REFRESH_ERROR (which marks the provider STALE).
     */
    private withTimeout<T>(promise: Promise<T>, timeoutMs: number): Promise<T> {
        return new Promise<T>((resolve, reject) => {
            const timer = setTimeout(() => {
                reject(
                    SuperpositionError.refreshError(
                        `Refresh timed out after ${timeoutMs}ms`,
                    ),
                );
            }, timeoutMs);
            promise.then(
                (value) => {
                    clearTimeout(timer);
                    resolve(value);
                },
                (error) => {
                    clearTimeout(timer);
                    reject(error);
                },
            );
        });
    }

    /**
     * The timeout the configured strategy puts on a single refresh, or undefined (unbounded) for
     * Watch/Manual. The switch is exhaustive over the strategy union (the `never` assertion turns
     * a new `kind` into a compile error rather than a silent fall-through).
     */
    private refreshTimeoutMs(): number | undefined {
        switch (this.refreshStrategy.kind) {
            case "polling":
            case "onDemand":
                return this.refreshStrategy.timeoutMilliseconds;
            case "watch":
            case "manual":
                return undefined;
            default: {
                const _exhaustive: never = this.refreshStrategy;
                return _exhaustive;
            }
        }
    }

    /**
     * Mark the provider STALE while a failed refresh leaves the cache frozen, and clear it when a
     * later refresh recovers. Only meaningful from READY: a failure during init is an ERROR, and a
     * shut-down provider stays NOT_READY. The event matters as much as the status: OpenFeature keeps
     * its own copy of provider status and only sees ours if we emit.
     */
    private recordRefreshOutcome(succeeded: boolean): void {
        if (succeeded) {
            if (this.status === ProviderStatus.STALE) {
                this.status = ProviderStatus.READY;
                this.events.emit(ProviderEvents.Ready, {
                    message: "Refresh recovered; flags are current again",
                });
            }
        } else if (this.status === ProviderStatus.READY) {
            this.status = ProviderStatus.STALE;
            this.events.emit(ProviderEvents.Stale, {
                message: "Refresh failed; serving the last known good config",
            });
        }
    }

    /** For OnDemand, refresh when a cache was last checked longer ago than the TTL. */
    private async ensureFreshData(): Promise<void> {
        if (this.refreshStrategy.kind !== "onDemand") {
            return;
        }
        const strategy: OnDemandStrategy = this.refreshStrategy;
        const now = Date.now();
        const elapsed = (checkedAt: Date | null): boolean =>
            checkedAt === null ||
            now - checkedAt.getTime() > strategy.ttlMilliseconds;

        const shouldRefreshConfig = elapsed(this.configCheckedAt);
        const shouldRefreshExperiments =
            this.primarySource.supportsExperiments() &&
            elapsed(this.experimentsCheckedAt);

        if (shouldRefreshConfig || shouldRefreshExperiments) {
            try {
                await this.refresh();
            } catch (error) {
                if (!strategy.useStaleOnError) {
                    throw error;
                }
                console.warn(
                    "Error refreshing on demand, serving stale data:",
                    error,
                );
            }
        }
    }

    private updateConfigFfiCache(): void {
        if (!this.ffiCache || !this.cachedConfig) {
            return;
        }
        const config = this.cachedConfig.data;
        this.ffiCache.initConfig(
            config.default_configs,
            config.contexts,
            config.overrides,
            config.dimensions,
        );
    }

    private updateExperimentsFfiCache(): void {
        if (!this.ffiCache || !this.cachedExperiments) {
            return;
        }
        const exp = this.cachedExperiments.data;
        this.ffiCache.initExperiments(exp.experiments, exp.experiment_groups);
    }

    // --- Background refresh strategies ---

    private startRefreshStrategy(): void {
        switch (this.refreshStrategy.kind) {
            case "polling":
                this.startPolling(this.refreshStrategy.intervalMilliseconds);
                break;
            case "watch":
                this.startWatch(this.refreshStrategy.debounceMs);
                break;
            case "manual":
            case "onDemand":
                // Manual: caller invokes refresh(). OnDemand: refresh on first stale access.
                break;
        }
    }

    private startPolling(intervalMs: number): void {
        // WeakRef so a provider that is dropped without shutdown() stops polling and can be collected.
        const weakSelf = new WeakRef(this);
        const tick = async (): Promise<void> => {
            const self = weakSelf.deref();
            if (!self || self.stopped) {
                return;
            }
            try {
                await self.refresh();
            } catch (e) {
                // Keep polling on failure; the last known good data stays in place.
                console.warn("Polling refresh failed:", e);
            }
            if (self.stopped) {
                return;
            }
            self.pollTimer = setTimeout(tick, intervalMs);
        };
        this.pollTimer = setTimeout(tick, intervalMs);
    }

    private startWatch(debounceMs: number): void {
        const iterator = this.primarySource.watch?.() ?? null;
        if (!iterator) {
            throw SuperpositionError.configError(
                "Watch strategy selected but data source does not support watching",
            );
        }
        this.watchIterator = iterator;

        const weakSelf = new WeakRef(this);
        void (async () => {
            try {
                for await (const _change of iterator) {
                    const self = weakSelf.deref();
                    if (!self || self.stopped) {
                        return;
                    }
                    // Coalesce a burst of rapid changes: (re)arm a single debounced refresh.
                    if (self.debounceTimer) {
                        clearTimeout(self.debounceTimer);
                    }
                    self.debounceTimer = setTimeout(() => {
                        const s = weakSelf.deref();
                        if (!s || s.stopped) {
                            return;
                        }
                        s.refresh().catch((e) =>
                            console.warn("Watch refresh failed:", e),
                        );
                    }, debounceMs);
                }
            } catch (e) {
                console.warn("Watch loop error:", e);
            }
        })();
    }

    // --- SuperpositionDataSource implementation ---
    //
    // A LocalResolutionProvider is itself a data source (mirroring Python/Java), so another provider
    // can compose it — its cached, filtered view is served through the native cache's filter methods.
    // These read the already-loaded cache; they do not hit the network, so `ifModifiedSince` is
    // ignored (the cache is refreshed by this provider's own strategy, not by these calls).

    fetchConfig(ifModifiedSince?: Date): Promise<FetchResponse<ConfigData>> {
        return this.fetchFilteredConfig(
            undefined,
            undefined,
            undefined,
            ifModifiedSince,
        );
    }

    async fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date,
    ): Promise<FetchResponse<ConfigData>> {
        if (!this.ffiCache || !this.cachedConfig) {
            throw SuperpositionError.dataSourceError(
                "No cached config available",
            );
        }
        if (ifModifiedSince !== undefined) {
            // The cache is kept fresh by this provider's own refresh strategy, so a conditional fetch
            // is meaningless here — the filtered cache view is always returned. Mirrors Python/Java.
            console.debug(
                "LocalResolutionProvider: ignoring ifModifiedSince, always returning cached data",
            );
        }
        return FetchResponse.data<ConfigData>({
            data: this.ffiCache.filterConfig(
                context,
                prefixFilter,
                excludePrefixFilter,
            ),
            fetchedAt: this.cachedConfig.fetchedAt,
        });
    }

    async fetchActiveExperiments(
        ifModifiedSince?: Date,
    ): Promise<FetchResponse<ExperimentData>> {
        if (!this.supportsExperiments()) {
            throw SuperpositionError.dataSourceError(
                "Experiments not supported by this provider",
            );
        }
        if (!this.cachedExperiments) {
            throw SuperpositionError.dataSourceError(
                "No cached experiments available",
            );
        }
        if (ifModifiedSince !== undefined) {
            console.debug(
                "LocalResolutionProvider: ignoring ifModifiedSince for experiments, always returning cached data",
            );
        }
        return FetchResponse.data<ExperimentData>(this.cachedExperiments);
    }

    async fetchCandidateActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date,
    ): Promise<FetchResponse<ExperimentData>> {
        return this.filteredExperiments(
            context,
            prefixFilter,
            excludePrefixFilter,
            ifModifiedSince,
            false,
        );
    }

    async fetchMatchingActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date,
    ): Promise<FetchResponse<ExperimentData>> {
        return this.filteredExperiments(
            context,
            prefixFilter,
            excludePrefixFilter,
            ifModifiedSince,
            true,
        );
    }

    /**
     * Shared body for candidate/matching experiment fetches: `partialApply` is `false` for candidate
     * (all satisfied experiments) and `true` for matching (context-evaluated) — the same mapping the
     * native `filter_experiment` uses across the clients.
     *
     * Goes through {@link fetchActiveExperiments} for the cached data (matching Java's
     * `filterCachedExperiments`), so the experiment-support check, the empty-cache guard and the
     * ignored-`ifModifiedSince` log all live in one place.
     */
    private async filteredExperiments(
        context: Record<string, any> | undefined,
        prefixFilter: string[] | undefined,
        excludePrefixFilter: string[] | undefined,
        ifModifiedSince: Date | undefined,
        partialApply: boolean,
    ): Promise<FetchResponse<ExperimentData>> {
        const cached = (
            await this.fetchActiveExperiments(ifModifiedSince)
        ).getData();
        if (!cached || !this.ffiCache) {
            throw SuperpositionError.dataSourceError(
                "No cached experiments available",
            );
        }
        return FetchResponse.data<ExperimentData>({
            data: this.ffiCache.filterExperiment(
                context,
                prefixFilter,
                excludePrefixFilter,
                partialApply,
            ),
            fetchedAt: cached.fetchedAt,
        });
    }

    supportsExperiments(): boolean {
        return this.primarySource.supportsExperiments();
    }

    /** A LocalResolutionProvider wraps its own sources; it is not itself a watchable source. */
    watch(): AsyncGenerator<string, void, unknown> | null {
        return null;
    }

    /** Data-source cleanup — an alias for {@link shutdown}. */
    close(): Promise<void> {
        return this.shutdown();
    }

    // --- Context merging ---

    private mergeContexts(
        context?: EvaluationContext,
    ): [string | undefined, Record<string, any>] {
        const merged: EvaluationContext = {
            ...this.globalContext,
            ...(context ?? {}),
        };
        const { targetingKey, ...attributes } = merged;
        // Raw values: the native eval parses query data as a JSON value map (not string-encoded).
        return [targetingKey, attributes as Record<string, any>];
    }
}
