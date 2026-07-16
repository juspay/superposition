package io.juspay.superposition.openfeature.provider;

import dev.openfeature.sdk.EvaluationContext;
import dev.openfeature.sdk.EventProvider;
import dev.openfeature.sdk.ImmutableContext;
import dev.openfeature.sdk.Metadata;
import dev.openfeature.sdk.ProviderEvaluation;
import dev.openfeature.sdk.ProviderEventDetails;
import dev.openfeature.sdk.ProviderState;
import dev.openfeature.sdk.Value;
import io.juspay.superposition.openfeature.EvaluationArgs;
import io.juspay.superposition.openfeature.data_source.ConfigData;
import io.juspay.superposition.openfeature.data_source.ExperimentData;
import io.juspay.superposition.openfeature.data_source.FetchResponse;
import io.juspay.superposition.openfeature.data_source.SuperpositionDataSource;
import io.juspay.superposition.openfeature.data_source.WatchStream;
import io.juspay.superposition.openfeature.error.SuperpositionError;
import io.juspay.superposition.openfeature.options.RefreshStrategy;
import io.juspay.superposition.openfeature.traits.AllFeatureProvider;
import io.juspay.superposition.openfeature.traits.FeatureExperimentMeta;
import java.lang.ref.WeakReference;
import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uniffi.superposition_client.OperationException;
import uniffi.superposition_client.ProviderCache;
import uniffi.superposition_types.Config;
import uniffi.superposition_types.MergeStrategy;

/**
 * Local resolution provider: resolves configurations in-process using cached data.
 *
 * Supports:
 * - Multiple data sources (primary + fallback)
 * - Multiple refresh strategies (Polling, OnDemand, Watch, Manual)
 * - Configuration caching with last-known-good on refresh failure
 * - Experiment variant tracking
 *
 * Cached config and experiments are held in a {@link ProviderCache}, so evaluation, filtering and
 * variant resolution all run through the same core logic the service itself uses.
 *
 * Also implements {@link SuperpositionDataSource}, so other consumers can fetch filtered data
 * from this provider's cache.
 */
public class LocalResolutionProvider extends EventProvider
        implements AllFeatureProvider, FeatureExperimentMeta, SuperpositionDataSource {

    private static final Logger log = LoggerFactory.getLogger(LocalResolutionProvider.class);

    /** How often the watch thread wakes on its own to check that the provider is still reachable. */
    private static final long LIVENESS_CHECK_MS = 5_000;

    private final SuperpositionDataSource primaryDataSource;
    private final Optional<SuperpositionDataSource> fallbackDataSource;
    private final RefreshStrategy refreshStrategy;

    /**
     * Native cache holding the resolved config and experiments.
     *
     * <p>Created once and reused: {@code initConfig}/{@code initExperiments} overwrite its contents
     * wholesale.</p>
     */
    private final ProviderCache cache = new ProviderCache();

    // Caches — updated atomically; null until first successful fetch
    private final AtomicReference<ConfigData> cachedConfigData = new AtomicReference<>(null);
    private final AtomicReference<ExperimentData> cachedExperimentData = new AtomicReference<>(null);

    /**
     * When each cache was last successfully checked against its source, by the local clock.
     *
     * <p>Deliberately not {@code ConfigData.getFetchedAt()}: for an HTTP source that is the
     * <em>server's</em> last-modified — when the config last <em>changed</em>, not when we last
     * <em>looked</em>. Driving the OnDemand TTL off it meant a config that had been stable for
     * longer than the TTL was permanently "stale", so every evaluation fired a fetch; the 304 that
     * came back left the timestamp untouched, so the next evaluation fired another. A perfectly
     * stable config produced maximum load, which is the exact opposite of the point of OnDemand.
     *
     * <p>Advanced on every successful check, <em>including</em> a 304.
     */
    private final AtomicReference<Instant> configCheckedAt = new AtomicReference<>(null);
    private final AtomicReference<Instant> experimentsCheckedAt = new AtomicReference<>(null);

    private final AtomicReference<ProviderState> state = new AtomicReference<>(ProviderState.NOT_READY);
    private volatile EvaluationContext globalContext = new ImmutableContext();

    // Background refresh
    private final ScheduledExecutorService refreshExecutor =
            Executors.newSingleThreadScheduledExecutor(r -> {
                Thread t = new Thread(r, "superposition-refresh");
                t.setDaemon(true);
                return t;
            });
    /** Runs the refresh itself, so a hung data source blocks a worker rather than the caller. */
    private final ExecutorService refreshWorker =
            Executors.newSingleThreadExecutor(r -> {
                Thread t = new Thread(r, "superposition-refresh-worker");
                t.setDaemon(true);
                return t;
            });
    private volatile ScheduledFuture<?> pollingTask = null;
    private volatile Thread watchThread = null;

    /**
     * Create a new local resolution provider.
     *
     * @param primaryDataSource  primary data source for fetching config/experiments
     * @param fallbackDataSource optional fallback if primary fails on init
     * @param refreshStrategy    strategy for updating data
     */
    public LocalResolutionProvider(
            SuperpositionDataSource primaryDataSource,
            Optional<SuperpositionDataSource> fallbackDataSource,
            RefreshStrategy refreshStrategy) {
        this.primaryDataSource = primaryDataSource;
        this.fallbackDataSource = fallbackDataSource;
        this.refreshStrategy = refreshStrategy;
        log.debug("LocalResolutionProvider created with strategy: {}",
                refreshStrategy.getClass().getSimpleName());
    }

    // ========== FeatureProvider ==========

    @Override
    public Metadata getMetadata() {
        return () -> "LocalResolutionProvider";
    }

    @Override
    public ProviderState getState() {
        return state.get();
    }

    @Override
    public void initialize(EvaluationContext context) throws Exception {
        // Single-shot: a provider is initialized once and then served. A second initialize() on a
        // live provider would strand the first strategy's still-running task and leak its native
        // cache, so it is refused. A prior *failed* attempt (ERROR) may be retried — nothing is
        // running after one, and this loop below starts clean.
        ProviderState current = state.get();
        if (current == ProviderState.READY || current == ProviderState.STALE) {
            log.warn("LocalResolutionProvider.initialize() called on an already-initialized "
                    + "provider; ignoring. Providers are single-shot — build a new instance.");
            return;
        }

        log.debug("Initializing LocalResolutionProvider...");
        state.set(ProviderState.NOT_READY);

        try {
            loadInitialConfig();
            loadInitialExperiments();
            startRefreshStrategy();
            this.globalContext = context != null ? context : new ImmutableContext();
            state.set(ProviderState.READY);
        } catch (Exception e) {
            state.set(ProviderState.ERROR);
            throw e;
        }

        log.info("LocalResolutionProvider initialized successfully");
    }

    @Override
    public void shutdown() {
        log.debug("LocalResolutionProvider shutting down...");
        if (pollingTask != null) {
            pollingTask.cancel(false);
            pollingTask = null;
        }
        if (watchThread != null) {
            watchThread.interrupt();
            watchThread = null;
        }
        refreshExecutor.shutdownNow();
        refreshWorker.shutdownNow();
        // shutdownNow() only interrupts. A refresh already inside fetchConfig() or cache.initConfig()
        // keeps using primaryDataSource and cache, so give it a bounded chance to stop before those
        // handles are closed below. Cleanup proceeds regardless if it overruns.
        awaitQuietly(refreshWorker);
        awaitQuietly(refreshExecutor);

        closeQuietly(primaryDataSource, "primary");
        fallbackDataSource.ifPresent(source -> closeQuietly(source, "fallback"));

        cachedConfigData.set(null);
        cachedExperimentData.set(null);
        configCheckedAt.set(null);
        experimentsCheckedAt.set(null);
        globalContext = new ImmutableContext();
        cache.close();
        state.set(ProviderState.NOT_READY);
        // Detaches the SDK's event listener; without it a shut-down provider keeps a reference.
        super.shutdown();
        log.info("LocalResolutionProvider shut down");
    }

    // ========== Typed evaluations ==========
    // Delegated to AllFeatureProvider, which parses the JSON-encoded values and reports
    // FLAG_NOT_FOUND / TYPE_MISMATCH the way the OpenFeature spec expects.

    @Override
    public ProviderEvaluation<Boolean> getBooleanEvaluation(
            String key, Boolean defaultValue, EvaluationContext context) {
        return resolveBool(key, context);
    }

    @Override
    public ProviderEvaluation<String> getStringEvaluation(
            String key, String defaultValue, EvaluationContext context) {
        return resolveString(key, context);
    }

    @Override
    public ProviderEvaluation<Integer> getIntegerEvaluation(
            String key, Integer defaultValue, EvaluationContext context) {
        return resolveInt(key, context);
    }

    @Override
    public ProviderEvaluation<Double> getDoubleEvaluation(
            String key, Double defaultValue, EvaluationContext context) {
        return resolveFloat(key, context);
    }

    @Override
    public ProviderEvaluation<Value> getObjectEvaluation(
            String key, Value defaultValue, EvaluationContext context) {
        return resolveStruct(key, context);
    }

    // ========== AllFeatureProvider ==========

    @Override
    public Map<String, String> resolveAllFeaturesWithFilter(
            EvaluationContext context,
            Optional<List<String>> prefixFilter) throws SuperpositionError {
        ensureFreshData();

        if (cachedConfigData.get() == null) {
            // Evaluating before a successful init, or after one that failed. Not a config error:
            // the config is not malformed, the provider is unusable.
            throw SuperpositionError.providerError(
                    "Provider not initialized: no cached config available");
        }

        EvaluationContext merged = mergeWithGlobal(context);
        try {
            return cache.evalConfig(
                    EvaluationArgs.buildQueryData(merged),
                    MergeStrategy.MERGE,
                    prefixFilter.orElse(null),
                    merged.getTargetingKey());
        } catch (OperationException e) {
            throw SuperpositionError.configError("Failed to evaluate config: " + e.getMessage(), e);
        }
    }

    // ========== FeatureExperimentMeta ==========

    @Override
    public List<String> getApplicableVariants(
            EvaluationContext context,
            Optional<List<String>> prefixFilter) throws SuperpositionError {
        ensureFreshData();

        if (cachedExperimentData.get() == null) {
            return Collections.emptyList();
        }

        EvaluationContext merged = mergeWithGlobal(context);
        String targetingKey = merged.getTargetingKey() != null ? merged.getTargetingKey() : "";
        try {
            return cache.getApplicableVariants(
                    EvaluationArgs.buildQueryData(merged), prefixFilter.orElse(null), targetingKey);
        } catch (OperationException e) {
            throw SuperpositionError.configError(
                    "Failed to resolve applicable variants: " + e.getMessage(), e);
        }
    }

    // ========== SuperpositionDataSource (serve the cache to nested consumers) ==========

    @Override
    public FetchResponse<ConfigData> fetchFilteredConfig(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince) throws SuperpositionError {
        if (ifModifiedSince.isPresent()) {
            log.debug("LocalResolutionProvider: ignoring ifModifiedSince for config, always returning cached data");
        }

        ConfigData cached = cachedConfigData.get();
        if (cached == null) {
            throw SuperpositionError.dataSourceError("No cached config available");
        }

        try {
            Config filtered = cache.filterConfig(context.orElse(null), prefixFilter.orElse(null));
            return FetchResponse.data(new ConfigData(filtered, cached.getFetchedAt()));
        } catch (OperationException e) {
            throw SuperpositionError.dataSourceError("Failed to filter config: " + e.getMessage(), e);
        }
    }

    @Override
    public FetchResponse<ExperimentData> fetchActiveExperiments(Optional<Instant> ifModifiedSince)
            throws SuperpositionError {
        requireExperimentSupport();
        if (ifModifiedSince.isPresent()) {
            log.debug("LocalResolutionProvider: ignoring ifModifiedSince for experiments, always returning cached data");
        }

        ExperimentData cached = cachedExperimentData.get();
        if (cached == null) {
            throw SuperpositionError.dataSourceError("No cached experiments available");
        }
        return FetchResponse.data(cached);
    }

    @Override
    public FetchResponse<ExperimentData> fetchCandidateActiveExperiments(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince) throws SuperpositionError {
        return filterCachedExperiments(context, prefixFilter, ifModifiedSince, false);
    }

    @Override
    public FetchResponse<ExperimentData> fetchMatchingActiveExperiments(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince) throws SuperpositionError {
        return filterCachedExperiments(context, prefixFilter, ifModifiedSince, true);
    }

    @Override
    public boolean supportsExperiments() {
        return primaryDataSource.supportsExperiments();
    }

    @Override
    public Optional<WatchStream> watch() {
        return Optional.empty();
    }

    @Override
    public void close() {
        shutdown();
    }

    // ========== Refresh ==========

    /**
     * Refresh config and experiments from the primary source, keeping the last known good data
     * on failure. Drives the Manual strategy; Polling, Watch and OnDemand call it internally.
     *
     * <p>The refresh runs on a worker thread and is bounded by the refresh strategy's timeout, so a
     * data source that hangs cannot stall the caller — an evaluation thread under OnDemand, or the
     * poller — indefinitely.
     *
     * @throws SuperpositionError if the refresh timed out, the config refresh failed, or only the
     *     experiment refresh did
     */
    public void refresh() throws SuperpositionError {
        int timeoutMs = refreshStrategy.getTimeoutMilliseconds();
        Future<SuperpositionError> pending = refreshWorker.submit(this::refreshOnce);
        boolean succeeded = false;

        try {
            SuperpositionError error =
                    timeoutMs > 0 ? pending.get(timeoutMs, TimeUnit.MILLISECONDS) : pending.get();
            if (error != null) {
                throw error;
            }
            succeeded = true;
        } catch (TimeoutException e) {
            pending.cancel(true);
            throw SuperpositionError.refreshError("Refresh timed out after " + timeoutMs + "ms");
        } catch (InterruptedException e) {
            pending.cancel(true);
            Thread.currentThread().interrupt();
            throw SuperpositionError.refreshError("Refresh interrupted", e);
        } catch (ExecutionException e) {
            // A task that dies without a cause still has to produce a usable error.
            Throwable cause = e.getCause() != null ? e.getCause() : e;
            throw SuperpositionError.refreshError("Refresh failed: " + cause.getMessage(), cause);
        } finally {
            // Every refresh path — polling, watch, on-demand and manual — funnels through here.
            recordRefreshOutcome(succeeded);
        }
    }

    /**
     * A refresh that fails while the cache is still served leaves the provider STALE: the flags are
     * frozen at their last known good values, and this is the only signal a consumer has that they
     * stopped tracking the source of truth. The next successful refresh clears it.
     *
     * <p>Only meaningful from READY. A failure during init is an ERROR — there is no good data to
     * be stale — and a provider that has been shut down stays NOT_READY.
     *
     * <p>The event matters as much as the field. {@code FeatureProviderStateManager} keeps the
     * SDK's own copy of the provider state and never calls {@link #getState()}, so without emitting
     * here, nothing reached through an OpenFeature client — {@code client.getProviderState()}, an
     * {@code onProviderStale} handler — would ever see this. Evaluation is unaffected either way:
     * the client only short-circuits on NOT_READY and FATAL.
     */
    private void recordRefreshOutcome(boolean succeeded) {
        if (succeeded) {
            if (state.compareAndSet(ProviderState.STALE, ProviderState.READY)) {
                log.info("LocalResolutionProvider: refresh recovered, no longer stale");
                emitProviderReady(ProviderEventDetails.builder()
                        .message("Refresh recovered; flags are current again")
                        .build());
            }
        } else if (state.compareAndSet(ProviderState.READY, ProviderState.STALE)) {
            log.warn("LocalResolutionProvider: refresh failed, serving stale data");
            emitProviderStale(ProviderEventDetails.builder()
                    .message("Refresh failed; serving the last known good config")
                    .build());
        }
    }

    /** @return the error that made the refresh fail, or null if config and experiments both refreshed */
    private SuperpositionError refreshOnce() {
        SuperpositionError configError = refreshConfig();
        SuperpositionError experimentError = refreshExperiments();

        if (configError != null) {
            return configError;
        }
        return experimentError;
    }

    // ========== Private: init helpers ==========

    private void loadInitialConfig() throws SuperpositionError {
        ConfigData configData;
        try {
            configData = fetchConfigFrom(primaryDataSource);
            log.info("LocalResolutionProvider: fetched config from primary source");
        } catch (SuperpositionError primaryError) {
            log.warn("LocalResolutionProvider: primary config fetch failed: {}", primaryError.getMessage());
            SuperpositionDataSource fallback = fallbackDataSource.orElseThrow(() ->
                    SuperpositionError.configError(
                            "Primary config fetch failed and no fallback configured: "
                                    + primaryError.getMessage(), primaryError));
            try {
                configData = fetchConfigFrom(fallback);
                log.info("LocalResolutionProvider: fetched config from fallback source");
            } catch (SuperpositionError fallbackError) {
                throw SuperpositionError.configError(
                        "Both primary and fallback config fetch failed. Primary: "
                                + primaryError.getMessage() + ". Fallback: " + fallbackError.getMessage(),
                        fallbackError);
            }
        }

        cacheConfig(configData);
    }

    private static ConfigData fetchConfigFrom(SuperpositionDataSource source) throws SuperpositionError {
        return source.fetchConfig(Optional.empty())
                .getData()
                .orElseThrow(() -> SuperpositionError.configError(
                        "Data source returned no config on initial fetch"));
    }

    /**
     * Mirrors the Rust provider: experiments come from the primary, falling back to the fallback
     * source. A source that does not support experiments simply yields none.
     */
    private void loadInitialExperiments() throws SuperpositionError {
        if (!primaryDataSource.supportsExperiments()) {
            return;
        }

        ExperimentData experimentData;
        try {
            experimentData = primaryDataSource.fetchActiveExperiments(Optional.empty())
                    .getData()
                    .orElse(null);
        } catch (SuperpositionError primaryError) {
            log.warn("LocalResolutionProvider: primary experiment fetch failed: {}",
                    primaryError.getMessage());
            SuperpositionDataSource fallback = fallbackDataSource.orElseThrow(() ->
                    SuperpositionError.configError(
                            "Primary experiment fetch failed and no fallback configured: "
                                    + primaryError.getMessage(), primaryError));
            if (!fallback.supportsExperiments()) {
                log.warn("LocalResolutionProvider: fallback does not support experiments");
                return;
            }
            try {
                experimentData = fallback.fetchActiveExperiments(Optional.empty())
                        .getData()
                        .orElse(null);
            } catch (SuperpositionError fallbackError) {
                throw SuperpositionError.configError(
                        "Both primary and fallback experiment fetch failed. Primary: "
                                + primaryError.getMessage() + ". Fallback: " + fallbackError.getMessage(),
                        fallbackError);
            }
        }

        if (experimentData != null) {
            cacheExperiments(experimentData);
        }
    }

    private void cacheConfig(ConfigData configData) throws SuperpositionError {
        Config config = configData.getData();
        try {
            cache.initConfig(
                    config.getDefaultConfigs(),
                    config.getContexts(),
                    config.getOverrides(),
                    config.getDimensions());
        } catch (OperationException e) {
            throw SuperpositionError.configError(
                    "Failed to initialize config cache: " + e.getMessage(), e);
        }
        cachedConfigData.set(configData);
        configCheckedAt.set(Instant.now());
        log.debug("Config cached: {}", configData);
    }

    private void cacheExperiments(ExperimentData experimentData) throws SuperpositionError {
        try {
            cache.initExperiments(
                    experimentData.getData().getExperiments(),
                    experimentData.getData().getExperimentGroups());
        } catch (OperationException e) {
            throw SuperpositionError.configError(
                    "Failed to initialize experiments cache: " + e.getMessage(), e);
        }
        cachedExperimentData.set(experimentData);
        experimentsCheckedAt.set(Instant.now());
        log.debug("Experiments cached: {}", experimentData);
    }

    // ========== Private: refresh strategies ==========

    private void startRefreshStrategy() throws SuperpositionError {
        if (refreshStrategy instanceof RefreshStrategy.Polling polling) {
            startPolling(polling);
        } else if (refreshStrategy instanceof RefreshStrategy.Watch watch) {
            startWatching(watch);
        } else if (refreshStrategy instanceof RefreshStrategy.OnDemand onDemand) {
            log.info("LocalResolutionProvider: using OnDemand strategy with ttl={}ms", onDemand.getTtlMilliseconds());
        } else {
            log.info("LocalResolutionProvider: using Manual refresh strategy");
        }
    }

    /**
     * The refresh loops must not hold the provider alive. A caller that drops the provider without
     * calling {@link #shutdown()} would otherwise leak it — and its native cache — for the life of
     * the process, since the executor's queued task and the watch thread are both GC roots. They
     * take a weak reference instead and stop once the provider is gone.
     */
    private void startPolling(RefreshStrategy.Polling polling) {
        log.info("LocalResolutionProvider: starting polling with interval={}ms", polling.getIntervalMilliseconds());

        WeakReference<LocalResolutionProvider> self = new WeakReference<>(this);
        ScheduledExecutorService scheduler = refreshExecutor;
        ExecutorService worker = refreshWorker;

        pollingTask = scheduler.scheduleWithFixedDelay(
                () -> {
                    LocalResolutionProvider provider = self.get();
                    if (provider == null) {
                        log.info("LocalResolutionProvider was garbage collected, stopping polling");
                        scheduler.shutdown();
                        worker.shutdown();
                        return;
                    }
                    provider.refreshQuietly();
                },
                polling.getIntervalMilliseconds(), polling.getIntervalMilliseconds(), TimeUnit.MILLISECONDS);
    }

    private void startWatching(RefreshStrategy.Watch watch) throws SuperpositionError {
        WatchStream stream = primaryDataSource.watch().orElseThrow(() ->
                SuperpositionError.configError(
                        "Watch strategy selected but data source does not support watching"));

        log.info("LocalResolutionProvider: starting watch with debounce={}ms", watch.getDebounceMs());

        WeakReference<LocalResolutionProvider> self = new WeakReference<>(this);
        int debounceMs = watch.getDebounceMs();

        watchThread = new Thread(() -> {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    // Wait for a change, but wake periodically even when nothing happens, so a
                    // dropped provider can be noticed instead of being pinned by this thread.
                    if (!stream.tryGetNextEvent(LIVENESS_CHECK_MS)) {
                        if (stream.isClosed()) {
                            log.debug("LocalResolutionProvider: watch stream closed");
                            break;
                        }
                        if (self.get() == null) {
                            log.info("LocalResolutionProvider was garbage collected, stopping watch");
                            break;
                        }
                        continue;
                    }

                    // Debounce: a burst of changes (an editor's write-then-rename, a bulk deploy)
                    // must cost one refresh, so keep extending the window while events keep landing.
                    while (stream.tryGetNextEvent(debounceMs)) {
                        log.debug("LocalResolutionProvider: change during debounce, extending window");
                    }

                    LocalResolutionProvider provider = self.get();
                    if (provider == null) {
                        log.info("LocalResolutionProvider was garbage collected, stopping watch");
                        break;
                    }
                    provider.refreshQuietly();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }, "superposition-watcher");
        watchThread.setDaemon(true);
        watchThread.start();
    }

    /** Background refresh: a failure keeps the last known good data, so it is only logged. */
    private void refreshQuietly() {
        try {
            refresh();
        } catch (SuperpositionError e) {
            log.warn("LocalResolutionProvider: background refresh failed: {}", e.getMessage());
        }
    }

    /** @return the error if the refresh failed and the cached config was kept, else null */
    private SuperpositionError refreshConfig() {
        ConfigData current = cachedConfigData.get();
        Optional<Instant> since = Optional.ofNullable(current).map(ConfigData::getFetchedAt);

        try {
            FetchResponse<ConfigData> response = primaryDataSource.fetchConfig(since);
            if (response.isNotModified()) {
                // A 304 is a successful check: the cache is confirmed current, so the TTL clock
                // restarts. Without this the next evaluation would ask again, immediately.
                configCheckedAt.set(Instant.now());
                log.debug("LocalResolutionProvider: config not modified");
                return null;
            }
            cacheConfig(response.getData().orElseThrow());
            log.debug("LocalResolutionProvider: config refreshed from primary");
            return null;
        } catch (SuperpositionError e) {
            log.warn("LocalResolutionProvider: config refresh failed, keeping last known good: {}",
                    e.getMessage());
            return e;
        }
    }

    /** @return the error if the refresh failed and the cached experiments were kept, else null */
    private SuperpositionError refreshExperiments() {
        if (!primaryDataSource.supportsExperiments()) {
            return null;
        }

        ExperimentData current = cachedExperimentData.get();
        Optional<Instant> since = Optional.ofNullable(current).map(ExperimentData::getFetchedAt);

        try {
            FetchResponse<ExperimentData> response = primaryDataSource.fetchActiveExperiments(since);
            if (response.isNotModified()) {
                // See refreshConfig: a 304 restarts the TTL clock.
                experimentsCheckedAt.set(Instant.now());
                log.debug("LocalResolutionProvider: experiments not modified");
                return null;
            }
            cacheExperiments(response.getData().orElseThrow());
            log.debug("LocalResolutionProvider: experiments refreshed from primary");
            return null;
        } catch (SuperpositionError e) {
            log.warn("LocalResolutionProvider: experiment refresh failed, keeping last known good: {}",
                    e.getMessage());
            return e;
        }
    }

    /** For OnDemand: refresh synchronously once the cached data is older than the TTL. */
    private void ensureFreshData() throws SuperpositionError {
        if (!(refreshStrategy instanceof RefreshStrategy.OnDemand onDemand)) {
            return;
        }

        Instant staleBefore = Instant.now().minusMillis(onDemand.getTtlMilliseconds());
        boolean configStale = isStale(configCheckedAt.get(), staleBefore);
        boolean experimentsStale = supportsExperiments()
                && isStale(experimentsCheckedAt.get(), staleBefore);

        if (!configStale && !experimentsStale) {
            return;
        }

        log.debug("LocalResolutionProvider: TTL expired, refreshing on-demand");
        try {
            refresh();
        } catch (SuperpositionError e) {
            if (!onDemand.getUseStaleOnError()) {
                throw e;
            }
            log.warn("LocalResolutionProvider: on-demand refresh failed, using stale data: {}",
                    e.getMessage());
        }
    }

    /** Never checked, or last checked before the TTL window opened. */
    private static boolean isStale(Instant checkedAt, Instant staleBefore) {
        return checkedAt == null || checkedAt.isBefore(staleBefore);
    }

    // ========== Private: helpers ==========

    /** Global context supplies defaults; the evaluation context wins on conflict. */
    private EvaluationContext mergeWithGlobal(EvaluationContext context) {
        return context == null ? globalContext : globalContext.merge(context);
    }

    private FetchResponse<ExperimentData> filterCachedExperiments(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince,
            boolean partialApply) throws SuperpositionError {
        ExperimentData cached = fetchActiveExperiments(ifModifiedSince).getData().orElseThrow();

        try {
            return FetchResponse.data(new ExperimentData(
                    cache.filterExperiment(
                            context.orElse(null), prefixFilter.orElse(null), partialApply),
                    cached.getFetchedAt()));
        } catch (OperationException e) {
            throw SuperpositionError.dataSourceError(
                    "Failed to filter experiments: " + e.getMessage(), e);
        }
    }

    private void requireExperimentSupport() throws SuperpositionError {
        if (!supportsExperiments()) {
            throw SuperpositionError.dataSourceError("Experiments not supported by this provider");
        }
    }

    /**
     * Wait a bounded time for an already-shutdown executor to finish its current task. Re-asserts
     * the interrupt and returns if interrupted, so shutdown() always proceeds to release resources.
     */
    private static void awaitQuietly(ExecutorService executor) {
        try {
            if (!executor.awaitTermination(2, TimeUnit.SECONDS)) {
                log.warn("LocalResolutionProvider: refresh executor did not stop within 2s; "
                        + "closing resources anyway");
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    private static void closeQuietly(SuperpositionDataSource source, String label) {
        try {
            source.close();
        } catch (Exception e) {
            log.warn("LocalResolutionProvider: error closing {} source: {}", label, e.getMessage());
        }
    }
}
