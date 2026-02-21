package io.juspay.superposition.provider;

import dev.openfeature.sdk.EvaluationContext;
import dev.openfeature.sdk.FeatureProvider;
import dev.openfeature.sdk.Metadata;
import dev.openfeature.sdk.ProviderEvaluation;
import dev.openfeature.sdk.Value;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Local resolution provider that implements FeatureProvider with primary/fallback
 * data sources and configurable refresh strategies.
 */
public class LocalResolutionProvider implements FeatureProvider, AllFeatureProvider, FeatureExperimentMeta {

    private final SuperpositionDataSource primary;
    private final Optional<SuperpositionDataSource> fallback;
    private final RefreshStrategy refreshStrategy;
    private final Optional<CacheOptions> cacheOptions;

    private final AtomicReference<Optional<ConfigData>> cachedConfig;
    private final AtomicReference<Optional<ExperimentData>> cachedExperiments;
    private Optional<ScheduledFuture<?>> pollingTask;
    private final ScheduledExecutorService scheduler;
    private volatile boolean initialized;

    private Instant lastRefreshTime;

    /**
     * Creates a new LocalResolutionProvider.
     *
     * @param primary the primary data source
     * @param fallback optional fallback data source
     * @param refreshStrategy the refresh strategy to use
     * @param cacheOptions optional cache configuration
     */
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
        this.scheduler = Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "LocalResolutionProvider-Scheduler");
            t.setDaemon(true);
            return t;
        });
        this.initialized = false;
        this.lastRefreshTime = Instant.MIN;
    }

    @Override
    public Metadata getMetadata() {
        return () -> "LocalResolutionProvider";
    }

    @Override
    public void initialize(EvaluationContext context) {
        try {
            doRefresh().get();

            if (refreshStrategy instanceof RefreshStrategy.Polling polling) {
                startPolling(polling.interval());
            }

            initialized = true;
        } catch (Exception e) {
            initialized = false;
            throw new RuntimeException("Failed to initialize LocalResolutionProvider", e);
        }
    }

    @Override
    public void shutdown() {
        stopPolling();

        CompletableFuture<Void> primaryClose = primary.close();
        CompletableFuture<Void> fallbackClose = fallback
            .map(SuperpositionDataSource::close)
            .orElse(CompletableFuture.completedFuture(null));

        CompletableFuture.allOf(primaryClose, fallbackClose).join();

        cachedConfig.set(Optional.empty());
        cachedExperiments.set(Optional.empty());

        scheduler.shutdown();
        try {
            if (!scheduler.awaitTermination(5, TimeUnit.SECONDS)) {
                scheduler.shutdownNow();
            }
        } catch (InterruptedException e) {
            scheduler.shutdownNow();
            Thread.currentThread().interrupt();
        }

        initialized = false;
    }

    /**
     * Manually triggers a refresh of the configuration.
     *
     * @return a future that completes when refresh is done
     */
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
            Optional<ExperimentData> experiments = cachedExperiments.get();
            if (experiments.isEmpty()) {
                return Collections.emptyList();
            }

            // TODO: Use FFI to get applicable variants from cached experiments
            // This is a placeholder implementation
            return Collections.emptyList();
        });
    }

    @Override
    public ProviderEvaluation<Boolean> getBooleanEvaluation(
        String key,
        Boolean defaultValue,
        EvaluationContext ctx
    ) {
        return getEvaluation(key, defaultValue, ctx, Boolean.class);
    }

    @Override
    public ProviderEvaluation<String> getStringEvaluation(
        String key,
        String defaultValue,
        EvaluationContext ctx
    ) {
        return getEvaluation(key, defaultValue, ctx, String.class);
    }

    @Override
    public ProviderEvaluation<Integer> getIntegerEvaluation(
        String key,
        Integer defaultValue,
        EvaluationContext ctx
    ) {
        return getEvaluation(key, defaultValue, ctx, Integer.class);
    }

    @Override
    public ProviderEvaluation<Double> getDoubleEvaluation(
        String key,
        Double defaultValue,
        EvaluationContext ctx
    ) {
        return getEvaluation(key, defaultValue, ctx, Double.class);
    }

    @Override
    public ProviderEvaluation<Value> getObjectEvaluation(
        String key,
        Value defaultValue,
        EvaluationContext ctx
    ) {
        ProviderEvaluation<Object> evaluation = getEvaluation(key, defaultValue, ctx, Object.class);
        return ProviderEvaluation.<Value>builder()
            .value(Value.objectToValue(evaluation.getValue()))
            .variant(evaluation.getVariant())
            .errorCode(evaluation.getErrorCode())
            .errorMessage(evaluation.getErrorMessage())
            .build();
    }

    /**
     * Generic evaluation logic for all types.
     */
    private <T> ProviderEvaluation<T> getEvaluation(
        String key,
        T defaultValue,
        EvaluationContext ctx,
        Class<T> clazz
    ) {
        checkOnDemandRefresh();

        Optional<ConfigData> config = cachedConfig.get();
        if (config.isEmpty()) {
            return ProviderEvaluation.<T>builder()
                .value(defaultValue)
                .variant("default")
                .errorCode(dev.openfeature.sdk.ErrorCode.PROVIDER_NOT_READY)
                .errorMessage("Configuration not available")
                .build();
        }

        try {
            Map<String, Object> evaluated = evalWithContext(ctx, Optional.empty()).get();
            Object value = evaluated.get(key);

            if (value == null) {
                return ProviderEvaluation.<T>builder()
                    .value(defaultValue)
                    .variant("default")
                    .errorCode(dev.openfeature.sdk.ErrorCode.FLAG_NOT_FOUND)
                    .errorMessage("Key not found: " + key)
                    .build();
            }

            T convertedValue = convertValue(value, clazz, defaultValue);
            return ProviderEvaluation.<T>builder()
                .value(convertedValue)
                .variant("evaluated")
                .build();
        } catch (Exception e) {
            return ProviderEvaluation.<T>builder()
                .value(defaultValue)
                .variant("default")
                .errorCode(dev.openfeature.sdk.ErrorCode.GENERAL)
                .errorMessage(e.getMessage())
                .build();
        }
    }

    /**
     * Core evaluation with FFI.
     */
    private CompletableFuture<Map<String, Object>> evalWithContext(
        EvaluationContext context,
        Optional<List<String>> prefixFilter
    ) {
        return CompletableFuture.supplyAsync(() -> {
            Optional<ConfigData> config = cachedConfig.get();
            if (config.isEmpty()) {
                return Collections.emptyMap();
            }

            ConfigData data = config.get();
            Map<String, Object> result = new HashMap<>();

            // Start with default configs
            if (data.getDefaultConfigs() != null) {
                result.putAll(data.getDefaultConfigs());
            }

            // Apply contexts in priority order
            if (data.getContexts() != null) {
                List<Map<String, Object>> sortedContexts = new ArrayList<>(data.getContexts());
                sortedContexts.sort((a, b) -> {
                    Integer priorityA = (Integer) a.getOrDefault("priority", 0);
                    Integer priorityB = (Integer) b.getOrDefault("priority", 0);
                    return priorityB.compareTo(priorityA);
                });

                Map<String, String> extractedContext = extractContext(context);

                for (Map<String, Object> ctx : sortedContexts) {
                    if (matchesContext(ctx, extractedContext)) {
                        @SuppressWarnings("unchecked")
                        List<String> overrideKeys = (List<String>) ctx.get("override_with_keys");
                        if (overrideKeys != null && data.getOverrides() != null) {
                            for (String key : overrideKeys) {
                                @SuppressWarnings("unchecked")
                                Map<String, Object> override = (Map<String, Object>) data.getOverrides().get(key);
                                if (override != null) {
                                    result.putAll(override);
                                }
                            }
                        }
                    }
                }
            }

            // Apply prefix filter if present
            if (prefixFilter.isPresent()) {
                List<String> prefixes = prefixFilter.get();
                result.entrySet().removeIf(entry -> {
                    String key = entry.getKey();
                    return prefixes.stream().noneMatch(key::startsWith);
                });
            }

            return result;
        });
    }

    /**
     * Fetches configuration from primary with fallback.
     */
    private CompletableFuture<Void> doRefresh() {
        CompletableFuture<ConfigData> configFuture = primary.fetchConfig()
            .exceptionally(ex -> {
                if (fallback.isPresent()) {
                    return fallback.get().fetchConfig().join();
                }
                throw new RuntimeException("Primary failed and no fallback available", ex);
            });

        CompletableFuture<Optional<ExperimentData>> experimentsFuture = primary.supportsExperiments()
            ? primary.fetchActiveExperiments()
                .exceptionally(ex -> {
                    if (fallback.isPresent() && fallback.get().supportsExperiments()) {
                        return fallback.get().fetchActiveExperiments().join();
                    }
                    return Optional.empty();
                })
            : CompletableFuture.completedFuture(Optional.empty());

        return configFuture.thenCombine(experimentsFuture, (config, experiments) -> {
            cachedConfig.set(Optional.of(config));
            cachedExperiments.set(experiments);
            lastRefreshTime = Instant.now();
            return null;
        });
    }

    /**
     * Checks TTL and refreshes if needed for OnDemand strategy.
     */
    private void checkOnDemandRefresh() {
        if (refreshStrategy instanceof RefreshStrategy.OnDemand onDemand) {
            long ttlSeconds = onDemand.ttl();
            Instant now = Instant.now();
            if (lastRefreshTime.plusSeconds(ttlSeconds).isBefore(now)) {
                doRefresh();
            }
        }
    }

    /**
     * Starts polling at the given interval.
     */
    private void startPolling(long intervalSeconds) {
        ScheduledFuture<?> future = scheduler.scheduleAtFixedRate(
            () -> doRefresh().exceptionally(ex -> {
                System.err.println("Polling refresh failed: " + ex.getMessage());
                return null;
            }),
            intervalSeconds,
            intervalSeconds,
            TimeUnit.SECONDS
        );
        pollingTask = Optional.of(future);
    }

    /**
     * Stops the polling task.
     */
    private void stopPolling() {
        pollingTask.ifPresent(future -> {
            future.cancel(false);
            pollingTask = Optional.empty();
        });
    }

    /**
     * Extracts context as Map<String, String> from EvaluationContext.
     */
    private Map<String, String> extractContext(EvaluationContext context) {
        Map<String, String> result = new HashMap<>();
        if (context == null) {
            return result;
        }

        // Extract targeting key
        String targetingKey = context.getTargetingKey();
        if (targetingKey != null) {
            result.put("targeting_key", targetingKey);
        }

        // Extract all attributes
        context.asMap().forEach((key, value) -> {
            if (value != null) {
                result.put(key, value.toString());
            }
        });

        return result;
    }

    /**
     * Checks if a context matches the extracted context.
     */
    @SuppressWarnings("unchecked")
    private boolean matchesContext(Map<String, Object> ctx, Map<String, String> extractedContext) {
        Object conditionObj = ctx.get("condition");
        if (!(conditionObj instanceof Map)) {
            return true; // No condition means matches all
        }

        Map<String, Object> condition = (Map<String, Object>) conditionObj;
        for (Map.Entry<String, Object> entry : condition.entrySet()) {
            String extractedValue = extractedContext.get(entry.getKey());
            if (extractedValue == null) {
                return false;
            }
            if (!extractedValue.equals(entry.getValue().toString())) {
                return false;
            }
        }

        return true;
    }

    /**
     * Converts a value to the target type.
     */
    @SuppressWarnings("unchecked")
    private <T> T convertValue(Object value, Class<T> clazz, T defaultValue) {
        if (value == null) {
            return defaultValue;
        }

        if (clazz.isInstance(value)) {
            return clazz.cast(value);
        }

        try {
            if (clazz == Boolean.class) {
                return (T) Boolean.valueOf(value.toString());
            } else if (clazz == String.class) {
                return (T) value.toString();
            } else if (clazz == Integer.class) {
                if (value instanceof Number) {
                    return (T) Integer.valueOf(((Number) value).intValue());
                }
                return (T) Integer.valueOf(value.toString());
            } else if (clazz == Double.class) {
                if (value instanceof Number) {
                    return (T) Double.valueOf(((Number) value).doubleValue());
                }
                return (T) Double.valueOf(value.toString());
            } else if (clazz == Object.class) {
                return (T) value;
            }
        } catch (Exception e) {
            // Conversion failed, return default
        }

        return defaultValue;
    }
}
