package io.juspay.superposition.openfeature;

import com.google.gson.JsonSyntaxException;
import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.model.*;
import lombok.NonNull;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.Nullable;
import software.amazon.smithy.java.auth.api.AuthProperties;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResolver;
import software.amazon.smithy.java.auth.api.identity.TokenIdentity;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResult;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;
import dev.openfeature.sdk.*;
import com.google.gson.Gson;
import uniffi.superposition_client.ExperimentationArgs;
import uniffi.superposition_client.FfiExperiment;
import uniffi.superposition_client.OperationException;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static software.amazon.smithy.java.auth.api.identity.TokenIdentity.*;

/**
 * Openfeature Provider implementation for Superposition.
 * <h2>Basic Usage:</h2>
 * <pre>{@code
 * // Configure experimentation options
 * SuperpositionProviderOptions.ExperimentationOptions expOptions =
 * SuperpositionProviderOptions.ExperimentationOptions.builder()
 * .refreshStrategy(RefreshStrategy.Polling.of(5000, 2000)) // 5s timeout, 2s interval
 * .build();
 *
 * // Configure provider options
 * SuperpositionProviderOptions options =
 * SuperpositionProviderOptions.builder()
 * .orgId("your-org-id")
 * .workspaceId("your-workspace-id")
 * .endpoint("https://api.superposition.dev")
 * .token("your-api-token")
 * .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000)) // 10s timeout, 5s interval
 * .experimentationOptions(expOptions)
 * .build();
 *
 * // Initialize provider
 * SuperpositionOpenFeatureProvider provider =
 * new SuperpositionOpenFeatureProvider(options);
 * EvaluationContext initCtx =
 * new ImmutableContext(Map.of("foo", new Value("bar")));
 * provider.initialize(initCtx);
 * OpenFeatureAPI.getInstance().setProvider(provider);
 * Client client = OpenFeatureAPI.getInstance().getClient();
 *
 * // Create evaluation context (optional)
 * EvaluationContext ctx =
 * new ImmutableContext(Map.of("userId", new Value("123")));
 *
 * // Evaluate a boolean flag
 * boolean enabled = client.getBooleanValue("my-feature-flag", false, ctx);
 *  }
 *  </pre>
 */
@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
@Slf4j
public class SuperpositionOpenFeatureProvider implements FeatureProvider {
    private static final Gson gson = new Gson();
    private final SuperpositionAsyncClient sdk;
    private final RefreshJob<EvaluationArgs> configRefresh;
    private final Optional<RefreshJob<List<FfiExperiment>>> expRefresh;
    private Optional<EvaluationContext> defaultCtx;
    private final Optional<EvaluationArgs> fallbackArgs;

    public SuperpositionOpenFeatureProvider(@NonNull SuperpositionProviderOptions options) {
        if (options.fallbackConfig != null) {
            fallbackArgs = Optional.of(new EvaluationArgs(options.fallbackConfig));
        } else {
            fallbackArgs = Optional.empty();
        }
        var builder = SuperpositionAsyncClient.builder()
            .endpointResolver(EndpointResolver.staticEndpoint(options.endpoint))
            .addIdentityResolver(new IdentityResolverImpl(options.token));
        this.sdk = builder.build();
        var getConfigInput = GetConfigInput.builder()
            .context(Map.of())
            .orgId(options.orgId)
            .workspaceId(options.workspaceId)
            .build();
        this.configRefresh = RefreshJob.create(
            options.refreshStrategy,
            () -> sdk.getConfig(getConfigInput).thenApply(EvaluationArgs::new)
        );
        if (options.experimentationOptions != null) {
            var listExpInput = ListExperimentInput.builder()
                .orgId(options.orgId)
                .workspaceId(options.workspaceId)
                .status(ExperimentStatusType.INPROGRESS)
                .build();
            this.expRefresh = Optional.of(
                RefreshJob.create(
                    options.experimentationOptions.refreshStrategy,
                    () -> sdk.listExperiment(listExpInput)
                        .thenApply(e -> e
                            .data()
                            .stream()
                            .map(EvaluationArgs.Helpers::toFfiExperiment)
                            .toList()
                        ))
            );
        } else {
            this.expRefresh = Optional.empty();
        }
    }

    @Override
    public Metadata getMetadata() {
        return () -> "SuperpositionOpenFeatureProvider";
    }

    @Override
    public void initialize(EvaluationContext eCtx) {
        this.defaultCtx = Optional.ofNullable(eCtx);
        if (configRefresh instanceof RefreshJob.Poll) {
            ((RefreshJob.Poll<?>) configRefresh).start();
        }
        if (expRefresh.isPresent()) {
            var r = expRefresh.get();
            if (r instanceof RefreshJob.Poll) {
                ((RefreshJob.Poll<?>) r).start();
            }
        }
    }

    @Override
    public void shutdown() {
        configRefresh.shutdown();
    }

    @SneakyThrows
    @Override
    public ProviderEvaluation<Boolean> getBooleanEvaluation(
        @NonNull String key,
        @NonNull Boolean defaultValue,
        @NonNull EvaluationContext ctx
    ) {
        return getEvaluation(key, defaultValue, ctx, Boolean.class);
    }

    @Override
    public ProviderEvaluation<String> getStringEvaluation(
        @NonNull String key,
        @NonNull String defaultValue,
        @NonNull EvaluationContext ctx
    ) {
        return getEvaluation(key, defaultValue, ctx, String.class);
    }

    @Override
    public ProviderEvaluation<Integer> getIntegerEvaluation(
        @NonNull String key,
        @NonNull Integer defaultValue,
        @NonNull EvaluationContext ctx
    ) {
        return getEvaluation(key, defaultValue, ctx, Integer.class);
    }

    @Override
    public ProviderEvaluation<Double> getDoubleEvaluation(
        @NonNull String key,
        @NonNull Double defaultValue,
        @NonNull EvaluationContext ctx
    ) {
        return getEvaluation(key, defaultValue, ctx, Double.class);
    }

    @SneakyThrows
    @Override
    public ProviderEvaluation<Value> getObjectEvaluation(
        @NonNull String key,
        @NonNull Value defaultValue,
        @NonNull EvaluationContext ctx
    ) {
        ProviderEvaluation<Object> pe = getEvaluation(key, defaultValue, ctx, Object.class);
        return ProviderEvaluation.<Value>builder()
            .value(Value.objectToValue(pe.getValue()))
            .variant(pe.getVariant())
            .errorMessage(pe.getErrorMessage())
            .errorCode(pe.getErrorCode())
            .build();
    }

    private <T> ProviderEvaluation<T> getEvaluation(String key, T defaultValue, EvaluationContext ctx, Class<T> c) {
        try {
            var config = evaluateConfigInternal(ctx);
            var entry = config.get(key);
            if (entry == null) {
                throw new NullPointerException("Evaluation-key: " + key + " not found!");
            }
            var value = gson.fromJson(entry, c);
            return ProviderEvaluation.<T>builder()
                .value(value)
                .variant("evaluated")
                .build();
        } catch (Exception e) {
            log.error("An exception occurred during evaluation.\nMessage: {}.", e.getMessage());
            ErrorCode ec = ErrorCode.PROVIDER_NOT_READY;
            if (e instanceof JsonSyntaxException) {
                ec = ErrorCode.TYPE_MISMATCH;
            } else if (e instanceof NullPointerException) {
                ec = ErrorCode.FLAG_NOT_FOUND;
            } else if (e instanceof OperationException) {
                ec = ErrorCode.PROVIDER_FATAL;
            }
            return ProviderEvaluation.<T>builder()
                .value(defaultValue)
                .variant("default")
                .errorCode(ec)
                .errorMessage(e.getMessage())
                .build();
        }
    }

    public @Nullable List<String> applicableVariants(@NonNull EvaluationContext ctx) {
        try {
            return EvaluationArgs.Companion.getApplicableVariants$openfeature_provider(ctx, getExperimentationArgs(ctx));
        } catch (Exception e) {
            log.error("An exception occurred during evaluation.\nMessage: {}.", e.getMessage());
            return null;
        }
    }

    public @Nullable Map<String, Object> evaluateConfig(@NonNull EvaluationContext ctx) {
        try {
            var config = evaluateConfigInternal(ctx);
            return config.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                entry -> gson.fromJson(entry.getValue(), Object.class)
            ));
        } catch (Exception e) {
            log.error("An exception occurred during evaluation.\nMessage: {}.", e.getMessage());
            return null;
        }
    }

    private Map<String, String> evaluateConfigInternal(EvaluationContext ctx) throws Exception {
        EvaluationArgs args;
        var out = configRefresh.getOutput();
        if (out.isEmpty() && fallbackArgs.isPresent()) {
            log.error("Config refresh failed, evaluating using fallback-config.");
            args = fallbackArgs.get();
        } else if (out.isPresent()) {
            args = out.get();
        } else {
            throw new Exception("No configuration available to evaluate.");
        }
        var ctx_ = defaultCtx.isPresent() ? ctx.merge(defaultCtx.get()) : ctx;
        return args.evaluate(ctx_, getExperimentationArgs(ctx_));
    }

    private ExperimentationArgs getExperimentationArgs(EvaluationContext ctx) {
        var job = expRefresh.orElse(null);
        var tkey = ctx.getTargetingKey();
        if (tkey != null) {
            log.debug("Targeting-key is: {}", tkey);
            if (job == null) {
                log.warn("Attempting to use targeting-key w/o setting up experimentation.");
            } else if (job.getOutput().isEmpty()) {
                log.error("Experimentation data is not available.");
            } else {
                log.debug("Using experimentation output: {}", job.getOutput().get());
                return new ExperimentationArgs(job.getOutput().get(), tkey);
            }
        }
        return null;
    }

    // TODO EXPLAIN???
    @SuppressWarnings("rawtypes")
    private static class IdentityResolverImpl implements IdentityResolver {
        TokenIdentity identity;

        IdentityResolverImpl(String token) {
            this.identity = create(token);
        }

        @Override
        public CompletableFuture<IdentityResult> resolveIdentity(AuthProperties requestProperties) {
            return CompletableFuture.completedFuture(IdentityResult.of(identity));
        }

        @Override
        public Class identityType() {
            return TokenIdentity.class;
        }
    }
}
