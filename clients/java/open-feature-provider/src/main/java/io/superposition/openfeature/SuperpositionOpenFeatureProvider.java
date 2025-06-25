package io.superposition.openfeature;

import com.google.gson.JsonSyntaxException;
import com.google.gson.reflect.TypeToken;
import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.model.GetConfigOutput;
import lombok.NonNull;
import lombok.SneakyThrows;
import software.amazon.smithy.java.auth.api.AuthProperties;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResolver;
import software.amazon.smithy.java.auth.api.identity.TokenIdentity;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResult;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;
import dev.openfeature.sdk.*;
import io.juspay.superposition.model.GetConfigInput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.google.gson.Gson;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.List;

import static software.amazon.smithy.java.auth.api.identity.TokenIdentity.*;

@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
public class SuperpositionOpenFeatureProvider implements FeatureProvider {
    private static final Gson gson = new Gson();
    private static final Logger logger =
        LoggerFactory.getLogger(SuperpositionOpenFeatureProvider.class);
    private final SuperpositionAsyncClient sdk;
    private final SuperpositionProviderOptions options;
    private final GetConfigInput input;
    private RefreshJob<GetConfigOutput> configRefresh;
    private Optional<EvaluationContext> defaultCtx;
    private final Optional<EvaluationArgs> fallbackArgs;

    SuperpositionOpenFeatureProvider(@NonNull SuperpositionProviderOptions options) {
        this.options = options;
        if (options.fallbackConfig != null) {
            fallbackArgs = Optional.of(new EvaluationArgs(options.fallbackConfig));
        } else {
            fallbackArgs = Optional.empty();
        }
        var builder = SuperpositionAsyncClient.builder()
            .endpointResolver(EndpointResolver.staticEndpoint(options.endpoint))
            .addIdentityResolver(new IdentityResolverImpl(options.token));
        this.sdk = builder.build();
        this.input = GetConfigInput.builder()
            .context(Map.of())
            .orgId(options.orgId)
            .workspaceId(options.workspaceId)
            .build();
    }

    @Override
    public Metadata getMetadata() {
        return () -> "SuperpositionOpenFeatureProvider";
    }

    @Override
    public void initialize(EvaluationContext eCtx) {
        this.defaultCtx = Optional.ofNullable(eCtx);
        this.configRefresh = RefreshJob.create(options.refreshStrategy, () -> sdk.getConfig(input));
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
        var val = pe.getValue();
        if (!(val instanceof Value)) {
            // REVIEW Is this even needed?
            if (val instanceof String) {
                // If resulted into a String, this could be a serialized object/array as `stringToValue`
                // only handles primitives.
                try {
                    logger.debug("Trying to cast as an object.");
                    val = gson.fromJson((String)val, new TypeToken<Map<String, Object>>(){}.getType());
                } catch (Exception ignored) {
                    logger.debug("Not an object, trying to cast as a list.");
                    try {
                        val = gson.fromJson((String)val, new TypeToken<List<Object>>(){}.getType());
                    } catch (Exception ignored1) {
                    }
                }
            }
            val = Value.objectToValue(val);
        }
        return ProviderEvaluation.<Value>builder()
            .value((Value)val)
            .variant(pe.getVariant())
            .errorMessage(pe.getErrorMessage())
            .errorCode(pe.getErrorCode())
            .build();
    }

    private <T> ProviderEvaluation<T> getEvaluation(String key, T defaultValue, EvaluationContext ctx, Class<T> c) {
        try {
            var config = runEvaluation(ctx);
            var entry = config.get(key);
            if (entry == null) {
                throw new NullPointerException("Key not found!");
            }
            var value = gson.fromJson(entry, c);
            return ProviderEvaluation.<T>builder()
                .value(value)
                .variant("evaluated")
                .build();
        } catch (Exception e) {
            logger.error("An exception occurred during evaluation.\nMessage: {}.", e.getMessage());
            ErrorCode ec = ErrorCode.PROVIDER_FATAL;
            if (e instanceof JsonSyntaxException) {
                ec = ErrorCode.TYPE_MISMATCH;
            } else if (e instanceof NullPointerException) {
                ec = ErrorCode.TARGETING_KEY_MISSING;
            }
            return ProviderEvaluation.<T>builder()
                .value(defaultValue)
                .variant("default")
                .errorCode(ec)
                .errorMessage(e.getMessage())
                .build();
        }
    }

    private Map<String, String> runEvaluation(EvaluationContext ctx) throws Exception {
        EvaluationArgs args;
        var out = configRefresh.getOutput();
        if (out.isEmpty() && fallbackArgs.isPresent()) {
            logger.error("Config refresh failed, evaluating using fallback-config.");
            args = fallbackArgs.get();
        } else if (out.isPresent()) {
            // FIXME Move this setup out into refresh, invalid configs should be rejected.
            args = new EvaluationArgs(out.get());
        } else {
            throw new Exception("No configuration available to evaluate.");
        }
        return args.evaluate(defaultCtx.isPresent() ? ctx.merge(defaultCtx.get()) : ctx);
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
