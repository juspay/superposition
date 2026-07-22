package io.juspay.superposition.openfeature.provider;

import dev.openfeature.sdk.EvaluationContext;
import dev.openfeature.sdk.FeatureProvider;
import dev.openfeature.sdk.ImmutableContext;
import dev.openfeature.sdk.Metadata;
import dev.openfeature.sdk.ProviderEvaluation;
import dev.openfeature.sdk.ProviderState;
import dev.openfeature.sdk.Value;
import io.juspay.superposition.client.SuperpositionClient;
import io.juspay.superposition.model.ApplicableVariantsInput;
import io.juspay.superposition.model.ApplicableVariantsOutput;
import io.juspay.superposition.model.GetResolvedConfigWithIdentifierInput;
import io.juspay.superposition.model.GetResolvedConfigWithIdentifierOutput;
import io.juspay.superposition.model.Variant;
import io.juspay.superposition.openfeature.EvaluationArgs;
import io.juspay.superposition.openfeature.FfiUtils;
import io.juspay.superposition.openfeature.error.SuperpositionError;
import io.juspay.superposition.openfeature.options.SuperpositionOptions;
import io.juspay.superposition.openfeature.traits.AllFeatureProvider;
import io.juspay.superposition.openfeature.traits.FeatureExperimentMeta;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;
import software.amazon.smithy.java.core.serde.document.Document;

/**
 * Remote resolution provider: delegates all resolution to the Superposition API.
 *
 * Every evaluation is a server-side resolve — the service applies contexts, overrides and
 * experiment variants and returns the finished config. Use this when:
 * - Near real-time config updates are required (zero cache staleness)
 * - Local storage is not desirable
 * - The application can tolerate the HTTP round-trip latency
 *
 * Uses the Smithy-generated synchronous {@link SuperpositionClient}.
 */
public class SuperpositionAPIProvider implements FeatureProvider, AllFeatureProvider, FeatureExperimentMeta {

    private static final Logger log = LoggerFactory.getLogger(SuperpositionAPIProvider.class);

    private final SuperpositionOptions options;
    private final SuperpositionClient sdk;
    private final AtomicReference<ProviderState> state = new AtomicReference<>(ProviderState.NOT_READY);
    private volatile EvaluationContext globalContext = new ImmutableContext();

    /**
     * Create a new remote resolution provider.
     *
     * @param options connection options (endpoint, token, org_id, workspace_id)
     */
    public SuperpositionAPIProvider(SuperpositionOptions options) {
        options.validate();
        this.options = options;
        this.sdk = SuperpositionClient.builder()
                .endpointResolver(EndpointResolver.staticEndpoint(options.getEndpoint()))
                .addIdentityResolver(options.getAuth().identityResolver())
                .build();
        log.debug("SuperpositionAPIProvider created for endpoint: {}", options.getEndpoint());
    }

    // ========== FeatureProvider ==========

    @Override
    public Metadata getMetadata() {
        return () -> "SuperpositionAPIProvider";
    }

    @Override
    public ProviderState getState() {
        return state.get();
    }

    @Override
    public void initialize(EvaluationContext context) {
        this.globalContext = context != null ? context : new ImmutableContext();
        state.set(ProviderState.READY);
        log.info("SuperpositionAPIProvider initialized successfully");
    }

    @Override
    public void shutdown() {
        state.set(ProviderState.NOT_READY);
        log.info("SuperpositionAPIProvider shut down");
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
        EvaluationContext merged = mergeWithGlobal(context);

        GetResolvedConfigWithIdentifierInput.Builder inputBuilder =
                GetResolvedConfigWithIdentifierInput.builder()
                        .workspaceId(options.getWorkspaceId())
                        .orgId(options.getOrgId())
                        .context(contextOf(merged));

        Optional.ofNullable(merged.getTargetingKey())
                .filter(key -> !key.isEmpty())
                .ifPresent(inputBuilder::identifier);
        prefixFilter.filter(prefixes -> !prefixes.isEmpty()).ifPresent(inputBuilder::prefix);

        try {
            GetResolvedConfigWithIdentifierOutput output =
                    sdk.getResolvedConfigWithIdentifier(inputBuilder.build());
            return FfiUtils.resolvedConfigToJsonMap(output.config());
        } catch (Exception e) {
            throw SuperpositionError.networkError(
                    "Failed to get resolved config: " + e.getMessage(), e);
        }
    }

    // ========== FeatureExperimentMeta ==========

    @Override
    public List<String> getApplicableVariants(
            EvaluationContext context,
            Optional<List<String>> prefixFilter) throws SuperpositionError {
        EvaluationContext merged = mergeWithGlobal(context);

        // An absent targeting key is not an error: the service buckets an empty identifier the
        // same way local resolution does, and simply matches no experiments.
        String targetingKey = Optional.ofNullable(merged.getTargetingKey()).orElse("");

        ApplicableVariantsInput.Builder inputBuilder = ApplicableVariantsInput.builder()
                .workspaceId(options.getWorkspaceId())
                .orgId(options.getOrgId())
                .context(contextOf(merged))
                .identifier(targetingKey);

        prefixFilter.filter(prefixes -> !prefixes.isEmpty()).ifPresent(inputBuilder::prefix);

        try {
            ApplicableVariantsOutput output = sdk.applicableVariants(inputBuilder.build());
            return output.data().stream().map(Variant::id).collect(Collectors.toList());
        } catch (Exception e) {
            throw SuperpositionError.networkError(
                    "Failed to get applicable variants: " + e.getMessage(), e);
        }
    }

    // ========== Private helpers ==========

    /** Global context supplies defaults; the evaluation context wins on conflict. */
    private EvaluationContext mergeWithGlobal(EvaluationContext context) {
        return context == null ? globalContext : globalContext.merge(context);
    }

    /**
     * Build the request context. Values go through the same JSON encoding the FFI uses, so
     * numbers and booleans reach the service as numbers and booleans rather than strings.
     */
    private static Map<String, Document> contextOf(EvaluationContext context) {
        return FfiUtils.contextToDocuments(EvaluationArgs.buildQueryData(context));
    }
}
