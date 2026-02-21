package io.juspay.superposition.provider;

import dev.openfeature.sdk.EvaluationContext;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Interface for bulk configuration resolution.
 */
public interface AllFeatureProvider {

    /**
     * Resolve all features for the given evaluation context.
     *
     * @param context the evaluation context
     * @return a future resolving to all features as a map
     */
    CompletableFuture<Map<String, Object>> resolveAllFeatures(EvaluationContext context);

    /**
     * Resolve features matching prefix filters.
     *
     * @param context the evaluation context
     * @param prefixFilter list of prefix filters to apply
     * @return a future resolving to filtered features as a map
     */
    CompletableFuture<Map<String, Object>> resolveAllFeaturesWithFilter(
        EvaluationContext context,
        List<String> prefixFilter
    );
}
