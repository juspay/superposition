package io.juspay.superposition.provider;

import dev.openfeature.sdk.EvaluationContext;

import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * Interface for experiment metadata and variant resolution.
 */
public interface FeatureExperimentMeta {

    /**
     * Get applicable variant IDs for the given context.
     *
     * @param context the evaluation context
     * @return a future resolving to list of applicable variant IDs
     */
    CompletableFuture<List<String>> getApplicableVariants(EvaluationContext context);
}
