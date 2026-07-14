package io.juspay.superposition.openfeature.traits;

import dev.openfeature.sdk.EvaluationContext;
import io.juspay.superposition.openfeature.error.SuperpositionError;
import java.util.List;
import java.util.Optional;

/**
 * Interface for experiment variant resolution.
 * 
 * Implementors provide the ability to determine which experiment variants
 * are applicable for a given evaluation context.
 * 
 * This is particularly useful for understanding which variants have been assigned
 * to a targeting key, enabling analytics, monitoring, and A/B testing workflows.
 */
public interface FeatureExperimentMeta {
    
    /**
     * Get the list of applicable experiment variant IDs for the given context.
     * 
     * Returns the variant IDs that should be applied based on the experiments
     * that match the given evaluation context.
     * 
     * An optional prefix_filter can be provided to narrow the scope to only
     * variants in experiments whose IDs start with the given prefixes.
     * 
     * @param context the evaluation context containing dimensions and targeting key
     * @param prefixFilter optional list of experiment ID prefixes
     * @return list of applicable variant IDs (unordered, may contain duplicates)
     * @throws SuperpositionError if resolution fails
     */
    List<String> getApplicableVariants(
            EvaluationContext context,
            Optional<List<String>> prefixFilter) throws SuperpositionError;
    
    /**
     * Get the list of applicable experiment variant IDs for the given context.
     * 
     * Convenience method equivalent to calling with empty prefix filter.
     * 
     * @param context the evaluation context
     * @return list of applicable variant IDs
     * @throws SuperpositionError if resolution fails
     */
    default List<String> getApplicableVariants(EvaluationContext context) throws SuperpositionError {
        return getApplicableVariants(context, Optional.empty());
    }
}
