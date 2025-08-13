package io.juspay.superposition.openfeature;

import io.juspay.superposition.openfeature.options.EvaluationCacheOptions;
import io.juspay.superposition.openfeature.options.RefreshStrategy;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import org.jetbrains.annotations.Nullable;

/**
 * Options for configuring the Superposition OpenFeature provider.
 * Includes connection details, refresh strategy, caching, and experimentation options.
 */
@Builder
@Data
public class SuperpositionProviderOptions {
    /** The Superposition API endpoint URL. */
    @NonNull
    String endpoint;
    /** The workspace ID for the Superposition project. */
    @NonNull
    String workspaceId;
    /** The organization ID for the Superposition project. */
    @NonNull
    String orgId;
    /** The authentication token for accessing the Superposition API. */
    @NonNull
    String token;
    /** Strategy for refreshing feature flag data from the backend. */
    @NonNull
    RefreshStrategy refreshStrategy;
    /** Fallback configuration if remote fetch fails (optional). */
    @Nullable SuperpositionConfig fallbackConfig;
    /** Experimentation-specific options (optional). */
    @Nullable ExperimentationOptions experimentationOptions;

    /**
     * Options for experimentation mode, allowing separate refresh and cache settings.
     */
    @Builder
    public static class ExperimentationOptions {
        /** Refresh strategy for experimentation data. */
        @NonNull
        RefreshStrategy refreshStrategy;
        /** Evaluation cache options for experimentation data (optional). */
        @Nullable
        EvaluationCacheOptions evaluationCacheOptions;
    }
}
