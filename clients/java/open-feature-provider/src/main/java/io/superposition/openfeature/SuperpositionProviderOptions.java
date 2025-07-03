package io.superposition.openfeature;

import io.superposition.openfeature.options.EvaluationCacheOptions;
import io.superposition.openfeature.options.RefreshStrategy;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;
import org.jetbrains.annotations.Nullable;

@Builder
@Data
public class SuperpositionProviderOptions {
    @NonNull
    String endpoint;
    @NonNull
    String workspaceId;
    @NonNull
    String orgId;
    @NonNull
    String token;
    @NonNull
    RefreshStrategy refreshStrategy;
    @Nullable EvaluationCacheOptions evaluationCacheOptions;
    @Nullable SuperpositionConfig fallbackConfig;
    @Nullable ExperimentationOptions experimentationOptions;

    @Builder
    public static class ExperimentationOptions {
        @NonNull
        RefreshStrategy refreshStrategy;
        @Nullable
        EvaluationCacheOptions evaluationCacheOptions;
    }
}
