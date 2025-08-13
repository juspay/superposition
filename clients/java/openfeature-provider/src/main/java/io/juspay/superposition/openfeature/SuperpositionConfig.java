package io.juspay.superposition.openfeature;

import dev.openfeature.sdk.Structure;
import dev.openfeature.sdk.Value;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

import java.util.List;
import java.util.Map;

/**
 * Data class to represent Superposition's config.
 * */
@Data
@Builder
public class SuperpositionConfig {
    @NonNull
    Map<String, Value> defaultConfig;
    @NonNull
    List<Context> contexts;
    @NonNull
    Map<String, Map<String, Value>> overrides;

    /**
     * Superposition's representation of context.
     * See Superposition docs for more detail.
     * */
    @Builder
    public static class Context {
        @NonNull
        String id;
        @NonNull
        Map<String, Structure> condition;
        int priority;
        int weight;
        @NonNull
        List<String> overrideWithKeys;
    }
}
