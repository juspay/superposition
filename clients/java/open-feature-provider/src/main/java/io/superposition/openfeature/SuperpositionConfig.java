package io.superposition.openfeature;

import dev.openfeature.sdk.Structure;
import dev.openfeature.sdk.Value;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

import java.util.List;
import java.util.Map;

@Data
@Builder
public class SuperpositionConfig {
    @NonNull
    Map<String, Value> defaultConfig;
    @NonNull
    List<Context> contexts;
    @NonNull
    Map<String, Map<String, Value>> overrides;

    // Copy created for public use.
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
