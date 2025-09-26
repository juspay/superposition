package io.juspay.superposition.openfeature;

import dev.openfeature.sdk.Structure;
import dev.openfeature.sdk.Value;
import lombok.Builder;
import lombok.Data;
import lombok.NonNull;

import java.util.List;
import java.util.Map;

import uniffi.superposition_types.DimensionInfo;
import uniffi.superposition_types.Context;

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
    @NonNull
    Map<String, DimensionInfo> dimensions;
}
