package io.juspay.superposition.provider;

import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * Data class representing Superposition configuration data.
 */
public class ConfigData {
    private final Map<String, Object> defaultConfigs;
    private final List<Map<String, Object>> contexts;
    private final Map<String, Object> overrides;
    private final Map<String, Object> dimensions;
    private final Instant fetchedAt;

    public ConfigData(
        Map<String, Object> defaultConfigs,
        List<Map<String, Object>> contexts,
        Map<String, Object> overrides,
        Map<String, Object> dimensions
    ) {
        this.defaultConfigs = defaultConfigs;
        this.contexts = contexts;
        this.overrides = overrides;
        this.dimensions = dimensions;
        this.fetchedAt = Instant.now();
    }

    public Map<String, Object> getDefaultConfigs() {
        return defaultConfigs;
    }

    public List<Map<String, Object>> getContexts() {
        return contexts;
    }

    public Map<String, Object> getOverrides() {
        return overrides;
    }

    public Map<String, Object> getDimensions() {
        return dimensions;
    }

    public Instant getFetchedAt() {
        return fetchedAt;
    }
}
