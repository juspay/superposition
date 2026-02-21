package io.juspay.superposition.provider;

import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * Data class representing Superposition experiment data.
 */
public class ExperimentData {
    private final List<Map<String, Object>> experiments;
    private final List<Map<String, Object>> experimentGroups;
    private final Instant fetchedAt;

    public ExperimentData(
        List<Map<String, Object>> experiments,
        List<Map<String, Object>> experimentGroups
    ) {
        this.experiments = experiments;
        this.experimentGroups = experimentGroups;
        this.fetchedAt = Instant.now();
    }

    public List<Map<String, Object>> getExperiments() {
        return experiments;
    }

    public List<Map<String, Object>> getExperimentGroups() {
        return experimentGroups;
    }

    public Instant getFetchedAt() {
        return fetchedAt;
    }
}
