package io.juspay.superposition.provider;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

/**
 * Interface for abstracting data sources.
 */
public interface SuperpositionDataSource {

    /**
     * Fetch the latest configuration from the data source.
     *
     * @return a future resolving to the configuration data
     */
    CompletableFuture<ConfigData> fetchConfig();

    /**
     * Fetch configuration with context/prefix filters.
     *
     * @param context optional context for filtering
     * @param prefixFilter optional prefix filter for filtering config keys
     * @return a future resolving to the filtered configuration data
     */
    CompletableFuture<ConfigData> fetchFilteredConfig(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    );

    /**
     * Fetch all active experiment data.
     *
     * @return a future resolving to optional experiment data
     */
    CompletableFuture<Optional<ExperimentData>> fetchActiveExperiments();

    /**
     * Fetch active experiments filtered with partial context matching.
     *
     * @param context optional context for filtering
     * @param prefixFilter optional prefix filter
     * @return a future resolving to optional experiment data
     */
    CompletableFuture<Optional<ExperimentData>> fetchCandidateActiveExperiments(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    );

    /**
     * Fetch active experiments filtered with exact context matching.
     *
     * @param context optional context for filtering
     * @param prefixFilter optional prefix filter
     * @return a future resolving to optional experiment data
     */
    CompletableFuture<Optional<ExperimentData>> fetchMatchingActiveExperiments(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    );

    /**
     * Check if this data source supports experiments.
     *
     * @return true if experiments are supported
     */
    boolean supportsExperiments();

    /**
     * Close and cleanup resources.
     *
     * @return a future that completes when cleanup is done
     */
    CompletableFuture<Void> close();
}
