package io.juspay.superposition.openfeature.data_source;

import io.juspay.superposition.openfeature.error.SuperpositionError;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Interface for abstracting data sources that provide configuration and experiment data.
 * Different implementations can fetch from various backends:
 * - HTTP API (HttpDataSource)
 * - Local JSON/TOML files (FileDataSource)
 * - Redis (future)
 * - Database (future)
 * All methods support conditional fetching via {@code ifModifiedSince} timestamps,
 * enabling efficient caching using HTTP 304 Not Modified responses.
 *
 * <p>Context maps carry <em>JSON-encoded</em> values (e.g. {@code "42"}, {@code "\"android\""}),
 * matching the encoding used across the FFI boundary, so numbers and booleans survive the
 * round trip instead of degrading to strings.
 */
public interface SuperpositionDataSource {

    /**
     * Fetch the full resolved configuration from the data source.
     *
     * @param ifModifiedSince if provided, only return data if modified since this timestamp
     * @return FetchResponse containing ConfigData or NotModified
     * @throws SuperpositionError if the fetch fails
     */
    default FetchResponse<ConfigData> fetchConfig(Optional<Instant> ifModifiedSince)
            throws SuperpositionError {
        return fetchFilteredConfig(Optional.empty(), Optional.empty(), ifModifiedSince);
    }

    /**
     * Fetch configuration filtered by context and key prefixes.
     *
     * @param context optional dimension values (JSON-encoded) to filter contexts by
     * @param prefixFilter optional list of key prefixes (only keys starting with these are returned)
     * @param ifModifiedSince optional timestamp for conditional fetching
     * @return FetchResponse containing filtered ConfigData or NotModified
     * @throws SuperpositionError if the fetch fails
     */
    FetchResponse<ConfigData> fetchFilteredConfig(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince)
            throws SuperpositionError;

    /**
     * Fetch all active experiments.
     *
     * @param ifModifiedSince optional timestamp for conditional fetching
     * @return FetchResponse containing ExperimentData or NotModified
     * @throws SuperpositionError if the fetch fails
     */
    FetchResponse<ExperimentData> fetchActiveExperiments(Optional<Instant> ifModifiedSince)
            throws SuperpositionError;

    /**
     * Fetch active experiments whose conditions are candidate matches for the given context.
     *
     * Uses exact dimension matching strategy: experiment condition must exactly match
     * dimension values in the context.
     *
     * @param context optional dimension values (JSON-encoded) to match against
     * @param prefixFilter optional list of key prefixes to filter experiments
     * @param ifModifiedSince optional timestamp for conditional fetching
     * @return FetchResponse containing filtered ExperimentData or NotModified
     * @throws SuperpositionError if the fetch fails
     */
    FetchResponse<ExperimentData> fetchCandidateActiveExperiments(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince)
            throws SuperpositionError;

    /**
     * Fetch active experiments that match the given context.
     *
     * Uses subset dimension matching strategy: context dimensions must be a subset of
     * experiment condition dimensions.
     *
     * @param context optional dimension values (JSON-encoded) to match against
     * @param prefixFilter optional list of key prefixes to filter experiments
     * @param ifModifiedSince optional timestamp for conditional fetching
     * @return FetchResponse containing filtered ExperimentData or NotModified
     * @throws SuperpositionError if the fetch fails
     */
    FetchResponse<ExperimentData> fetchMatchingActiveExperiments(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince)
            throws SuperpositionError;

    /**
     * Check if this data source supports experiments.
     *
     * @return true if experiments are supported, false otherwise
     */
    boolean supportsExperiments();

    /**
     * Set up file watching for changes and return a stream of notifications.
     *
     * Only data sources that support watching (e.g., FileDataSource) will return
     * a non-empty Optional. The default implementation returns empty (no watching).
     *
     * @return Optional containing a WatchStream if watching is supported, empty otherwise
     * @throws SuperpositionError if watching setup fails
     */
    default Optional<WatchStream> watch() throws SuperpositionError {
        return Optional.empty();
    }

    /**
     * Clean up resources held by this data source.
     *
     * Called when the provider is shutting down or switching data sources.
     *
     * @throws SuperpositionError if cleanup fails
     */
    void close() throws SuperpositionError;
}
