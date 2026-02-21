package io.juspay.superposition.provider;

/**
 * Sealed interface for defining how configuration data should be refreshed.
 */
public sealed interface RefreshStrategy {

    /**
     * Polling strategy that periodically refreshes configuration at a fixed interval.
     *
     * @param interval the polling interval in seconds
     */
    record Polling(long interval) implements RefreshStrategy {}

    /**
     * On-demand strategy that refreshes only when TTL expires.
     *
     * @param ttl the time-to-live in seconds
     * @param useStaleOnError whether to use stale data on error
     */
    record OnDemand(long ttl, boolean useStaleOnError) implements RefreshStrategy {}

    /**
     * Manual strategy that requires explicit refresh calls.
     */
    record Manual() implements RefreshStrategy {}
}
