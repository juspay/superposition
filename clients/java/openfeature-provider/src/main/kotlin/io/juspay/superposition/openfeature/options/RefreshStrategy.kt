package io.juspay.superposition.openfeature.options

/**
 * How a provider keeps its cached configuration current.
 *
 * All durations are milliseconds, named with a `Milliseconds` suffix to match the Rust and Python
 * clients. (`debounceMs` is the one exception, kept for its established name.)
 */
sealed interface RefreshStrategy {

    /** How long a single refresh may take before it is abandoned. */
    val timeoutMilliseconds: Int

    /**
     * Fetch periodically at a fixed interval, on a background task started at initialization.
     *
     * @property intervalMilliseconds How often to poll.
     */
    data class Polling
    @JvmOverloads constructor(
        override val timeoutMilliseconds: Int,
        val intervalMilliseconds: Int,
    ) : RefreshStrategy

    /**
     * Fetch lazily, when the cached data is older than its TTL.
     *
     * Keeps backend load down at the cost of a bounded amount of staleness. If a refresh fails and
     * [useStaleOnError] is set, the last known good data is served rather than failing the call.
     *
     * @property ttlMilliseconds How long cached data stays fresh.
     * @property useStaleOnError Whether to serve stale data when a refresh fails.
     */
    data class OnDemand
    @JvmOverloads constructor(
        override val timeoutMilliseconds: Int,
        val ttlMilliseconds: Int,
        val useStaleOnError: Boolean = true,
    ) : RefreshStrategy

    /**
     * Refresh when the underlying source signals a change. Only usable with a data source that
     * supports watching; a provider configured this way against one that does not fails to
     * initialize rather than silently never refreshing.
     *
     * @property debounceMs How long to coalesce a burst of rapid changes.
     */
    data class Watch
    @JvmOverloads constructor(
        override val timeoutMilliseconds: Int,
        val debounceMs: Int = 500,
    ) : RefreshStrategy

    /** Never refresh on its own; the caller drives it by invoking `refresh()`. */
    data class Manual(
        override val timeoutMilliseconds: Int,
    ) : RefreshStrategy
}
