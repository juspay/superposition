package io.juspay.superposition.openfeature.options;

/**
 * Options for the native (FFI) evaluation result cache.
 * Repeated resolutions with identical inputs are served from the Rust layer
 * without re-evaluating, and the cache is emptied whenever config or
 * experiment data is reloaded.
 */
public final class EvaluationCacheOptions {
    /** Unused: freshness is governed by the refresh strategy. */
    public int ttl;
    /** Memory budget for cached evaluations, in megabytes. Non-positive disables caching. */
    public int size;

    EvaluationCacheOptions(int ttl, int size) {
        this.ttl = ttl;
        this.size = size;
    }

    /**
     * Creates evaluation cache options with the given memory budget.
     * @param ttl unused, kept for API compatibility
     * @param size memory budget in megabytes; non-positive disables caching
     * @return a new EvaluationCacheOptions
     */
    public static EvaluationCacheOptions of(int ttl, int size) {
        return new EvaluationCacheOptions(ttl, size);
    }
}
