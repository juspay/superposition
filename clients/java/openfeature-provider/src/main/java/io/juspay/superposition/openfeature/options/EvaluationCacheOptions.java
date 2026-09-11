package io.juspay.superposition.openfeature.options;

/**
 * Options for the native (FFI) evaluation result cache.
 * Repeated resolutions with identical inputs are served from the Rust layer
 * without re-evaluating, and the cache is emptied whenever config or
 * experiment data is reloaded.
 */
public final class EvaluationCacheOptions {
    /** Maximum number of cached resolutions. Non-positive disables caching. */
    public final int maxEntries;

    EvaluationCacheOptions(int maxEntries) {
        this.maxEntries = maxEntries;
    }

    /**
     * Creates evaluation cache options with the given maximum number of entries.
     * @param maxEntries maximum number of cached resolutions; non-positive disables caching
     * @return a new EvaluationCacheOptions
     */
    public static EvaluationCacheOptions of(int maxEntries) {
        return new EvaluationCacheOptions(maxEntries);
    }
}
