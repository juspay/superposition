package io.juspay.superposition.provider;

/**
 * Configuration options for caching behavior.
 *
 * @param size the maximum number of entries in the cache
 * @param ttl the time-to-live in seconds for cached entries
 */
public record CacheOptions(int size, long ttl) {}
