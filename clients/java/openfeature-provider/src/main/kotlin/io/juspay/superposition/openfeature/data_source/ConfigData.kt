package io.juspay.superposition.openfeature.data_source

import uniffi.superposition_types.Config
import java.time.Instant

/**
 * Holds resolved configuration along with the timestamp it was fetched.
 *
 * Wraps [Config] (the UniFFI-generated Rust type) with a [fetchedAt] timestamp used to support
 * conditional fetching (HTTP 304 / If-Modified-Since). For HTTP sources this is the server's
 * `last-modified` time, not the local clock, so it can be sent straight back as
 * `if-modified-since`.
 *
 * [Config] contains:
 * - `contexts` — list of contexts matchable against an evaluation context
 * - `overrides` — per-context key/value overrides (`Map<String, Overrides>`)
 * - `defaultConfigs` — default key/value pairs (`ExtendedMap = Map<String, String>`)
 * - `dimensions` — dimension definitions (`Map<String, DimensionInfo>`)
 *
 * @property data The resolved configuration, directly from the UniFFI Rust bindings.
 * @property fetchedAt The timestamp when this config was last modified at its source.
 */
data class ConfigData(val data: Config, val fetchedAt: Instant) {

    override fun toString(): String =
        "ConfigData(fetched_at: $fetchedAt" +
            ", data.contexts: ${data.contexts.size}" +
            ", data.overrides: ${data.overrides.size}" +
            ", data.defaultConfigs: ${data.defaultConfigs.size}" +
            ", data.dimensions: ${data.dimensions.size})"
}
