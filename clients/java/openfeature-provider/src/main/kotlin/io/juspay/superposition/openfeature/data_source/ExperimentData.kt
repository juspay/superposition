package io.juspay.superposition.openfeature.data_source

import uniffi.superposition_client.ExperimentConfig
import java.time.Instant

/**
 * Holds active experiments and experiment groups along with the timestamp they were fetched.
 *
 * @property data The experiment configuration data.
 * @property fetchedAt The timestamp when this data was last modified at its source.
 */
data class ExperimentData(val data: ExperimentConfig, val fetchedAt: Instant) {

    override fun toString(): String =
        "ExperimentData(data: ${data.experiments.size} experiments, " +
            "${data.experimentGroups.size} experiment groups" +
            ", fetched_at: $fetchedAt)"
}
