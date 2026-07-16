package io.juspay.superposition.openfeature.options

/**
 * Connection details for the Superposition backend.
 *
 * Used directly by data sources, or wrapped in provider options.
 *
 * @property endpoint The API endpoint (e.g. `http://localhost:8080`).
 * @property token Bearer token for authentication.
 * @property orgId Organization ID within Superposition.
 * @property workspaceId Workspace ID for the configuration set.
 */
data class SuperpositionOptions(
    val endpoint: String,
    val token: String,
    val orgId: String,
    val workspaceId: String,
) {

    /**
     * Check that every field carries a value.
     *
     * @throws IllegalStateException if any field is blank
     */
    fun validate() {
        check(endpoint.isNotBlank()) { "endpoint is required" }
        check(token.isNotBlank()) { "token is required" }
        check(orgId.isNotBlank()) { "orgId is required" }
        check(workspaceId.isNotBlank()) { "workspaceId is required" }
    }
}
