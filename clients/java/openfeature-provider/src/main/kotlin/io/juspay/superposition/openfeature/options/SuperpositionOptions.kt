package io.juspay.superposition.openfeature.options

import io.juspay.superposition.client.auth.BasicAuthIdentityResolver
import io.juspay.superposition.client.auth.BearerTokenIdentityResolver
import software.amazon.smithy.java.client.core.auth.identity.IdentityResolver

/**
 * How to authenticate with the Superposition backend.
 */
sealed interface AuthMethod {

    /** The Smithy identity resolver that carries these credentials. */
    fun identityResolver(): IdentityResolver<*>

    /** Bearer-token authentication. */
    data class Token(val token: String) : AuthMethod {
        override fun identityResolver(): IdentityResolver<*> = BearerTokenIdentityResolver(token)
    }

    /** HTTP basic authentication. */
    data class Basic(val username: String, val password: String) : AuthMethod {
        override fun identityResolver(): IdentityResolver<*> =
            BasicAuthIdentityResolver(username, password)
    }
}

/**
 * Connection details for the Superposition backend.
 *
 * Used directly by data sources, or wrapped in provider options.
 *
 * @property endpoint The API endpoint (e.g. `http://localhost:8080`).
 * @property auth How to authenticate — a bearer token or basic credentials.
 * @property orgId Organization ID within Superposition.
 * @property workspaceId Workspace ID for the configuration set.
 */
data class SuperpositionOptions(
    val endpoint: String,
    val auth: AuthMethod,
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
        when (val method = auth) {
            is AuthMethod.Token -> check(method.token.isNotBlank()) { "token is required" }
            is AuthMethod.Basic -> {
                check(method.username.isNotBlank()) { "username is required" }
                check(method.password.isNotBlank()) { "password is required" }
            }
        }
        check(orgId.isNotBlank()) { "orgId is required" }
        check(workspaceId.isNotBlank()) { "workspaceId is required" }
    }
}
