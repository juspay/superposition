package io.juspay.superposition.client.auth;

import software.amazon.smithy.java.auth.api.AuthProperties;
import software.amazon.smithy.java.auth.api.identity.TokenIdentity;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResolver;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResult;

import java.util.concurrent.CompletableFuture;

/**
 * IdentityResolver implementation for Bearer token authentication.
 *
 * <p>This resolver validates the token and creates a TokenIdentity that will be used
 * by the Smithy auth framework to add the {@code Authorization: Bearer <token>} header.</p>
 *
 * <p>Bearer tokens are commonly used for API authentication and are defined in
 * <a href="https://datatracker.ietf.org/doc/html/rfc6750">RFC 6750</a>.</p>
 *
 * <h2>Example Usage:</h2>
 * <pre>{@code
 * import io.juspay.superposition.client.SuperpositionAsyncClient;
 * import io.juspay.superposition.client.auth.BearerTokenIdentityResolver;
 * import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;
 *
 * // Create a client with bearer token authentication
 * SuperpositionAsyncClient client = SuperpositionAsyncClient.builder()
 *     .endpointResolver(EndpointResolver.staticEndpoint("https://api.example.com"))
 *     .addIdentityResolver(new BearerTokenIdentityResolver("your-api-token"))
 *     .build();
 *
 * // Make API calls - token is automatically included in Authorization header
 * var response = client.getConfig(getConfigInput).get();
 * }</pre>
 *
 * <h2>Error Handling:</h2>
 * The constructor validates the token and throws {@link IllegalArgumentException}
 * if the token is null or empty:
 *
 * <pre>{@code
 * try {
 *     new BearerTokenIdentityResolver(null);
 * } catch (IllegalArgumentException e) {
 *     System.err.println("Token required: " + e.getMessage());
 * }
 * }</pre>
 *
 * @see <a href="https://datatracker.ietf.org/doc/html/rfc6750">RFC 6750 - The OAuth 2.0 Bearer Token Usage</a>
 * @see BasicAuthIdentityResolver
 */
@SuppressWarnings("rawtypes")
public class BearerTokenIdentityResolver implements IdentityResolver {
    private final TokenIdentity identity;

    /**
     * Creates a new Bearer token identity resolver.
     *
     * @param token the bearer token for authentication. Must not be null or empty.
     * @throws IllegalArgumentException if token is null or empty (after trimming)
     */
    public BearerTokenIdentityResolver(String token) {
        if (token == null || token.trim().isEmpty()) {
            throw new IllegalArgumentException("Bearer token cannot be null or empty");
        }
        this.identity = TokenIdentity.create(token);
    }

    @Override
    public CompletableFuture<IdentityResult> resolveIdentity(AuthProperties requestProperties) {
        return CompletableFuture.completedFuture(IdentityResult.of(identity));
    }

    @Override
    public Class identityType() {
        return TokenIdentity.class;
    }
}
