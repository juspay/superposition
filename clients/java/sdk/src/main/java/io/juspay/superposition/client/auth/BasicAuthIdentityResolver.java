package io.juspay.superposition.client.auth;

import software.amazon.smithy.java.auth.api.AuthProperties;
import software.amazon.smithy.java.auth.api.identity.TokenIdentity;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResolver;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResult;

import java.util.concurrent.CompletableFuture;

/**
 * IdentityResolver implementation for HTTP Basic authentication.
 *
 * <p>This resolver validates the credentials, encodes them as base64(username:password),
 * and creates a TokenIdentity for the {@code Authorization: Basic <credentials>} header.</p>
 *
 * <p>Basic authentication is defined in <a href="https://datatracker.ietf.org/doc/html/rfc7617">RFC 7617</a>.
 * While simpler than bearer tokens, basic auth requires transmitting credentials with each request,
 * so HTTPS should always be used.</p>
 *
 * <h2>Example Usage:</h2>
 * <pre>{@code
 * import io.juspay.superposition.client.SuperpositionAsyncClient;
 * import io.juspay.superposition.client.auth.BasicAuthIdentityResolver;
 * import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;
 *
 * // Create a client with basic authentication
 * SuperpositionAsyncClient client = SuperpositionAsyncClient.builder()
 *     .endpointResolver(EndpointResolver.staticEndpoint("https://api.example.com"))
 *     .addIdentityResolver(new BasicAuthIdentityResolver("username", "password"))
 *     .build();
 *
 * // Make API calls - credentials are automatically included in Authorization header
 * var response = client.getConfig(getConfigInput).get();
 * }</pre>
 *
 * <h2>Error Handling:</h2>
 * The constructor validates the credentials and throws {@link IllegalArgumentException}
 * if username is null/empty or if password is null:
 *
 * <pre>{@code
 * try {
 *     new BasicAuthIdentityResolver("", "password");
 * } catch (IllegalArgumentException e) {
 *     System.err.println("Username required: " + e.getMessage());
 * }
 * }</pre>
 *
 * <h2>Security Considerations:</h2>
 * <ul>
 *     <li>Always use HTTPS when transmitting basic auth credentials</li>
 *     <li>Never hardcode credentials - use environment variables or secure vaults</li>
 *     <li>Basic auth credentials are sent with every request, so token rotation is important</li>
 *     <li>Consider using bearer tokens (BearerTokenIdentityResolver) for better security</li>
 * </ul>
 *
 * @see <a href="https://datatracker.ietf.org/doc/html/rfc7617">RFC 7617 - The 'Basic' HTTP Authentication Scheme</a>
 * @see BearerTokenIdentityResolver
 */
@SuppressWarnings("rawtypes")
public class BasicAuthIdentityResolver implements IdentityResolver {
    private final TokenIdentity identity;

    /**
     * Creates a new Basic authentication identity resolver.
     *
     * @param username the username for basic authentication. Must not be null or empty.
     * @param password the password for basic authentication. Must not be null (can be empty).
     * @throws IllegalArgumentException if username is null or empty (after trimming),
     *                                  or if password is null
     */
    public BasicAuthIdentityResolver(String username, String password) {
        if (username == null || username.trim().isEmpty()) {
            throw new IllegalArgumentException("Username cannot be null or empty");
        }
        if (password == null) {
            throw new IllegalArgumentException("Password cannot be null");
        }

        // For Basic auth, encode as base64(username:password)
        String credentials = username + ":" + password;
        String encodedCredentials = java.util.Base64.getEncoder()
            .encodeToString(credentials.getBytes(java.nio.charset.StandardCharsets.UTF_8));
        this.identity = TokenIdentity.create(encodedCredentials);
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
