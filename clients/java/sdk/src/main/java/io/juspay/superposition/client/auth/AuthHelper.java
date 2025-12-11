package io.juspay.superposition.client.auth;

import software.amazon.smithy.java.auth.api.AuthProperties;
import software.amazon.smithy.java.auth.api.identity.TokenIdentity;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResolver;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResult;

import java.util.concurrent.CompletableFuture;

/**
 * Authentication utilities for Superposition SDK.
 *
 * <p>Provides IdentityResolver implementations for Bearer token and Basic authentication
 * schemes supported by the Superposition API.</p>
 *
 * <h2>Example Usage - Bearer Token:</h2>
 * <pre>{@code
 * SuperpositionClient client = SuperpositionClient.builder()
 *     .endpointResolver(EndpointResolver.staticEndpoint("https://api.example.com"))
 *     .addIdentityResolver(new BearerTokenIdentityResolver("your-token-here"))
 *     .build();
 * }</pre>
 *
 * <h2>Example Usage - Basic Authentication:</h2>
 * <pre>{@code
 * SuperpositionClient client = SuperpositionClient.builder()
 *     .endpointResolver(EndpointResolver.staticEndpoint("https://api.example.com"))
 *     .addIdentityResolver(new BasicAuthIdentityResolver("username", "password"))
 *     .build();
 * }</pre>
 *
 * @see software.amazon.smithy.java.auth.api.identity.TokenIdentity
 * @see software.amazon.smithy.java.client.core.auth.identity.IdentityResolver
 */
public final class AuthHelper {

    private AuthHelper() {
        // Utility class - prevent instantiation
    }

    /**
     * IdentityResolver implementation for Bearer token authentication.
     *
     * <p>This resolver validates the token and creates a TokenIdentity that will be used
     * by the Smithy auth framework to add the {@code Authorization: Bearer <token>} header.</p>
     *
     * <h2>Example:</h2>
     * <pre>{@code
     * IdentityResolver resolver = new BearerTokenIdentityResolver("my-api-token");
     * SuperpositionClient client = SuperpositionClient.builder()
     *     .addIdentityResolver(resolver)
     *     .build();
     * }</pre>
     *
     * @see <a href="https://datatracker.ietf.org/doc/html/rfc6750">RFC 6750 - Bearer Token Usage</a>
     */
    @SuppressWarnings("rawtypes")
    public static class BearerTokenIdentityResolver implements IdentityResolver {
        private final TokenIdentity identity;

        /**
         * Creates a new Bearer token identity resolver.
         *
         * @param token the bearer token for authentication
         * @throws IllegalArgumentException if token is null or empty
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

    /**
     * IdentityResolver implementation for HTTP Basic authentication.
     *
     * <p>This resolver validates the credentials, encodes them as base64(username:password),
     * and creates a TokenIdentity for the {@code Authorization: Basic <credentials>} header.</p>
     *
     * <h2>Example:</h2>
     * <pre>{@code
     * IdentityResolver resolver = new BasicAuthIdentityResolver("admin", "password123");
     * SuperpositionClient client = SuperpositionClient.builder()
     *     .addIdentityResolver(resolver)
     *     .build();
     * }</pre>
     *
     * <p>Note: Basic auth in Smithy Java uses TokenIdentity where the token contains
     * the base64-encoded credentials.</p>
     *
     * @see <a href="https://datatracker.ietf.org/doc/html/rfc7617">RFC 7617 - HTTP Basic Authentication</a>
     */
    @SuppressWarnings("rawtypes")
    public static class BasicAuthIdentityResolver implements IdentityResolver {
        private final TokenIdentity identity;

        /**
         * Creates a new Basic authentication identity resolver.
         *
         * @param username the username for basic authentication
         * @param password the password for basic authentication
         * @throws IllegalArgumentException if username or password is null or empty
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
}
