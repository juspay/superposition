# Superposition Java SDK - Authentication Guide

## Overview

The Superposition Java SDK is generated using Smithy, which provides a flexible authentication framework. This guide explains how to authenticate API requests using Bearer tokens or Basic authentication.

## Authentication Methods

### Bearer Token Authentication (Recommended)

Bearer token authentication is the recommended approach for API access. It provides a secure way to authenticate without sending credentials in every request.

#### Usage Example

```java
import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.client.auth.BearerTokenIdentityResolver;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;

// Create client with bearer token
SuperpositionAsyncClient client = SuperpositionAsyncClient.builder()
    .endpointResolver(EndpointResolver.staticEndpoint("https://api.example.com"))
    .addIdentityResolver(new BearerTokenIdentityResolver("your-bearer-token"))
    .build();

// Use client to make API calls
var result = client.getConfig(getConfigInput).get();
```

### Basic Authentication

Basic authentication encodes username and password in Base64 format for the `Authorization` header.

#### Usage Example

```java
import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.client.auth.BasicAuthIdentityResolver;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;

// Create client with basic auth
SuperpositionAsyncClient client = SuperpositionAsyncClient.builder()
    .endpointResolver(EndpointResolver.staticEndpoint("https://api.example.com"))
    .addIdentityResolver(new BasicAuthIdentityResolver("username", "password"))
    .build();

// Use client to make API calls
var result = client.getConfig(getConfigInput).get();
```

## Implementation Details

### File Structure

The SDK provides two authentication resolver implementations in separate files following Java conventions (one public class per file):

-   **`BearerTokenIdentityResolver.java`** - Bearer token authentication
-   **`BasicAuthIdentityResolver.java`** - HTTP Basic authentication

Both are located in the `io.juspay.superposition.client.auth` package.

### Why Custom Resolvers?

Smithy Java's code generator (v0.0.1+) includes the authentication framework but requires implementations of the `IdentityResolver` interface for specific authentication schemes. The Superposition SDK provides ready-made implementations:

#### `BearerTokenIdentityResolver`

-   **Purpose**: Authenticates using HTTP Bearer tokens (RFC 6750)
-   **Location**: `io.juspay.superposition.client.auth.BearerTokenIdentityResolver`
-   **File**: `BearerTokenIdentityResolver.java`
-   **Header Added**: `Authorization: Bearer {token}`
-   **Validation**: Ensures token is not null or empty

```java
public BearerTokenIdentityResolver(String token) {
    if (token == null || token.trim().isEmpty()) {
        throw new IllegalArgumentException("Bearer token cannot be null or empty");
    }
    // ...
}
```

#### `BasicAuthIdentityResolver`

-   **Purpose**: Authenticates using HTTP Basic Authentication (RFC 7617)
-   **Location**: `io.juspay.superposition.client.auth.BasicAuthIdentityResolver`
-   **File**: `BasicAuthIdentityResolver.java`
-   **Header Added**: `Authorization: Basic {base64(username:password)}`
-   **Validation**: Ensures username and password are not null or empty
-   **Encoding**: Automatically encodes credentials as Base64

```java
public BasicAuthIdentityResolver(String username, String password) {
    // Validates inputs
    // Encodes as Base64: username:password
    // Creates TokenIdentity
}
```

### Architecture

Both resolvers implement the Smithy `IdentityResolver` interface and follow the same pattern:

1. **Validation**: Input parameters are validated in the constructor
2. **Identity Creation**: A `TokenIdentity` is created with the appropriate credentials
3. **Resolution**: When a request is made, `resolveIdentity()` returns the identity
4. **Signing**: The Smithy framework uses the identity to add the appropriate `Authorization` header

```
Client Request
    ↓
IdentityResolver.resolveIdentity()
    ↓
TokenIdentity with credentials
    ↓
Smithy Framework adds Authorization header
    ↓
HTTP Request sent to API
```

## Error Handling

Both resolvers validate their inputs and throw `IllegalArgumentException` if invalid:

```java
try {
    var resolver = new BearerTokenIdentityResolver(null);
} catch (IllegalArgumentException e) {
    System.err.println(e.getMessage()); // "Bearer token cannot be null or empty"
}

try {
    var resolver = new BasicAuthIdentityResolver("", "password");
} catch (IllegalArgumentException e) {
    System.err.println(e.getMessage()); // "Username cannot be null or empty"
}
```

## Comparison with Other Smithy SDKs

### Rust SDK (Mature) ✅

```rust
let config = SdkConfig::builder()
    .endpoint_url(&endpoint)
    .bearer_token(token.into())  // Built-in!
    .build();
```

### TypeScript SDK (Mature) ✅

```typescript
const client = new SuperpositionClient({
    endpoint,
    token, // Built-in HttpBearerAuthSigner!
});
```

### Java SDK (Current Approach) 📋

```java
// Requires explicit resolver instantiation
client.builder()
    .addIdentityResolver(new BearerTokenIdentityResolver(token))
    .build();
```

The Java approach is more explicit but provides the same level of security and functionality.

## Best Practices

### 1. Use Environment Variables for Credentials

Never hardcode credentials. Use environment variables:

```java
String token = System.getenv("SUPERPOSITION_API_TOKEN");
SuperpositionAsyncClient client = SuperpositionAsyncClient.builder()
    .endpointResolver(EndpointResolver.staticEndpoint("https://api.example.com"))
    .addIdentityResolver(new BearerTokenIdentityResolver(token))
    .build();
```

### 2. Reuse Client Instances

Create the client once and reuse it for multiple requests:

```java
// ✅ Good - reuse across requests
SuperpositionAsyncClient client = createClient();
var result1 = client.getConfig(input1).get();
var result2 = client.getConfig(input2).get();

// ❌ Avoid - creating new client for each request
new BearerTokenIdentityResolver(token); // Don't do this repeatedly
```

### 3. Handle Validation Errors

Catch `IllegalArgumentException` when creating resolvers:

```java
try {
    var resolver = new BearerTokenIdentityResolver(userProvidedToken);
    // Use resolver...
} catch (IllegalArgumentException e) {
    log.error("Invalid token: {}", e.getMessage());
    // Handle error appropriately
}
```

### 4. Choose the Right Auth Method

-   **Bearer Token**: Use for service-to-service authentication, API tokens
-   **Basic Auth**: Use when the API requires username/password, development/testing

## OpenFeature Integration

The Superposition Java SDK integrates seamlessly with OpenFeature:

```java
var options = SuperpositionProviderOptions.builder()
    .endpoint("https://api.example.com")
    .token("your-bearer-token")  // Token is used internally
    .orgId("org-id")
    .workspaceId("workspace-id")
    .build();

SuperpositionOpenFeatureProvider provider =
    new SuperpositionOpenFeatureProvider(options);

OpenFeatureAPI.getInstance().setProvider(provider);
```

The provider handles creating the `BearerTokenIdentityResolver` automatically.

## Security Considerations

1. **Always use HTTPS**: Ensure API endpoint uses HTTPS protocol
2. **Protect tokens**: Never log or expose authentication tokens
3. **Rotate tokens regularly**: Follow your security policy for token rotation
4. **Use secure credential storage**: Store credentials in secure vaults, not config files
5. **Validate inputs**: Both resolvers validate their inputs - never bypass this

## Troubleshooting

### "Bearer token cannot be null or empty"

**Cause**: Token is `null` or an empty string

**Solution**: Verify token is correctly loaded from environment or configuration:

```java
String token = System.getenv("SUPERPOSITION_API_TOKEN");
if (token == null || token.trim().isEmpty()) {
    throw new IllegalStateException("SUPERPOSITION_API_TOKEN not set");
}
```

### "Username cannot be null or empty"

**Cause**: Username is `null` or empty string

**Solution**: Verify username is provided correctly

### "Password cannot be null"

**Cause**: Password is `null`

**Solution**: Password can be empty but not null. Provide an empty string if needed:

```java
var resolver = new BasicAuthIdentityResolver("user", ""); // Valid
var resolver = new BasicAuthIdentityResolver("user", null); // Invalid!
```

### Authentication Fails on Request

**Cause**: Token/credentials are valid but request is rejected

**Possible Solutions**:

1. Verify endpoint URL is correct
2. Check if token/credentials are still valid
3. Verify API permissions for the authenticated user
4. Check API documentation for required headers or parameters

## References

-   [RFC 6750 - Bearer Token Usage](https://datatracker.ietf.org/doc/html/rfc6750)
-   [RFC 7617 - HTTP Basic Authentication](https://datatracker.ietf.org/doc/html/rfc7617)
-   [Smithy Authentication Traits](https://smithy.io/2.0/spec/authentication-traits.html)
-   [Smithy Java GitHub](https://github.com/smithy-lang/smithy-java)

## See Also

-   [BearerTokenIdentityResolver.java](src/main/java/io/juspay/superposition/client/auth/BearerTokenIdentityResolver.java) - Bearer token implementation
-   [BasicAuthIdentityResolver.java](src/main/java/io/juspay/superposition/client/auth/BasicAuthIdentityResolver.java) - Basic auth implementation
-   [SuperpositionOpenFeatureProvider.java](../openfeature-provider/src/main/java/io/juspay/superposition/openfeature/SuperpositionOpenFeatureProvider.java) - Integration example
