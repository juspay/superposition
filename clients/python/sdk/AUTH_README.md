# Superposition Python SDK - Authentication Guide

## ⚠️ Current State: Workaround Required

The Smithy Python code generator (version `0.0.1`) is currently in **early alpha** and does not automatically generate convenience methods for HTTP authentication like the Rust and TypeScript SDKs do.

### Comparison with Other SDKs:

#### Rust SDK (Mature) ✅

```rust
let config = SdkConfig::builder()
    .endpoint_url(&endpoint)
    .bearer_token(token.into())  // Built-in!
    .build();
```

#### TypeScript SDK (Mature) ✅

```typescript
const client = new SuperpositionClient({
    endpoint,
    token, // Built-in HttpBearerAuthSigner!
});
```

#### Python SDK (Alpha) ⚠️

```python
# Manual implementation required!
# See auth_helpers.py
```

## Solution: Use `auth_helpers.py`

We've created a **temporary workaround** module (`auth_helpers.py`) that provides the authentication implementations until smithy-python adds built-in support.

### Usage Example

#### Bearer Token Authentication (Recommended)

```python
from superposition_sdk.auth_helpers import bearer_auth_config
from superposition_sdk.client import Superposition
from superposition_sdk.config import Config

# Get bearer auth config
(resolver, schemes) = bearer_auth_config(token="your-bearer-token-here")

# Create config
config = Config(
    endpoint_uri="https://api.example.com",
    http_auth_scheme_resolver=resolver,
    http_auth_schemes=schemes
)

# Create client
client = Superposition(config)

# Make API calls
response = await client.get_config(input_data)
```

#### Basic Authentication

```python
from superposition_sdk.auth_helpers import basic_auth_config
from superposition_sdk.client import Superposition
from superposition_sdk.config import Config

# Get basic auth config
(resolver, schemes) = basic_auth_config(username="user", password="pass")

# Create config
config = Config(
    endpoint_uri="https://api.example.com",
    http_auth_scheme_resolver=resolver,
    http_auth_schemes=schemes
)

# Create client
client = Superposition(config)
```

## Architecture

The `auth_helpers.py` module implements:

### Bearer Token Authentication:

1. **`BearerTokenIdentity`** - Holds the bearer token
2. **`BearerTokenSigner`** - Adds `Authorization: Bearer {token}` header
3. **`BearerTokenResolver`** - Provides token identity
4. **`BearerAuthScheme`** - Complete auth scheme implementation
5. **`BearerAuthSchemeResolver`** - Resolves which auth to use
6. **`bearer_auth_config()`** - Returns tuple of (resolver, schemes dict)

### Basic Authentication:

1. **`BasicAuthIdentity`** - Holds username and password
2. **`BasicAuthSigner`** - Adds `Authorization: Basic {base64(user:pass)}` header
3. **`BasicAuthResolver`** - Provides basic auth identity
4. **`BasicAuthScheme`** - Complete auth scheme implementation
5. **`BasicAuthSchemeResolver`** - Resolves which auth to use
6. **`basic_auth_config()`** - Returns tuple of (resolver, schemes dict)

## Why Is This Necessary?

Smithy uses a plugin architecture where each language's code generator can implement features differently. The Python generator is newer and hasn't yet implemented the convenience methods for auth that other languages have.

### What Smithy Python SHOULD Generate (but doesn't yet):

```python
# This would be ideal (like Rust):
config = Config.builder()
    .endpoint_url(endpoint)
    .bearer_token(token)  # ← This doesn't exist yet!
    .build()
```

### What We Have To Do Instead:

Manually implement the full Smithy HTTP Auth protocol:

-   Identity classes
-   Signers
-   Resolvers
-   Auth schemes
-   Auth scheme resolvers

## Future

Once smithy-python reaches version `1.0` or later, it should include built-in auth support. At that point:

1. Remove `auth_helpers.py`
2. Update all code to use the built-in methods
3. Update this README

## For SDK Users

If you're using the Superposition Python SDK, always use the helper functions:

```python
from superposition_sdk.auth_helpers import bearer_auth_config
from superposition_sdk.config import Config

# Don't manually implement auth - use the helper!
(resolver, schemes) = bearer_auth_config(token="your-token-here")
config = Config(
    endpoint_uri="https://api.example.com",
    http_auth_scheme_resolver=resolver,
    http_auth_schemes=schemes
)
```

This ensures:

-   ✅ Consistent auth implementation across the codebase
-   ✅ Easier migration when smithy-python adds built-in support
-   ✅ Proper auth protocol implementation
-   ✅ Less boilerplate in your code

## References

-   [Smithy Authentication Traits](https://smithy.io/2.0/spec/authentication-traits.html)
-   [Smithy Python GitHub](https://github.com/smithy-lang/smithy-python)
-   [AWS Smithy Rust (for comparison)](https://github.com/smithy-lang/smithy-rs)
