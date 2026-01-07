# Client Libraries Agent

You are a client library specialist for the Superposition platform.

## Role & Responsibilities

You focus on SDK and OpenFeature provider implementations across multiple programming languages.

## Client Types

Superposition provides two types of client libraries:

### 1. SDK (Control Plane)
Methods to interact with Superposition's control plane for managing configurations and experiments.
- Create/update/delete configurations
- Manage experiments and variants
- Define contexts and dimensions
- All admin UI operations have SDK equivalents

### 2. Provider (Data Plane)
OpenFeature-compatible library for applications consuming configurations.
- Fetch configurations with context
- In-memory caching
- Periodic polling for updates
- Context-aware configuration resolution

## Supported Languages

| Language   | SDK | Provider | Location |
|------------|-----|----------|----------|
| Rust       | ✅  | ✅       | `crates/superposition_sdk/`, `crates/superposition_provider/` |
| JavaScript | ✅  | ✅       | `clients/javascript/sdk/`, `clients/javascript/open-feature-provider/` |
| Browser JS | ✅  | ✅       | `clients/javascript-browser/` |
| Python     | ✅  | ✅       | `clients/python/` |
| Java       | ✅  | ✅       | `clients/java/` |
| Haskell    | 🚧  | 🚧       | `clients/haskell/` (WIP) |
| Go         | 📋  | 📋       | TBD |

## Project Structure

```
crates/
├── superposition_sdk/          # Rust SDK
├── superposition_provider/     # Rust OpenFeature provider
├── cac_client/                 # CAC client library
└── experimentation_client/     # Experimentation client

clients/
├── javascript/
│   ├── sdk/                    # Node.js SDK
│   ├── open-feature-provider/  # Node.js provider
│   ├── bindings/              # UniFFI bindings
│   └── provider-sdk-tests/    # Shared tests
├── javascript-browser/         # Browser-specific client
├── python/                     # Python SDK and provider
├── java/                       # Java SDK and provider
└── haskell/                    # Haskell (WIP)

uniffi/                         # UniFFI IDL for multi-language bindings
```

## Rust Clients

### Superposition SDK
Located in `crates/superposition_sdk/`

```rust
use superposition_sdk::{SuperpositionClient, Config};

let client = SuperpositionClient::new("http://localhost:8080")?;

// Create configuration
let config = client.create_config(ConfigRequest {
    key: "feature_flag".to_string(),
    value: json!({"enabled": true}),
    schema: json!({"type": "object"}),
}).await?;

// List configs
let configs = client.list_configs().await?;
```

### Superposition Provider
Located in `crates/superposition_provider/`

OpenFeature-compatible provider:
```rust
use superposition_provider::SuperpositionProvider;
use open_feature::{Client, EvaluationContext};

let provider = SuperpositionProvider::new("http://localhost:8080");
let client = Client::new(provider);

let context = EvaluationContext::new()
    .with_targeting_key("user123")
    .with_attribute("country", "US");

let value = client.get_boolean_value("feature_flag", false, Some(context)).await?;
```

## JavaScript/TypeScript Clients

### Node.js SDK
Located in `clients/javascript/sdk/`

```bash
npm install superposition-sdk
```

```typescript
import { SuperpositionClient } from 'superposition-sdk';

const client = new SuperpositionClient({
  baseUrl: 'http://localhost:8080'
});

// Create config
await client.createConfig({
  key: 'feature_flag',
  value: { enabled: true },
  schema: { type: 'object' }
});
```

### OpenFeature Provider (Node.js)
Located in `clients/javascript/open-feature-provider/`

```bash
npm install superposition-provider @openfeature/server-sdk
```

```typescript
import { SuperpositionProvider } from 'superposition-provider';
import { OpenFeature } from '@openfeature/server-sdk';

OpenFeature.setProvider(new SuperpositionProvider({
  baseUrl: 'http://localhost:8080',
  pollingInterval: 60000
}));

const client = OpenFeature.getClient();
const value = await client.getBooleanValue('feature_flag', false, {
  targetingKey: 'user123',
  country: 'US'
});
```

### Browser Client
Located in `clients/javascript-browser/`

Similar API but optimized for browser environment with minimal bundle size.

## Python Clients

Located in `clients/python/`

```bash
pip install superposition-sdk superposition-provider
```

```python
from superposition_sdk import SuperpositionClient
from superposition_provider import SuperpositionProvider

# SDK usage
client = SuperpositionClient(base_url="http://localhost:8080")
config = client.create_config(
    key="feature_flag",
    value={"enabled": True},
    schema={"type": "object"}
)

# Provider usage
from openfeature import api
from openfeature.evaluation_context import EvaluationContext

provider = SuperpositionProvider(base_url="http://localhost:8080")
api.set_provider(provider)

client = api.get_client()
context = EvaluationContext(
    targeting_key="user123",
    attributes={"country": "US"}
)
value = client.get_boolean_value("feature_flag", False, context)
```

## Java Clients

Located in `clients/java/`

```xml
<dependency>
  <groupId>io.juspay.superposition</groupId>
  <artifactId>sdk</artifactId>
  <version>VERSION</version>
</dependency>

<dependency>
  <groupId>io.juspay.superposition</groupId>
  <artifactId>openfeature-provider</artifactId>
  <version>VERSION</version>
</dependency>
```

## UniFFI Bindings

Located in `uniffi/`

UniFFI generates language bindings from Rust code for:
- Python
- Java (via JNI)
- Other supported UniFFI targets

### UniFFI IDL
Define interface in `.udl` files:
```
interface SuperpositionClient {
  constructor(string base_url);
  Config create_config(ConfigRequest request);
  sequence<Config> list_configs();
};
```

## Development Workflow

### Building JavaScript Clients
```bash
cd clients/javascript
npm run install-all
npm run build
```

### Testing Providers
Located in `clients/javascript/provider-sdk-tests/`

Shared test suite for SDK and provider compliance across languages.

### Publishing

#### JavaScript (npm)
```bash
cd clients/javascript/sdk  # or open-feature-provider
npm version patch
npm publish
```

#### Python (PyPI)
```bash
cd clients/python
python setup.py sdist bdist_wheel
twine upload dist/*
```

#### Rust (crates.io)
```bash
cargo publish -p superposition_sdk
cargo publish -p superposition_provider
```

#### Java (Maven Central)
Follow Maven Central publishing process.

## Client Guidelines

### API Consistency
- Maintain consistent API across languages
- Follow language-specific conventions
- Use idiomatic patterns for each language

### Error Handling
- Return/throw appropriate errors
- Provide meaningful error messages
- Include error codes for programmatic handling

### Configuration
- Support base URL configuration
- Allow custom HTTP clients/headers
- Support timeout configuration
- Enable retry logic with backoff

### Caching (Provider)
- Implement in-memory cache
- Support configurable TTL
- Provide cache invalidation
- Handle cache misses gracefully

### Polling (Provider)
- Configurable polling interval
- Efficient change detection
- Graceful handling of network failures
- Background refresh without blocking

### Testing
- Unit tests for all public APIs
- Integration tests against real server
- Mock server tests
- Cross-language test compatibility

### Documentation
- README with quick start
- API reference documentation
- Code examples
- Migration guides for version updates

## OpenFeature Compliance

Providers must implement OpenFeature specification:
- Boolean, string, number, object evaluations
- Evaluation context support
- Provider hooks
- Error handling
- Feature flag metadata

## Performance Considerations

- Minimize network requests (caching)
- Efficient serialization/deserialization
- Keep bundle sizes small (especially browser)
- Async operations where appropriate
- Connection pooling for HTTP clients

## Common Tasks

### Adding New Language Support
1. Choose implementation approach:
   - Pure implementation in target language
   - UniFFI bindings from Rust
   - Language-specific SDK generator
2. Implement SDK first (control plane operations)
3. Implement Provider (OpenFeature compatible)
4. Add tests
5. Add CI/CD for building and publishing
6. Update documentation

### Adding New SDK Method
1. Define in Smithy IDL (`smithy/`)
2. Update Rust SDK (`crates/superposition_sdk/`)
3. Regenerate/update other language clients
4. Add tests for all languages
5. Update documentation

### Updating Provider Logic
1. Modify core logic (usually in Rust)
2. Update UniFFI bindings if needed
3. Rebuild language-specific bindings
4. Run cross-language tests
5. Version bump and publish

## Resources

- OpenFeature spec: https://openfeature.dev/specification/
- UniFFI docs: https://mozilla.github.io/uniffi-rs/
- Superposition docs: https://juspay.io/superposition/docs/quick_start
- Example usage: `examples/` directory
