# OpenFeature Provider Reference

Complete configuration and usage reference for Superposition OpenFeature providers.

## Table of Contents

- [Provider Configuration](#provider-configuration)
- [Evaluation Context](#evaluation-context)
- [Evaluation Methods](#evaluation-methods)
- [Refresh Strategies](#refresh-strategies)
- [Experiments Support](#experiments-support)
- [Error Handling](#error-handling)
- [Advanced Configuration](#advanced-configuration)

---

## Provider Configuration

### Java Configuration

```java
import io.juspay.superposition.openfeature.*;
import io.juspay.superposition.openfeature.options.*;

SuperpositionProviderOptions options = SuperpositionProviderOptions.builder()
    // Required
    .orgId("myorg")                          // Organization ID
    .workspaceId("production")               // Workspace ID
    .endpoint("https://superposition.example.com")  // Superposition endpoint
    .token("your-api-token")                 // API token

    // Optional - Refresh
    .refreshStrategy(RefreshStrategy.Polling.of(30000, 10000))  // 30s timeout, 10s interval

    // Optional - Fallback
    .fallbackConfig(Map.of(
        "feature_x", false,
        "max_items", 100
    ))

    // Optional - Experiments
    .experimentationOptions(
        SuperpositionProviderOptions.ExperimentationOptions.builder()
            .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))  // 10s timeout, 5s interval
            .build()
    )

    .build();

SuperpositionOpenFeatureProvider provider = new SuperpositionOpenFeatureProvider(options);
```

### JavaScript Configuration

```typescript
import { SuperpositionProvider } from 'superposition-provider';

interface ProviderConfig {
  // Required
  endpoint: string;        // Superposition endpoint
  org_id: string;          // Organization ID
  workspace_id: string;    // Workspace ID
  token: string;           // API token

  // Optional
  refresh_strategy?: {
    interval: number;      // Polling interval in seconds
    timeout: number;       // Request timeout in seconds
  };
  fallback_config?: Record<string, any>;  // Fallback configuration
  experimentation_options?: {
    refresh_strategy?: {
      interval: number;
      timeout: number;
    };
  };
}

const provider = new SuperpositionProvider({
  endpoint: 'https://superposition.example.com',
  org_id: 'myorg',
  workspace_id: 'production',
  token: 'your-api-token',
  refresh_strategy: {
    interval: 10,
    timeout: 30,
  },
  fallback_config: {
    feature_x: false,
    max_items: 100,
  },
  experimentation_options: {
    refresh_strategy: {
      interval: 5,
      timeout: 10,
    },
  },
});
```

### Python Configuration

```python
from superposition_provider.provider import SuperpositionProvider
from superposition_provider.types import (
    SuperpositionProviderOptions,
    PollingStrategy,
    ExperimentationOptions,
)

options = SuperpositionProviderOptions(
    # Required
    endpoint="https://superposition.example.com",
    org_id="myorg",
    workspace_id="production",
    token="your-api-token",

    # Optional
    refresh_strategy=PollingStrategy(
        interval=10,  # seconds
        timeout=30,   # seconds
    ),
    fallback_config={
        "feature_x": False,
        "max_items": 100,
    },
    experimentation_options=ExperimentationOptions(
        refresh_strategy=PollingStrategy(
            interval=5,
            timeout=10,
        ),
    ),
)

provider = SuperpositionProvider(provider_options=options)
```

### Rust Configuration

```rust
use superposition_provider::{
    SuperpositionProvider, SuperpositionProviderOptions,
    RefreshStrategy, PollingStrategy,
};

let options = SuperpositionProviderOptions {
    // Required
    endpoint: "https://superposition.example.com/".to_string(),
    org_id: "myorg".to_string(),
    workspace_id: "production".to_string(),
    token: "your-api-token".to_string(),

    // Optional
    refresh_strategy: RefreshStrategy::Polling(PollingStrategy {
        interval: 10,
        timeout: Some(30),
    }),
    fallback_config: Some(serde_json::json!({
        "feature_x": false,
        "max_items": 100
    })),
    evaluation_cache: None,
    experimentation_options: None,
};

let provider = SuperpositionProvider::new(options);
```

---

## Evaluation Context

Evaluation context provides dimensions for configuration resolution.

### Java

```java
import dev.openfeature.sdk.*;

// Simple context
EvaluationContext ctx = new ImmutableContext();

// With attributes
EvaluationContext ctx = new ImmutableContext(Map.of(
    "city", new Value("Delhi"),
    "user_tier", new Value("gold"),
    "platform", new Value("mobile")
));

// With targeting key (for experiments)
EvaluationContext ctx = new ImmutableContext(
    "user-123",  // targeting key
    Map.of(
        "city", new Value("Delhi"),
        "user_tier", new Value("gold")
    )
);

// Nested values
EvaluationContext ctx = new ImmutableContext(Map.of(
    "user", new Value(Map.of(
        "id", new Value("user-123"),
        "tier", new Value("gold")
    ))
));
```

### JavaScript

```typescript
// Simple context
const context = {};

// With attributes
const context = {
  city: 'Delhi',
  user_tier: 'gold',
  platform: 'mobile',
};

// With targeting key (for experiments)
const context = {
  targetingKey: 'user-123',
  city: 'Delhi',
  user_tier: 'gold',
};

// Nested values
const context = {
  user: {
    id: 'user-123',
    tier: 'gold',
  },
};
```

### Python

```python
from openfeature.evaluation_context import EvaluationContext

# Simple context
ctx = EvaluationContext()

# With attributes
ctx = EvaluationContext(attributes={
    "city": "Delhi",
    "user_tier": "gold",
    "platform": "mobile",
})

# With targeting key (for experiments)
ctx = EvaluationContext(
    targeting_key="user-123",
    attributes={
        "city": "Delhi",
        "user_tier": "gold",
    },
)
```

### Rust

```rust
use std::collections::HashMap;

// Simple context
let context: HashMap<String, serde_json::Value> = HashMap::new();

// With attributes
let context = HashMap::from([
    ("city".to_string(), "Delhi".into()),
    ("user_tier".to_string(), "gold".into()),
    ("platform".to_string(), "mobile".into()),
]);
```

---

## Evaluation Methods

### Boolean Evaluation

**Java:**
```java
// Simple evaluation
boolean value = client.getBooleanValue("dark-mode", false, ctx);

// Detailed evaluation
FlagEvaluationDetails<Boolean> details = client.getBooleanDetails("dark-mode", false, ctx);
System.out.println("Value: " + details.getValue());
System.out.println("Variant: " + details.getVariant());
System.out.println("Reason: " + details.getReason());
```

**JavaScript:**
```typescript
// Simple evaluation
const value = await client.getBooleanValue('dark-mode', false, context);

// Detailed evaluation
const details = await client.getBooleanDetails('dark-mode', false, context);
console.log('Value:', details.value);
console.log('Variant:', details.variant);
console.log('Reason:', details.reason);
```

**Python:**
```python
# Simple evaluation
value = client.get_boolean_value("dark-mode", False, ctx)

# Detailed evaluation
details = client.get_boolean_details(
    flag_key="dark-mode",
    default_value=False,
    evaluation_context=ctx,
)
print(f"Value: {details.value}")
print(f"Variant: {details.variant}")
print(f"Reason: {details.reason}")
```

**Rust:**
```rust
// Simple evaluation
let value = client.get_bool_value("dark-mode", Some(false), Some(context)).await?;

// Detailed evaluation
let details = client.get_bool_details("dark-mode", Some(false), Some(context)).await?;
println!("Value: {:?}", details.value);
println!("Variant: {:?}", details.variant);
```

### String Evaluation

**Java:**
```java
String theme = client.getStringValue("ui_theme", "light", ctx);

FlagEvaluationDetails<String> details = client.getStringDetails("ui_theme", "light", ctx);
```

**JavaScript:**
```typescript
const theme = await client.getStringValue('ui_theme', 'light', context);
const details = await client.getStringDetails('ui_theme', 'light', context);
```

**Python:**
```python
theme = client.get_string_value("ui_theme", "light", ctx)
details = client.get_string_details("ui_theme", "light", ctx)
```

**Rust:**
```rust
let theme = client.get_string_value("ui_theme", Some("light"), Some(context)).await?;
```

### Number/Integer Evaluation

**Java:**
```java
int maxItems = client.getIntegerValue("max_items", 100, ctx);
double discount = client.getDoubleValue("discount", 0.0, ctx);
```

**JavaScript:**
```typescript
const maxItems = await client.getNumberValue('max_items', 100, context);
```

**Python:**
```python
max_items = client.get_integer_value("max_items", 100, ctx)
discount = client.get_number_value("discount", 0.0, ctx)
```

**Rust:**
```rust
let max_items = client.get_int_value("max_items", Some(100), Some(context)).await?;
let discount = client.get_float_value("discount", Some(0.0), Some(context)).await?;
```

### Object Evaluation

**Java:**
```java
Value config = client.getObjectValue("app_config", new Value(Map.of("timeout", 30)), ctx);
Map<String, Object> configMap = config.asObject();
```

**JavaScript:**
```typescript
const config = await client.getObjectValue('app_config', { timeout: 30 }, context);
```

**Python:**
```python
config = client.get_object_value("app_config", {"timeout": 30}, ctx)
```

### Get All Configuration

**Java:**
```java
Map<String, Object> allConfig = provider.evaluateConfig(ctx);
```

**JavaScript:**
```typescript
const allConfig = await provider.resolveAllConfigDetails({}, context);
```

**Python:**
```python
all_config = await provider.resolve_all_config_details({}, ctx)
```

---

## Refresh Strategies

### Polling Strategy

Polling periodically fetches configuration from the server.

**Java:**
```java
import io.juspay.superposition.openfeature.options.RefreshStrategy;

// 30 second timeout, 10 second interval
RefreshStrategy.Polling.of(30000, 10000)
```

**JavaScript:**
```typescript
const provider = new SuperpositionProvider({
  refresh_strategy: {
    interval: 10,  // Poll every 10 seconds
    timeout: 30,   // 30 second timeout
  },
});
```

**Python:**
```python
from superposition_provider.types import PollingStrategy

PollingStrategy(interval=10, timeout=30)
```

**Rust:**
```rust
use superposition_provider::{RefreshStrategy, PollingStrategy};

RefreshStrategy::Polling(PollingStrategy {
    interval: 10,
    timeout: Some(30),
})
```

### Recommended Polling Intervals

| Use Case | Interval | Timeout |
|----------|----------|---------|
| Real-time applications | 5-10s | 5s |
| Web services | 10-30s | 10s |
| Batch jobs | 60s | 30s |
| Mobile apps | 60-300s | 30s |

---

## Experiments Support

### Experiment Configuration

Experiments require separate refresh configuration:

**Java:**
```java
SuperpositionProviderOptions options = SuperpositionProviderOptions.builder()
    .experimentationOptions(
        SuperpositionProviderOptions.ExperimentationOptions.builder()
            .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))  // 10s timeout, 5s interval
            .build()
    )
    .build();
```

**JavaScript:**
```typescript
const provider = new SuperpositionProvider({
  experimentation_options: {
    refresh_strategy: {
      interval: 5,
      timeout: 10,
    },
  },
});
```

**Python:**
```python
from superposition_provider.types import ExperimentationOptions

options = SuperpositionProviderOptions(
    experimentation_options=ExperimentationOptions(
        refresh_strategy=PollingStrategy(interval=5, timeout=10),
    ),
)
```

### Variant Detection

When a flag is part of an experiment, the variant is returned:

**Java:**
```java
FlagEvaluationDetails<Boolean> details = client.getBooleanDetails("feature-x", false, ctx);
if (details.getReason() == Reason.TARGETING_MATCH) {
    System.out.println("Variant: " + details.getVariant());  // "control" or "treatment"
}
```

**JavaScript:**
```typescript
const details = await client.getBooleanDetails('feature-x', false, context);
if (details.reason === 'TARGETING_MATCH') {
  console.log('Variant:', details.variant);
}
```

**Python:**
```python
details = client.get_boolean_details("feature-x", False, ctx)
if details.reason == "TARGETING_MATCH":
    print(f"Variant: {details.variant}")
```

---

## Error Handling

### Error Codes

| Code | Description |
|------|-------------|
| `PROVIDER_NOT_READY` | Provider not initialized |
| `FLAG_NOT_FOUND` | Flag doesn't exist |
| `PARSE_ERROR` | Error parsing response |
| `TYPE_MISMATCH` | Value type doesn't match expected |
| `NETWORK_ERROR` | Network connectivity issue |
| `TIMEOUT` | Request timed out |

### Java Error Handling

```java
import dev.openfeature.sdk.exceptions.*;

try {
    boolean value = client.getBooleanValue("feature-x", false, ctx);
} catch (FlagNotFoundError e) {
    System.err.println("Flag not found: " + e.getMessage());
} catch (TypeMismatchError e) {
    System.err.println("Type mismatch: " + e.getMessage());
} catch (Exception e) {
    System.err.println("General error: " + e.getMessage());
}

// Check evaluation details for errors
FlagEvaluationDetails<Boolean> details = client.getBooleanDetails("feature-x", false, ctx);
if (details.getErrorCode() != null) {
    System.err.println("Error: " + details.getErrorCode());
    System.err.println("Message: " + details.getErrorMessage());
}
```

### JavaScript Error Handling

```typescript
import { OpenFeatureError } from '@openfeature/server-sdk';

try {
  const value = await client.getBooleanValue('feature-x', false, context);
} catch (error) {
  if (error instanceof OpenFeatureError) {
    console.error(`Error (${error.code}): ${error.message}`);
  }
}

// Check evaluation details
const details = await client.getBooleanDetails('feature-x', false, context);
if (details.errorCode) {
  console.error(`Error: ${details.errorCode}`);
  console.error(`Message: ${details.errorMessage}`);
}
```

### Python Error Handling

```python
from openfeature.exception import (
    OpenFeatureError,
    FlagNotFoundError,
    TypeMismatchError,
)

try:
    value = client.get_boolean_value("feature-x", False, ctx)
except FlagNotFoundError as e:
    print(f"Flag not found: {e}")
except TypeMismatchError as e:
    print(f"Type mismatch: {e}")
except OpenFeatureError as e:
    print(f"Error: {e}")

# Check evaluation details
details = client.get_boolean_details("feature-x", False, ctx)
if details.error_code:
    print(f"Error: {details.error_code}")
    print(f"Message: {details.error_message}")
```

---

## Advanced Configuration

### With Fallback Config

Provide a fallback when server is unavailable:

**Java:**
```java
Map<String, Object> fallback = Map.of(
    "feature_x", false,
    "max_items", 100,
    "timeout_ms", 30000
);

SuperpositionProviderOptions options = SuperpositionProviderOptions.builder()
    .fallbackConfig(fallback)
    .build();
```

**JavaScript:**
```typescript
const provider = new SuperpositionProvider({
  // ...
  fallback_config: {
    feature_x: false,
    max_items: 100,
    timeout_ms: 30000,
  },
});
```

### With Evaluation Cache

Cache evaluation results:

**Java:**
```java
import io.juspay.superposition.openfeature.options.EvaluationCacheOptions;

EvaluationCacheOptions cacheOptions = EvaluationCacheOptions.builder()
    .maxSize(1000)
    .ttl(Duration.ofMinutes(5))
    .build();

SuperpositionProviderOptions options = SuperpositionProviderOptions.builder()
    .evaluationCache(cacheOptions)
    .build();
```

### Browser Configuration

For client-side JavaScript:

```typescript
import { SuperpositionProvider } from 'superposition-provider';

const provider = new SuperpositionProvider({
  endpoint: 'https://superposition.example.com',
  org_id: 'myorg',
  workspace_id: 'production',
  token: 'public-safe-token',  // Use a public token

  // Browser-specific
  fetch_options: {
    credentials: 'include',
    headers: {
      'X-Custom-Header': 'value',
    },
  },
});
```

---

## Provider Initialization

### Async Initialization

**Java:**
```java
// Synchronous (blocking)
provider.initialize(initCtx);

// The provider is ready immediately after initialize() returns
```

**JavaScript:**
```typescript
// Wait for initialization
await OpenFeature.setProviderAndWait(provider);

// Or non-blocking
OpenFeature.setProvider(provider);
// Provider will be ready eventually
```

**Python:**
```python
# Async initialization
await provider.initialize(context=ctx)
api.set_provider(provider)
```

**Rust:**
```rust
// Set provider (initialization happens async)
api.set_provider(provider).await;

// Wait for initialization
tokio::time::sleep(Duration::from_secs(2)).await;
```

### Check Provider Status

**JavaScript:**
```typescript
const status = OpenFeature.getClient().providerStatus;
// 'NOT_READY' | 'READY' | 'ERROR'
```

**Python:**
```python
from openfeature import api

status = api.get_client().provider_status
# ProviderStatus.NOT_READY | READY | ERROR
```

---

## Complete Examples

### Java Full Setup

```java
import dev.openfeature.sdk.*;
import io.juspay.superposition.openfeature.*;
import io.juspay.superposition.openfeature.options.*;

public class Application {
    private static final Client client;

    static {
        SuperpositionProviderOptions options = SuperpositionProviderOptions.builder()
            .orgId(System.getenv("SUPERPOSITION_ORG"))
            .workspaceId(System.getenv("SUPERPOSITION_WORKSPACE"))
            .endpoint(System.getenv("SUPERPOSITION_ENDPOINT"))
            .token(System.getenv("SUPERPOSITION_TOKEN"))
            .refreshStrategy(RefreshStrategy.Polling.of(30000, 10000))
            .experimentationOptions(
                SuperpositionProviderOptions.ExperimentationOptions.builder()
                    .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))  // 10s timeout, 5s interval
                    .build()
            )
            .build();

        SuperpositionOpenFeatureProvider provider = new SuperpositionOpenFeatureProvider(options);
        provider.initialize(new ImmutableContext());
        OpenFeatureAPI.getInstance().setProvider(provider);

        client = OpenFeatureAPI.getInstance().getClient("app");
    }

    public boolean isFeatureEnabled(String feature, String userId) {
        EvaluationContext ctx = new ImmutableContext(userId, Map.of());
        return client.getBooleanValue(feature, false, ctx);
    }
}
```

### Python FastAPI Integration

```python
from fastapi import FastAPI, Request
from openfeature import api
from openfeature.evaluation_context import EvaluationContext
from superposition_provider.provider import SuperpositionProvider
from superposition_provider.types import SuperpositionProviderOptions, PollingStrategy
import os

app = FastAPI()

@app.on_event("startup")
async def startup():
    provider = SuperpositionProvider(provider_options=SuperpositionProviderOptions(
        endpoint=os.environ["SUPERPOSITION_ENDPOINT"],
        org_id=os.environ["SUPERPOSITION_ORG"],
        workspace_id=os.environ["SUPERPOSITION_WORKSPACE"],
        token=os.environ["SUPERPOSITION_TOKEN"],
        refresh_strategy=PollingStrategy(interval=10, timeout=30),
    ))
    await provider.initialize(context=EvaluationContext())
    api.set_provider(provider)

@app.get("/feature/{feature_name}")
async def get_feature(feature_name: str, request: Request):
    user_id = request.headers.get("X-User-ID", "anonymous")
    ctx = EvaluationContext(targeting_key=user_id)

    client = api.get_client()
    value = client.get_boolean_value(feature_name, False, ctx)

    return {"feature": feature_name, "enabled": value}
```
