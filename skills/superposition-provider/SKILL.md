---
name: superposition-provider
description: OpenFeature provider for consuming Superposition configurations in your application. Use when your app needs to read feature flags, evaluate configurations, or listen for config changes at runtime.
license: Apache-2.0
compatibility: Requires Superposition instance access and OpenFeature SDK
metadata:
  author: juspay
  version: "1.0"
---

# Superposition OpenFeature Provider

This skill helps you integrate Superposition with your application using the OpenFeature provider - the standard way to consume configurations at runtime.

## Why OpenFeature?

OpenFeature is like OpenTelemetry for feature flags - it makes your application agnostic to the underlying feature flag platform:

```javascript
// Your code stays the same regardless of provider
const flagValue = client.getBooleanValue('dark-mode', false);

// Switch providers without code changes
OpenFeature.setProvider(new SuperpositionProvider(config));
// or
OpenFeature.setProvider(new LaunchDarklyProvider(config));
// or
OpenFeature.setProvider(new UnleashProvider(config));
```

## Installation

### Java/Kotlin
```groovy
// build.gradle
implementation "dev.openfeature:sdk:1.15.1"
implementation "io.juspay.superposition:openfeature-provider:<version>"
```

### JavaScript/TypeScript
```bash
npm install @openfeature/server-sdk superposition-provider
```

### Python
```bash
pip install openfeature-sdk superposition-provider
```

### Rust
```toml
# Cargo.toml
[dependencies]
open-feature = "0.2"
superposition_provider = "0.100"
```

## Quick Start

### Java

```java
import dev.openfeature.sdk.*;
import io.juspay.superposition.openfeature.*;
import io.juspay.superposition.openfeature.options.RefreshStrategy;

public class AppConfig {
    public static void main(String[] args) {
        // Configure provider
        SuperpositionProviderOptions options = SuperpositionProviderOptions.builder()
            .orgId("myorg")
            .workspaceId("production")
            .endpoint("https://superposition.example.com")
            .token("your-api-token")
            .refreshStrategy(RefreshStrategy.Polling.of(30000, 10000)) // 30s timeout, 10s interval
            .build();

        // Initialize provider
        SuperpositionOpenFeatureProvider provider = new SuperpositionOpenFeatureProvider(options);
        EvaluationContext initCtx = new ImmutableContext();
        provider.initialize(initCtx);

        // Set as global provider
        OpenFeatureAPI.getInstance().setProvider(provider);

        // Get client
        Client client = OpenFeatureAPI.getInstance().getClient();

        // Evaluate feature flags
        boolean darkMode = client.getBooleanValue("dark-mode", false);
        int maxRetries = client.getIntegerValue("max-retries", 3);

        System.out.println("Dark mode enabled: " + darkMode);
    }
}
```

### Kotlin

```kotlin
import dev.openfeature.sdk.*
import io.juspay.superposition.openfeature.*
import io.juspay.superposition.openfeature.options.RefreshStrategy

fun main() {
    val options = SuperpositionProviderOptions.builder()
        .orgId("myorg")
        .workspaceId("production")
        .endpoint("https://superposition.example.com")
        .token("your-api-token")
        .refreshStrategy(RefreshStrategy.Polling.of(30000, 10000))
        .build()

    val provider = SuperpositionOpenFeatureProvider(options)
    provider.initialize(ImmutableContext())
    OpenFeatureAPI.getInstance().setProvider(provider)

    val client = OpenFeatureAPI.getInstance().client

    // Evaluate with context
    val ctx = ImmutableContext(mapOf(
        "city" to Value("Delhi"),
        "user_tier" to Value("gold")
    ))

    val discount = client.getDoubleValue("discount_percentage", 0.0, ctx)
    println("Your discount: $discount%")
}
```

### JavaScript/TypeScript

```typescript
import { OpenFeature } from '@openfeature/server-sdk';
import { SuperpositionProvider } from 'superposition-provider';

async function main() {
  // Configure provider
  const provider = new SuperpositionProvider({
    endpoint: 'https://superposition.example.com',
    org_id: 'myorg',
    workspace_id: 'production',
    token: 'your-api-token',
    refresh_strategy: {
      interval: 10,  // seconds
      timeout: 30,   // seconds
    },
  });

  // Initialize
  await OpenFeature.setProviderAndWait(provider);

  // Get client
  const client = OpenFeature.getClient();

  // Evaluate with context
  const context = {
    city: 'Delhi',
    user_tier: 'gold',
    platform: 'web',
  };

  const discount = await client.getNumberValue('discount_percentage', 0, context);
  console.log(`Your discount: ${discount}%`);

  // Get all config at once
  const allConfig = await provider.resolveAllConfigDetails({}, context);
  console.log('Full config:', allConfig);
}

main().catch(console.error);
```

### Python

```python
import asyncio
from openfeature import api
from openfeature.evaluation_context import EvaluationContext
from superposition_provider.provider import SuperpositionProvider
from superposition_provider.types import (
    SuperpositionProviderOptions,
    PollingStrategy,
)

async def main():
    # Configure provider
    options = SuperpositionProviderOptions(
        endpoint="https://superposition.example.com",
        token="your-api-token",
        org_id="myorg",
        workspace_id="production",
        refresh_strategy=PollingStrategy(
            interval=10,  # seconds
            timeout=30,   # seconds
        ),
    )

    provider = SuperpositionProvider(provider_options=options)

    # Initialize
    ctx = EvaluationContext(targeting_key="user-123", attributes={})
    await provider.initialize(context=ctx)
    api.set_provider(provider)

    # Get client
    client = api.get_client()

    # Evaluate with context
    eval_ctx = EvaluationContext(
        targeting_key="user-123",
        attributes={
            "city": "Delhi",
            "user_tier": "gold",
        }
    )

    discount = client.get_number_details(
        flag_key="discount_percentage",
        default_value=0.0,
        evaluation_context=eval_ctx,
    )
    print(f"Your discount: {discount.value}%")

    # Get full config
    full_config = await provider.resolve_all_config_details({}, eval_ctx)
    print(f"Full config: {full_config}")

asyncio.run(main())
```

### Rust

```rust
use superposition_provider::{
    SuperpositionProvider, SuperpositionProviderOptions, RefreshStrategy, PollingStrategy,
};
use open_feature::OpenFeature;
use std::collections::HashMap;

#[tokio::main]
async fn main() {
    // Configure provider
    let options = SuperpositionProviderOptions {
        endpoint: "https://superposition.example.com/".to_string(),
        token: "your-api-token".to_string(),
        org_id: "myorg".to_string(),
        workspace_id: "production".to_string(),
        refresh_strategy: RefreshStrategy::Polling(PollingStrategy {
            interval: 10,
            timeout: Some(30),
        }),
        ..Default::default()
    };

    // Set provider
    let mut api = OpenFeature::singleton_mut().await;
    api.set_provider(SuperpositionProvider::new(options)).await;

    // Wait for initialization
    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

    // Create client and evaluate
    let client = api.create_client();

    let context = HashMap::from([
        ("city".to_string(), "Delhi".into()),
        ("user_tier".to_string(), "gold".into()),
    ]);

    let discount = client.get_float_value("discount_percentage", Some(0.0), Some(context)).await;
    println!("Your discount: {}%", discount.unwrap_or(0.0));
}
```

## Evaluation Types

### Boolean Flags

```typescript
// JavaScript
const isEnabled = await client.getBooleanValue('feature-x', false, context);
if (isEnabled) {
  // Feature X logic
}
```

```java
// Java
boolean isEnabled = client.getBooleanValue("feature-x", false, context);
```

### String Values

```python
# Python
theme = client.get_string_details(
    flag_key="ui_theme",
    default_value="light",
    evaluation_context=ctx,
).value
```

### Number Values

```rust
// Rust
let max_items = client.get_int_value("max_items", Some(10), Some(context)).await?;
```

### Object Values

```typescript
// JavaScript
const settings = await client.getObjectValue('app_settings', { timeout: 30 }, context);
console.log(settings.timeout);
```

## Getting Full Configuration

Instead of evaluating individual flags, get the entire resolved config:

### Java
```java
Map<String, Object> fullConfig = provider.evaluateConfig(context);
System.out.println("Full config: " + fullConfig);
```

### JavaScript
```typescript
const fullConfig = await provider.resolveAllConfigDetails({}, context);
console.log('All keys:', Object.keys(fullConfig));
```

### Python
```python
full_config = await provider.resolve_all_config_details({}, ctx)
for key, value in full_config.items():
    print(f"{key}: {value}")
```

## Refresh Strategies

### Polling (Default)

Periodically fetch configuration updates:

```java
SuperpositionProviderOptions.builder()
    .refreshStrategy(RefreshStrategy.Polling.of(30000, 10000)) // timeout, interval in ms
    .build()
```

| Language | Interval Unit | Timeout Unit |
|----------|---------------|--------------|
| Java | milliseconds | milliseconds |
| JavaScript | seconds | seconds |
| Python | seconds | seconds |
| Rust | seconds | seconds |

### Configuration for High-Traffic Apps

```typescript
const provider = new SuperpositionProvider({
  endpoint: 'https://superposition.example.com',
  org_id: 'myorg',
  workspace_id: 'production',
  token: 'your-token',
  refresh_strategy: {
    interval: 30,  // Poll every 30 seconds
    timeout: 10,   // 10 second timeout
  },
  fallback_config: {  // Fallback if server unavailable
    feature_x: false,
    max_retries: 3,
  },
});
```

## Experiments Integration

When you need to participate in experiments, include a `targeting_key`:

### Python
```python
from openfeature.evaluation_context import EvaluationContext

# targeting_key is used for experiment variant assignment
ctx = EvaluationContext(
    targeting_key="user-123",  # User ID for consistent variant assignment
    attributes={
        "city": "Delhi",
        "platform": "web",
    }
)

# This will return the variant's override value if user is in experiment
checkout_steps = client.get_integer_details(
    flag_key="checkout_steps",
    default_value=5,
    evaluation_context=ctx,
)
print(f"Variant: {checkout_steps.variant}")  # "control" or "treatment"
```

### JavaScript
```typescript
const context = {
  targetingKey: 'user-123',  // For experiment consistency
  city: 'Delhi',
  platform: 'web',
};

const result = await client.getBooleanDetails('feature-x', false, context);
console.log('Value:', result.value);
console.log('Variant:', result.variant);  // Variant ID if in experiment
console.log('Reason:', result.reason);    // TARGETING_MATCH, DEFAULT, etc.
```

## Evaluation Details

Get detailed evaluation results including metadata:

### Java
```java
FlagEvaluationDetails<Boolean> details = client.getBooleanDetails("feature-x", false, context);

System.out.println("Value: " + details.getValue());
System.out.println("Variant: " + details.getVariant());
System.out.println("Reason: " + details.getReason());
System.out.println("Error: " + details.getErrorMessage());
```

### Python
```python
details = client.get_boolean_details(
    flag_key="feature-x",
    default_value=False,
    evaluation_context=ctx,
)

print(f"Value: {details.value}")
print(f"Variant: {details.variant}")
print(f"Reason: {details.reason}")
```

### Reason Values

| Reason | Meaning |
|--------|---------|
| `TARGETING_MATCH` | Context matched an override/experiment |
| `DEFAULT` | Using default value (no match) |
| `ERROR` | Error during evaluation |
| `DISABLED` | Feature flag disabled |

## Error Handling

### Java
```java
try {
    boolean value = client.getBooleanValue("feature-x", false, context);
} catch (Exception e) {
    // Falls back to default value automatically
    System.err.println("Error evaluating flag: " + e.getMessage());
}
```

### JavaScript
```typescript
try {
  const value = await client.getBooleanValue('feature-x', false, context);
} catch (error) {
  console.error('Flag evaluation failed:', error);
  // Default value is still returned
}
```

### Python
```python
try:
    value = client.get_boolean_value("feature-x", False, ctx)
except Exception as e:
    print(f"Error: {e}")
    # Falls back to default
```

## Best Practices

### 1. Use Targeting Keys for Experiments
```typescript
// Always set targeting_key for consistent experiment variant assignment
const context = {
  targetingKey: userId,  // Required for experiments
  ...otherContext,
};
```

### 2. Set Reasonable Timeouts
```java
// For real-time apps: shorter timeout
RefreshStrategy.Polling.of(5000, 5000)  // 5s timeout, 5s interval

// For batch jobs: longer timeout
RefreshStrategy.Polling.of(30000, 60000)  // 30s timeout, 60s interval
```

### 3. Provide Fallback Config
```typescript
const provider = new SuperpositionProvider({
  // ... other options
  fallback_config: {
    feature_x: false,
    max_items: 100,
    api_timeout: 30000,
  },
});
```

### 4. Handle Provider Events
```typescript
// JavaScript - Listen for config changes
provider.on('configuration_changed', (newConfig) => {
  console.log('Config updated:', newConfig);
});
```

## Related Skills

- [superposition-config](../superposition-config/) - Create the configs you're consuming
- [superposition-experiments](../superposition-experiments/) - Set up experiments
- [superposition-sdk](../superposition-sdk/) - For managing configs programmatically

See [references/REFERENCE.md](references/REFERENCE.md) for complete provider configuration options.
