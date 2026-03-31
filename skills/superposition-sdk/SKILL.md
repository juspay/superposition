---
name: superposition-sdk
description: Programmatic management of Superposition using SDKs in Rust, JavaScript, Python, and Java. Use when you need to create dimensions, manage configs, or run experiments from your code instead of the UI or curl.
license: Apache-2.0
compatibility: Requires Superposition instance access and appropriate package manager (cargo, npm, pip, or maven)
metadata:
  author: juspay
  version: "1.0"
---

# Superposition SDK

This skill helps you use Superposition SDKs to programmatically manage configurations and experiments from your application code.

## When to Use SDK vs Provider

| SDK | Provider |
|-----|----------|
| **Control plane** - manage configs, experiments | **Data plane** - consume configs |
| Create/update/delete dimensions | Evaluate feature flags |
| Create/update default configs | Get resolved configuration |
| Run experiments, ramp traffic | Listen for config changes |
| Requires admin credentials | Uses read-only token |

**Use SDK when:** Building admin tools, CI/CD pipelines, automation scripts
**Use Provider when:** Your app needs to read configs at runtime

## Installation

### Rust
```toml
# Cargo.toml
[dependencies]
superposition_sdk = "0.100"
tokio = { version = "1", features = ["full"] }
```

### JavaScript/TypeScript
```bash
npm install superposition-sdk
# or
yarn add superposition-sdk
```

### Python
```bash
pip install superposition-sdk
```

### Java/Kotlin
```groovy
// build.gradle
implementation "io.juspay.superposition:sdk:<version>"
```

```xml
<!-- pom.xml -->
<dependency>
    <groupId>io.juspay.superposition</groupId>
    <artifactId>sdk</artifactId>
    <version>VERSION</version>
</dependency>
```

## Quick Start Examples

### Rust

```rust
use superposition_sdk::{
    Configuration, CreateDimensionRequest, CreateDefaultConfigRequest,
    CreateExperimentRequest, Variant, VariantType
};
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize SDK client
    let config = Configuration::builder()
        .endpoint("http://localhost:8080")
        .org_id("localorg")
        .workspace_id("test")
        .token("your-api-token")
        .build()?;

    // Create a dimension
    let dimension = CreateDimensionRequest::builder()
        .dimension("city")
        .description("City for location-based config")
        .position(1)
        .schema(serde_json::json!({
            "type": "string",
            "enum": ["Bangalore", "Delhi", "Mumbai"]
        }))
        .change_reason("Adding city dimension")
        .build();

    config.create_dimension(dimension).await?;
    println!("Dimension created!");

    // Create default config
    let default_config = CreateDefaultConfigRequest::builder()
        .key("per_km_rate")
        .value(serde_json::json!({"value": 20.0}))
        .schema(serde_json::json!({"type": "number"}))
        .change_reason("Base rate configuration")
        .build();

    config.create_default_config(default_config).await?;

    Ok(())
}
```

### JavaScript/TypeScript

```typescript
import { Configuration, CreateDimensionRequest } from 'superposition-sdk';

// Initialize client
const config = new Configuration({
  endpoint: 'http://localhost:8080',
  orgId: 'localorg',
  workspaceId: 'test',
  token: 'your-api-token',
});

async function setupConfiguration() {
  // Create dimension
  await config.createDimension({
    dimension: 'user_tier',
    description: 'User membership tier',
    position: 2,
    schema: {
      type: 'string',
      enum: ['bronze', 'silver', 'gold', 'platinum'],
    },
    change_reason: 'Adding user tier dimension',
  });

  // Create default config
  await config.createDefaultConfig({
    key: 'discount_percentage',
    value: { value: 5 },
    schema: { type: 'number', minimum: 0, maximum: 50 },
    change_reason: 'Base discount configuration',
  });

  // Create context with override
  await config.createContext({
    context: {
      and: [
        { '==': [{ var: 'user_tier' }, 'platinum'] },
      ],
    },
    override: {
      discount_percentage: 25,
    },
    change_reason: 'Higher discount for platinum users',
  });

  console.log('Configuration setup complete!');
}

setupConfiguration().catch(console.error);
```

### Python

```python
import asyncio
from superposition_sdk import Configuration, CreateDimensionRequest
from superposition_sdk.types import CreateDefaultConfigRequest, CreateContextRequest

async def setup_superposition():
    # Initialize client
    config = Configuration(
        endpoint="http://localhost:8080",
        org_id="localorg",
        workspace_id="test",
        token="your-api-token",
    )

    # Create dimension
    await config.create_dimension(CreateDimensionRequest(
        dimension="platform",
        description="User platform (web, mobile, desktop)",
        position=1,
        schema={
            "type": "string",
            "enum": ["web", "ios", "android", "desktop"]
        },
        change_reason="Adding platform dimension",
    ))

    # Create default config
    await config.create_default_config(CreateDefaultConfigRequest(
        key="feature_x_enabled",
        value={"value": False},
        schema={"type": "boolean"},
        change_reason="Feature X flag - disabled by default",
    ))

    # Create experiment
    from superposition_sdk.types import CreateExperimentRequest, Variant, VariantType

    await config.create_experiment(CreateExperimentRequest(
        name="feature-x-rollout",
        description="Gradual rollout of Feature X",
        context={"==": [{"var": "platform"}, "web"]},
        variants=[
            Variant(
                id="control",
                variant_type=VariantType.CONTROL,
                overrides={"feature_x_enabled": False},
            ),
            Variant(
                id="treatment",
                variant_type=VariantType.EXPERIMENTAL,
                overrides={"feature_x_enabled": True},
            ),
        ],
        change_reason="Testing Feature X on web users",
    ))

    print("Setup complete!")

asyncio.run(setup_superposition())
```

### Java

```java
import io.juspay.superposition.sdk.*;
import io.juspay.superposition.sdk.types.*;
import java.util.List;
import java.util.Map;

public class SuperpositionExample {
    public static void main(String[] args) {
        // Initialize client
        Configuration config = Configuration.builder()
            .endpoint("http://localhost:8080")
            .orgId("localorg")
            .workspaceId("test")
            .token("your-api-token")
            .build();

        // Create dimension
        CreateDimensionRequest dimensionRequest = CreateDimensionRequest.builder()
            .dimension("region")
            .description("Geographic region")
            .position(1)
            .schema(Map.of(
                "type", "string",
                "enum", List.of("north", "south", "east", "west")
            ))
            .changeReason("Adding region dimension")
            .build();

        config.createDimension(dimensionRequest);

        // Create default config
        CreateDefaultConfigRequest configRequest = CreateDefaultConfigRequest.builder()
            .key("max_retries")
            .value(Map.of("value", 3))
            .schema(Map.of("type", "integer", "minimum", 0, "maximum", 10))
            .changeReason("Default retry count")
            .build();

        config.createDefaultConfig(configRequest);

        // Create experiment
        CreateExperimentRequest experimentRequest = CreateExperimentRequest.builder()
            .name("retry-optimization")
            .description("Test optimal retry count")
            .context(Map.of("==", List.of(Map.of("var", "region"), "north")))
            .variants(List.of(
                Variant.builder()
                    .id("control")
                    .variantType(VariantType.CONTROL)
                    .overrides(Map.of("max_retries", 3))
                    .build(),
                Variant.builder()
                    .id("higher-retries")
                    .variantType(VariantType.EXPERIMENTAL)
                    .overrides(Map.of("max_retries", 5))
                    .build()
            ))
            .changeReason("Testing higher retry count in north region")
            .build();

        config.createExperiment(experimentRequest);

        System.out.println("Setup complete!");
    }
}
```

## Common Operations

### List Dimensions

**Rust:**
```rust
let dimensions = config.list_dimensions().await?;
for dim in dimensions.data {
    println!("Dimension: {} at position {}", dim.dimension, dim.position);
}
```

**JavaScript:**
```typescript
const dimensions = await config.listDimensions();
dimensions.data.forEach(dim => {
  console.log(`${dim.dimension}: ${dim.description}`);
});
```

**Python:**
```python
dimensions = await config.list_dimensions()
for dim in dimensions.data:
    print(f"{dim.dimension}: {dim.description}")
```

**Java:**
```java
ListDimensionsResponse dimensions = config.listDimensions();
dimensions.getData().forEach(dim ->
    System.out.println(dim.getDimension() + ": " + dim.getDescription())
);
```

### Get Resolved Config

**Rust:**
```rust
use std::collections::HashMap;

let context = HashMap::from([
    ("city".to_string(), "Delhi".into()),
    ("vehicle_type".to_string(), "cab".into()),
]);

let resolved = config.get_config(context).await?;
println!("Resolved config: {:?}", resolved);
```

**JavaScript:**
```typescript
const resolved = await config.getConfig({
  city: 'Delhi',
  vehicle_type: 'cab',
});
console.log('Rate:', resolved.per_km_rate);
```

**Python:**
```python
resolved = await config.get_config({
    "city": "Delhi",
    "vehicle_type": "cab",
})
print(f"Rate: {resolved['per_km_rate']}")
```

**Java:**
```java
Map<String, Object> context = Map.of(
    "city", "Delhi",
    "vehicle_type", "cab"
);
Map<String, Object> resolved = config.getConfig(context);
System.out.println("Rate: " + resolved.get("per_km_rate"));
```

### Experiment Operations

**Start experiment:**
```typescript
// JavaScript
await config.rampExperiment('my-experiment', {
  trafficPercentage: 10,
  changeReason: 'Starting experiment with 10% traffic',
});
```

**Conclude experiment:**
```python
# Python
await config.conclude_experiment("my-experiment", ConcludeExperimentRequest(
    chosen_variant="treatment",
    change_reason="Treatment showed 15% improvement",
))
```

**List active experiments:**
```rust
// Rust
let experiments = config.list_experiments()
    .status(ExperimentStatus::InProgress)
    .await?;
```

## Error Handling

### Rust
```rust
use superposition_sdk::Error;

match config.create_dimension(request).await {
    Ok(response) => println!("Created: {:?}", response),
    Err(Error::ApiError(e)) => {
        eprintln!("API error: {} - {}", e.status, e.message);
    }
    Err(e) => eprintln!("Error: {}", e),
}
```

### JavaScript
```typescript
try {
  await config.createDimension(request);
} catch (error) {
  if (error.response) {
    console.error(`API Error ${error.response.status}: ${error.response.data.message}`);
  } else {
    console.error('Network error:', error.message);
  }
}
```

### Python
```python
from superposition_sdk.exceptions import ApiError

try:
    await config.create_dimension(request)
except ApiError as e:
    print(f"API Error {e.status}: {e.message}")
```

### Java
```java
try {
    config.createDimension(request);
} catch (ApiException e) {
    System.err.println("API Error " + e.getCode() + ": " + e.getMessage());
}
```

## Related Skills

- [superposition-config](../superposition-config/) - Conceptual understanding of CAC
- [superposition-provider](../superposition-provider/) - For reading configs at runtime
- [superposition-api](../superposition-api/) - REST API reference

See [references/REFERENCE.md](references/REFERENCE.md) for complete SDK method reference.
