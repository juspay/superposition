# SDK Reference

Complete method reference for Superposition SDKs across all supported languages.

## Table of Contents

- [Client Configuration](#client-configuration)
- [Dimensions API](#dimensions-api)
- [Default Config API](#default-config-api)
- [Context API](#context-api)
- [Function API](#function-api)
- [Experiments API](#experiments-api)
- [Webhooks API](#webhooks-api)
- [Configuration Resolution](#configuration-resolution)

---

## Client Configuration

### Rust

```rust
use superposition_sdk::Configuration;

let config = Configuration::builder()
    .endpoint("http://localhost:8080")     // Required
    .org_id("myorg")                       // Required
    .workspace_id("default")               // Required
    .token("your-api-token")               // Required for write operations
    .timeout(std::time::Duration::from_secs(30))  // Optional
    .build()?;
```

### JavaScript

```typescript
import { Configuration } from 'superposition-sdk';

const config = new Configuration({
  endpoint: 'http://localhost:8080',  // Required
  orgId: 'myorg',                     // Required
  workspaceId: 'default',             // Required
  token: 'your-api-token',            // Required for write operations
  timeout: 30000,                     // Optional, default 30000ms
});
```

### Python

```python
from superposition_sdk import Configuration

config = Configuration(
    endpoint="http://localhost:8080",  # Required
    org_id="myorg",                    # Required
    workspace_id="default",            # Required
    token="your-api-token",            # Required for write operations
    timeout=30.0,                      # Optional, default 30.0 seconds
)
```

### Java

```java
import io.juspay.superposition.sdk.Configuration;

Configuration config = Configuration.builder()
    .endpoint("http://localhost:8080")   // Required
    .orgId("myorg")                      // Required
    .workspaceId("default")              // Required
    .token("your-api-token")             // Required for write operations
    .timeout(Duration.ofSeconds(30))     // Optional
    .build();
```

---

## Dimensions API

### Create Dimension

**Rust:**
```rust
use superposition_sdk::CreateDimensionRequest;

let request = CreateDimensionRequest::builder()
    .dimension("city")
    .description("City location")
    .position(1)
    .schema(serde_json::json!({
        "type": "string",
        "enum": ["Bangalore", "Delhi", "Mumbai"]
    }))
    .dependencies(vec![])  // Optional
    .dimension_type(DimensionType::Regular)  // Optional, default Regular
    .value_validation_function_name("validate_city")  // Optional
    .change_reason("Adding city dimension")
    .build();

let response = config.create_dimension(request).await?;
```

**JavaScript:**
```typescript
const response = await config.createDimension({
  dimension: 'city',
  description: 'City location',
  position: 1,
  schema: {
    type: 'string',
    enum: ['Bangalore', 'Delhi', 'Mumbai'],
  },
  dependencies: [],  // Optional
  dimensionType: 'REGULAR',  // Optional
  valueValidationFunctionName: 'validate_city',  // Optional
  changeReason: 'Adding city dimension',
});
```

**Python:**
```python
from superposition_sdk.types import CreateDimensionRequest, DimensionType

response = await config.create_dimension(CreateDimensionRequest(
    dimension="city",
    description="City location",
    position=1,
    schema={
        "type": "string",
        "enum": ["Bangalore", "Delhi", "Mumbai"],
    },
    dependencies=[],  # Optional
    dimension_type=DimensionType.REGULAR,  # Optional
    value_validation_function_name="validate_city",  # Optional
    change_reason="Adding city dimension",
))
```

**Java:**
```java
import io.juspay.superposition.sdk.types.*;

CreateDimensionRequest request = CreateDimensionRequest.builder()
    .dimension("city")
    .description("City location")
    .position(1)
    .schema(Map.of(
        "type", "string",
        "enum", List.of("Bangalore", "Delhi", "Mumbai")
    ))
    .dependencies(List.of())
    .dimensionType(DimensionType.REGULAR)
    .changeReason("Adding city dimension")
    .build();

CreateDimensionResponse response = config.createDimension(request);
```

### List Dimensions

**Rust:**
```rust
let response = config.list_dimensions().await?;
for dim in response.data {
    println!("{}: position {}", dim.dimension, dim.position);
}
```

**JavaScript:**
```typescript
const response = await config.listDimensions();
response.data.forEach(dim => {
  console.log(`${dim.dimension}: ${dim.position}`);
});
```

**Python:**
```python
response = await config.list_dimensions()
for dim in response.data:
    print(f"{dim.dimension}: {dim.position}")
```

**Java:**
```java
ListDimensionsResponse response = config.listDimensions();
response.getData().forEach(dim ->
    System.out.println(dim.getDimension() + ": " + dim.getPosition())
);
```

### Get Dimension

**Rust:**
```rust
let dimension = config.get_dimension("city").await?;
println!("Description: {}", dimension.description);
println!("Schema: {:?}", dimension.schema);
```

**JavaScript:**
```typescript
const dimension = await config.getDimension('city');
console.log('Description:', dimension.description);
console.log('Schema:', dimension.schema);
```

**Python:**
```python
dimension = await config.get_dimension("city")
print(f"Description: {dimension.description}")
print(f"Schema: {dimension.schema}")
```

**Java:**
```java
GetDimensionResponse dimension = config.getDimension("city");
System.out.println("Description: " + dimension.getDescription());
System.out.println("Schema: " + dimension.getSchema());
```

### Update Dimension

**Rust:**
```rust
let request = UpdateDimensionRequest::builder()
    .schema(serde_json::json!({
        "type": "string",
        "enum": ["Bangalore", "Delhi", "Mumbai", "Chennai"]
    }))
    .change_reason("Added Chennai")
    .build();

let response = config.update_dimension("city", request).await?;
```

**JavaScript:**
```typescript
const response = await config.updateDimension('city', {
  schema: {
    type: 'string',
    enum: ['Bangalore', 'Delhi', 'Mumbai', 'Chennai'],
  },
  changeReason: 'Added Chennai',
});
```

**Python:**
```python
from superposition_sdk.types import UpdateDimensionRequest

response = await config.update_dimension("city", UpdateDimensionRequest(
    schema={
        "type": "string",
        "enum": ["Bangalore", "Delhi", "Mumbai", "Chennai"],
    },
    change_reason="Added Chennai",
))
```

**Java:**
```java
UpdateDimensionRequest request = UpdateDimensionRequest.builder()
    .schema(Map.of(
        "type", "string",
        "enum", List.of("Bangalore", "Delhi", "Mumbai", "Chennai")
    ))
    .changeReason("Added Chennai")
    .build();

UpdateDimensionResponse response = config.updateDimension("city", request);
```

### Delete Dimension

**Rust:**
```rust
config.delete_dimension("old_dimension").await?;
```

**JavaScript:**
```typescript
await config.deleteDimension('old_dimension');
```

**Python:**
```python
await config.delete_dimension("old_dimension")
```

**Java:**
```java
config.deleteDimension("old_dimension");
```

---

## Default Config API

### Create Default Config

**Rust:**
```rust
let request = CreateDefaultConfigRequest::builder()
    .key("checkout_timeout")
    .value(serde_json::json!({"value": 300}))
    .schema(serde_json::json!({"type": "integer", "minimum": 60}))
    .function_name("validate_timeout")  // Optional
    .change_reason("Setting default checkout timeout")
    .build();

let response = config.create_default_config(request).await?;
```

**JavaScript:**
```typescript
const response = await config.createDefaultConfig({
  key: 'checkout_timeout',
  value: { value: 300 },
  schema: { type: 'integer', minimum: 60 },
  functionName: 'validate_timeout',  // Optional
  changeReason: 'Setting default checkout timeout',
});
```

**Python:**
```python
from superposition_sdk.types import CreateDefaultConfigRequest

response = await config.create_default_config(CreateDefaultConfigRequest(
    key="checkout_timeout",
    value={"value": 300},
    schema={"type": "integer", "minimum": 60},
    function_name="validate_timeout",  # Optional
    change_reason="Setting default checkout timeout",
))
```

**Java:**
```java
CreateDefaultConfigRequest request = CreateDefaultConfigRequest.builder()
    .key("checkout_timeout")
    .value(Map.of("value", 300))
    .schema(Map.of("type", "integer", "minimum", 60))
    .functionName("validate_timeout")
    .changeReason("Setting default checkout timeout")
    .build();

CreateDefaultConfigResponse response = config.createDefaultConfig(request);
```

### List Default Configs

**Rust:**
```rust
let response = config.list_default_configs().await?;
for cfg in response.data {
    println!("{}: {:?}", cfg.key, cfg.value);
}
```

**JavaScript:**
```typescript
const response = await config.listDefaultConfigs();
response.data.forEach(cfg => {
  console.log(`${cfg.key}: ${JSON.stringify(cfg.value)}`);
});
```

**Python:**
```python
response = await config.list_default_configs()
for cfg in response.data:
    print(f"{cfg.key}: {cfg.value}")
```

**Java:**
```java
ListDefaultConfigsResponse response = config.listDefaultConfigs();
response.getData().forEach(cfg ->
    System.out.println(cfg.getKey() + ": " + cfg.getValue())
);
```

### Get Default Config

**Rust:**
```rust
let cfg = config.get_default_config("checkout_timeout").await?;
println!("Value: {:?}", cfg.value);
println!("Schema: {:?}", cfg.schema);
```

**JavaScript:**
```typescript
const cfg = await config.getDefaultConfig('checkout_timeout');
console.log('Value:', cfg.value);
console.log('Schema:', cfg.schema);
```

**Python:**
```python
cfg = await config.get_default_config("checkout_timeout")
print(f"Value: {cfg.value}")
print(f"Schema: {cfg.schema}")
```

**Java:**
```java
GetDefaultConfigResponse cfg = config.getDefaultConfig("checkout_timeout");
System.out.println("Value: " + cfg.getValue());
System.out.println("Schema: " + cfg.getSchema());
```

### Update Default Config

**Rust:**
```rust
let request = UpdateDefaultConfigRequest::builder()
    .value(serde_json::json!({"value": 600}))
    .change_reason("Increased timeout")
    .build();

let response = config.update_default_config("checkout_timeout", request).await?;
```

**JavaScript:**
```typescript
const response = await config.updateDefaultConfig('checkout_timeout', {
  value: { value: 600 },
  changeReason: 'Increased timeout',
});
```

**Python:**
```python
from superposition_sdk.types import UpdateDefaultConfigRequest

response = await config.update_default_config("checkout_timeout", UpdateDefaultConfigRequest(
    value={"value": 600},
    change_reason="Increased timeout",
))
```

**Java:**
```java
UpdateDefaultConfigRequest request = UpdateDefaultConfigRequest.builder()
    .value(Map.of("value", 600))
    .changeReason("Increased timeout")
    .build();

config.updateDefaultConfig("checkout_timeout", request);
```

### Delete Default Config

**Rust:**
```rust
config.delete_default_config("old_config").await?;
```

**JavaScript:**
```typescript
await config.deleteDefaultConfig('old_config');
```

**Python:**
```python
await config.delete_default_config("old_config")
```

**Java:**
```java
config.deleteDefaultConfig("old_config");
```

---

## Context API

### Create Context

**Rust:**
```rust
let request = CreateContextRequest::builder()
    .context(serde_json::json!({
        "and": [
            {"==": [{"var": "city"}, "Delhi"]},
            {"==": [{"var": "vehicle_type"}, "cab"]}
        ]
    }))
    .override(serde_json::json!({
        "per_km_rate": 25.0,
        "base_fare": 50.0
    }))
    .change_reason("Delhi cab pricing")
    .build();

let response = config.create_context(request).await?;
```

**JavaScript:**
```typescript
const response = await config.createContext({
  context: {
    and: [
      { '==': [{ var: 'city' }, 'Delhi'] },
      { '==': [{ var: 'vehicle_type' }, 'cab'] },
    ],
  },
  override: {
    per_km_rate: 25.0,
    base_fare: 50.0,
  },
  changeReason: 'Delhi cab pricing',
});
```

**Python:**
```python
from superposition_sdk.types import CreateContextRequest

response = await config.create_context(CreateContextRequest(
    context={
        "and": [
            {"==": [{"var": "city"}, "Delhi"]},
            {"==": [{"var": "vehicle_type"}, "cab"]},
        ]
    },
    override={
        "per_km_rate": 25.0,
        "base_fare": 50.0,
    },
    change_reason="Delhi cab pricing",
))
```

**Java:**
```java
CreateContextRequest request = CreateContextRequest.builder()
    .context(Map.of(
        "and", List.of(
            Map.of("==", List.of(Map.of("var", "city"), "Delhi")),
            Map.of("==", List.of(Map.of("var", "vehicle_type"), "cab"))
        )
    ))
    .override(Map.of(
        "per_km_rate", 25.0,
        "base_fare", 50.0
    ))
    .changeReason("Delhi cab pricing")
    .build();

CreateContextResponse response = config.createContext(request);
```

### List Contexts

**Rust:**
```rust
let response = config.list_contexts().await?;
for ctx in response.data {
    println!("Context ID: {}", ctx.id);
    println!("Condition: {:?}", ctx.context);
}
```

**JavaScript:**
```typescript
const response = await config.listContexts();
response.data.forEach(ctx => {
  console.log(`Context ${ctx.id}:`, ctx.context);
});
```

**Python:**
```python
response = await config.list_contexts()
for ctx in response.data:
    print(f"Context {ctx.id}: {ctx.context}")
```

**Java:**
```java
ListContextsResponse response = config.listContexts();
response.getData().forEach(ctx ->
    System.out.println("Context " + ctx.getId() + ": " + ctx.getContext())
);
```

### Get Context

**Rust:**
```rust
let ctx = config.get_context("context-id").await?;
println!("Override: {:?}", ctx.override);
```

**JavaScript:**
```typescript
const ctx = await config.getContext('context-id');
console.log('Override:', ctx.override);
```

**Python:**
```python
ctx = await config.get_context("context-id")
print(f"Override: {ctx.override}")
```

**Java:**
```java
GetContextResponse ctx = config.getContext("context-id");
System.out.println("Override: " + ctx.getOverride());
```

### Delete Context

**Rust:**
```rust
config.delete_context("context-id").await?;
```

**JavaScript:**
```typescript
await config.deleteContext('context-id');
```

**Python:**
```python
await config.delete_context("context-id")
```

**Java:**
```java
config.deleteContext("context-id");
```

---

## Function API

### Create Function

**Rust:**
```rust
let request = CreateFunctionRequest::builder()
    .function_name("validate_discount")
    .code(r#"
async function validate(key, value) {
    const maxDiscount = { "platinum": 50, "gold": 30, "silver": 20, "bronze": 10 };
    const tier = context.user_tier || "bronze";
    if (key === "discount_percentage") {
        return value <= maxDiscount[tier];
    }
    return true;
}
"#.to_string())
    .change_reason("Discount validation by tier")
    .build();

let response = config.create_function(request).await?;
```

**JavaScript:**
```typescript
const response = await config.createFunction({
  functionName: 'validate_discount',
  code: `
async function validate(key, value) {
    const maxDiscount = { platinum: 50, gold: 30, silver: 20, bronze: 10 };
    const tier = context.user_tier || 'bronze';
    if (key === 'discount_percentage') {
        return value <= maxDiscount[tier];
    }
    return true;
}
  `,
  changeReason: 'Discount validation by tier',
});
```

**Python:**
```python
from superposition_sdk.types import CreateFunctionRequest

response = await config.create_function(CreateFunctionRequest(
    function_name="validate_discount",
    code="""
async function validate(key, value) {
    const maxDiscount = { "platinum": 50, "gold": 30, "silver": 20, "bronze": 10 };
    const tier = context.user_tier || "bronze";
    if (key === "discount_percentage") {
        return value <= maxDiscount[tier];
    }
    return true;
}
""",
    change_reason="Discount validation by tier",
))
```

**Java:**
```java
CreateFunctionRequest request = CreateFunctionRequest.builder()
    .functionName("validate_discount")
    .code("async function validate(key, value) {\n" +
          "    const maxDiscount = { \"platinum\": 50, \"gold\": 30 };\n" +
          "    const tier = context.user_tier || \"bronze\";\n" +
          "    return value <= maxDiscount[tier];\n" +
          "}")
    .changeReason("Discount validation by tier")
    .build();

CreateFunctionResponse response = config.createFunction(request);
```

### Test Function

**Rust:**
```rust
let request = TestFunctionRequest::builder()
    .test_case(serde_json::json!({
        "key": "discount_percentage",
        "value": 25
    }))
    .context(serde_json::json!({
        "user_tier": "gold"
    }))
    .build();

let result = config.test_function("validate_discount", request).await?;
println!("Valid: {}", result.valid);
```

**JavaScript:**
```typescript
const result = await config.testFunction('validate_discount', {
  testCase: {
    key: 'discount_percentage',
    value: 25,
  },
  context: {
    user_tier: 'gold',
  },
});
console.log('Valid:', result.valid);
```

**Python:**
```python
from superposition_sdk.types import TestFunctionRequest

result = await config.test_function("validate_discount", TestFunctionRequest(
    test_case={
        "key": "discount_percentage",
        "value": 25,
    },
    context={
        "user_tier": "gold",
    },
))
print(f"Valid: {result.valid}")
```

**Java:**
```java
TestFunctionRequest request = TestFunctionRequest.builder()
    .testCase(Map.of("key", "discount_percentage", "value", 25))
    .context(Map.of("user_tier", "gold"))
    .build();

TestFunctionResponse result = config.testFunction("validate_discount", request);
System.out.println("Valid: " + result.isValid());
```

### List Functions

**Rust:**
```rust
let response = config.list_functions().await?;
for func in response.data {
    println!("{}: {}", func.function_name, func.description.unwrap_or_default());
}
```

**JavaScript:**
```typescript
const response = await config.listFunctions();
response.data.forEach(func => {
  console.log(`${func.functionName}`);
});
```

**Python:**
```python
response = await config.list_functions()
for func in response.data:
    print(f"{func.function_name}")
```

**Java:**
```java
ListFunctionsResponse response = config.listFunctions();
response.getData().forEach(func ->
    System.out.println(func.getFunctionName())
);
```

### Delete Function

**Rust:**
```rust
config.delete_function("old_function").await?;
```

**JavaScript:**
```typescript
await config.deleteFunction('old_function');
```

**Python:**
```python
await config.delete_function("old_function")
```

**Java:**
```java
config.deleteFunction("old_function");
```

---

## Experiments API

### Create Experiment

**Rust:**
```rust
use superposition_sdk::{CreateExperimentRequest, Variant, VariantType};

let request = CreateExperimentRequest::builder()
    .name("checkout-optimization")
    .description("Test simplified checkout")
    .context(serde_json::json!({"==": [{"var": "platform"}, "web"]}))
    .variants(vec![
        Variant {
            id: "control".to_string(),
            variant_type: VariantType::Control,
            overrides: serde_json::json!({"checkout_steps": 5}),
            ..Default::default()
        },
        Variant {
            id: "simplified".to_string(),
            variant_type: VariantType::Experimental,
            overrides: serde_json::json!({"checkout_steps": 3}),
            ..Default::default()
        },
    ])
    .change_reason("A/B test for checkout")
    .build();

let response = config.create_experiment(request).await?;
```

**JavaScript:**
```typescript
const response = await config.createExperiment({
  name: 'checkout-optimization',
  description: 'Test simplified checkout',
  context: { '==': [{ var: 'platform' }, 'web'] },
  variants: [
    {
      id: 'control',
      variantType: 'CONTROL',
      overrides: { checkout_steps: 5 },
    },
    {
      id: 'simplified',
      variantType: 'EXPERIMENTAL',
      overrides: { checkout_steps: 3 },
    },
  ],
  changeReason: 'A/B test for checkout',
});
```

**Python:**
```python
from superposition_sdk.types import CreateExperimentRequest, Variant, VariantType

response = await config.create_experiment(CreateExperimentRequest(
    name="checkout-optimization",
    description="Test simplified checkout",
    context={"==": [{"var": "platform"}, "web"]},
    variants=[
        Variant(
            id="control",
            variant_type=VariantType.CONTROL,
            overrides={"checkout_steps": 5},
        ),
        Variant(
            id="simplified",
            variant_type=VariantType.EXPERIMENTAL,
            overrides={"checkout_steps": 3},
        ),
    ],
    change_reason="A/B test for checkout",
))
```

**Java:**
```java
CreateExperimentRequest request = CreateExperimentRequest.builder()
    .name("checkout-optimization")
    .description("Test simplified checkout")
    .context(Map.of("==", List.of(Map.of("var", "platform"), "web")))
    .variants(List.of(
        Variant.builder()
            .id("control")
            .variantType(VariantType.CONTROL)
            .overrides(Map.of("checkout_steps", 5))
            .build(),
        Variant.builder()
            .id("simplified")
            .variantType(VariantType.EXPERIMENTAL)
            .overrides(Map.of("checkout_steps", 3))
            .build()
    ))
    .changeReason("A/B test for checkout")
    .build();

CreateExperimentResponse response = config.createExperiment(request);
```

### Ramp Experiment

**Rust:**
```rust
let request = RampExperimentRequest::builder()
    .traffic_percentage(25)
    .change_reason("Increasing to 25%")
    .build();

let response = config.ramp_experiment("checkout-optimization", request).await?;
```

**JavaScript:**
```typescript
const response = await config.rampExperiment('checkout-optimization', {
  trafficPercentage: 25,
  changeReason: 'Increasing to 25%',
});
```

**Python:**
```python
from superposition_sdk.types import RampExperimentRequest

response = await config.ramp_experiment("checkout-optimization", RampExperimentRequest(
    traffic_percentage=25,
    change_reason="Increasing to 25%",
))
```

**Java:**
```java
RampExperimentRequest request = RampExperimentRequest.builder()
    .trafficPercentage(25)
    .changeReason("Increasing to 25%")
    .build();

RampExperimentResponse response = config.rampExperiment("checkout-optimization", request);
```

### Conclude Experiment

**Rust:**
```rust
let request = ConcludeExperimentRequest::builder()
    .chosen_variant("simplified")
    .change_reason("Simplified showed 15% improvement")
    .build();

let response = config.conclude_experiment("checkout-optimization", request).await?;
```

**JavaScript:**
```typescript
const response = await config.concludeExperiment('checkout-optimization', {
  chosenVariant: 'simplified',
  changeReason: 'Simplified showed 15% improvement',
});
```

**Python:**
```python
from superposition_sdk.types import ConcludeExperimentRequest

response = await config.conclude_experiment("checkout-optimization", ConcludeExperimentRequest(
    chosen_variant="simplified",
    change_reason="Simplified showed 15% improvement",
))
```

**Java:**
```java
ConcludeExperimentRequest request = ConcludeExperimentRequest.builder()
    .chosenVariant("simplified")
    .changeReason("Simplified showed 15% improvement")
    .build();

config.concludeExperiment("checkout-optimization", request);
```

### List Experiments

**Rust:**
```rust
let response = config.list_experiments()
    .status(ExperimentStatus::InProgress)
    .await?;
```

**JavaScript:**
```typescript
const response = await config.listExperiments({
  status: 'INPROGRESS',
});
```

**Python:**
```python
response = await config.list_experiments(status="INPROGRESS")
```

**Java:**
```java
ListExperimentsResponse response = config.listExperiments(
    ListExperimentsRequest.builder().status("INPROGRESS").build()
);
```

---

## Configuration Resolution

### Get Config

**Rust:**
```rust
use std::collections::HashMap;

let context = HashMap::from([
    ("city".to_string(), serde_json::Value::String("Delhi".to_string())),
    ("vehicle_type".to_string(), serde_json::Value::String("cab".to_string())),
]);

let config_response = config.get_config(context).await?;
println!("per_km_rate: {:?}", config_response.get("per_km_rate"));
```

**JavaScript:**
```typescript
const resolved = await config.getConfig({
  city: 'Delhi',
  vehicle_type: 'cab',
});
console.log('per_km_rate:', resolved.per_km_rate);
```

**Python:**
```python
resolved = await config.get_config({
    "city": "Delhi",
    "vehicle_type": "cab",
})
print(f"per_km_rate: {resolved['per_km_rate']}")
```

**Java:**
```java
Map<String, Object> context = Map.of(
    "city", "Delhi",
    "vehicle_type", "cab"
);
Map<String, Object> resolved = config.getConfig(context);
System.out.println("per_km_rate: " + resolved.get("per_km_rate"));
```

---

## Webhooks API

### Create Webhook

**JavaScript:**
```typescript
const webhook = await config.createWebhook({
  name: 'experiment-webhook',
  description: 'Notify on experiment events',
  url: 'https://your-service.com/webhook',
  method: 'POST',
  enabled: true,
  events: ['ExperimentStarted', 'ExperimentConcluded'],
  customHeaders: {
    'Authorization': 'Bearer token',
  },
  changeReason: 'Setup experiment notifications',
});
```

### List Webhooks

**Python:**
```python
webhooks = await config.list_webhooks()
for wh in webhooks.data:
    print(f"{wh.name}: {wh.url}")
```

### Delete Webhook

**Rust:**
```rust
config.delete_webhook("experiment-webhook").await?;
```
