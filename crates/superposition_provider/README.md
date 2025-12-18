# Superposition Provider

Superposition provider is an openfeature provider that works with [Superposition](https://juspay.io/open-source/superposition) to manage configurations and experiments in your application. It allows you to fetch feature flags, configurations, and experiment variants from a Superposition server. Read the [Superposition Provider Docs](https://juspay.io/open-source/superposition/docs) or [docs.rs](https://docs.rs/superposition_provider/latest/superposition_provider/) for more details.

## Overview

The Superposition Provider integrates with OpenFeature to provide:

-   **Context Aware Configuration (CAC)**: Dynamic configuration management based on context
-   **Experimentation**: A/B testing and feature experimentation capabilities
-   **Real-time Updates**: Support for polling and on-demand refresh strategies
-   **Caching**: Built-in caching for improved performance

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
superposition_provider = "<version>"
open-feature = "0.2"
tokio = { version = "1.0", features = ["full"] }
env_logger = "0.10"
```

### OR

```
cargo add superposition_provider
```

## Quick Start

### Basic Usage

```rust
use superposition_provider::{
    ConfigurationOptions, SuperpositionOptions, SuperpositionProvider,
    SuperpositionProviderOptions, RefreshStrategy, PollingStrategy
};
use open_feature::OpenFeature;
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    env_logger::init();

    // Get the OpenFeature API singleton
    let mut api = OpenFeature::singleton_mut().await;

    // Configure the Superposition provider
    let options = SuperpositionProviderOptions {

        endpoint: "http://localhost:8080/".to_string(),
        token: "your_token_here".to_string(),
        org_id: "your_org_id".to_string(),
        workspace_id: "your_workspace_id".to_string(),
        fallback_config: None,
        evaluation_cache: None,
        refresh_strategy: RefreshStrategy::Polling(PollingStrategy {
            interval: 60, // Poll every 60 seconds
            timeout: Some(30)
        }),
        experimentation_options: None // CAC only
    };

    // Set the provider
    api.set_provider(SuperpositionProvider::new(options)).await;

    // Create a client
    let client = api.create_client();

    // Wait for initialization
    sleep(Duration::from_secs(3)).await;

    // Evaluate feature flags
    let int_value = client.get_int_value("my_feature_flag", None, None).await.unwrap();
    println!("Feature flag value: {}", int_value);
}
```

## New: Local Resolution Provider

In addition to the original `SuperpositionProvider` (which remains fully supported), we now offer `LocalResolutionProvider` - a more flexible provider that supports:

- **Pluggable Data Sources**: HTTP, File-based (CAC TOML), or custom implementations
- **Bulk Configuration Resolution**: Resolve all features at once with the `AllFeatureProvider` trait
- **Experiment Metadata**: Access detailed experiment information via `FeatureExperimentMeta` trait
- **File-based Configuration**: Load configuration from local `.cac.toml` files with optional file watching

### Using LocalResolutionProvider with HTTP

```rust
use std::sync::Arc;
use superposition_provider::{
    HttpDataSource, LocalResolutionProvider, LocalResolutionProviderOptions,
    PollingStrategy, RefreshStrategy, SuperpositionOptions,
};
use open_feature::OpenFeature;

#[tokio::main]
async fn main() {
    // Create HTTP data source
    let superposition_options = SuperpositionOptions::new(
        "http://localhost:8080".to_string(),
        "your_token".to_string(),
        "your_org".to_string(),
        "your_workspace".to_string(),
    );
    let data_source = Arc::new(HttpDataSource::new(superposition_options));

    // Create provider with polling
    let provider_options = LocalResolutionProviderOptions {
        refresh_strategy: RefreshStrategy::Polling(PollingStrategy {
            interval_seconds: 30,
        }),
        fallback_config: None,
        enable_experiments: true,
    };

    let provider = Arc::new(LocalResolutionProvider::new(
        data_source,
        provider_options,
    ));

    // Initialize and register with OpenFeature
    provider.init().await.unwrap();
    let mut api = OpenFeature::singleton_mut().await;
    api.set_provider(provider.clone()).await;

    // Use like normal OpenFeature client
    let client = api.create_client();
    let value = client.get_bool_value("feature_flag", None, None).await.unwrap();
}
```


### Migration from SuperpositionProvider

The original `SuperpositionProvider` continues to work without any changes. To migrate to the new `LocalResolutionProvider`:

**Before (SuperpositionProvider):**
```rust
let provider = SuperpositionProvider::new(SuperpositionProviderOptions {
    endpoint: "http://localhost:8080".to_string(),
    token: "token".to_string(),
    org_id: "org".to_string(),
    workspace_id: "workspace".to_string(),
    refresh_strategy: RefreshStrategy::Polling(PollingStrategy { interval_seconds: 60 }),
    fallback_config: None,
    evaluation_cache: None,
    experimentation_options: None,
});
```

**After (LocalResolutionProvider):**
```rust
let superposition_options = SuperpositionOptions::new(
    "http://localhost:8080".to_string(),
    "token".to_string(),
    "org".to_string(),
    "workspace".to_string(),
);
let data_source = Arc::new(HttpDataSource::new(superposition_options));
let provider = Arc::new(LocalResolutionProvider::new(
    data_source,
    LocalResolutionProviderOptions {
        refresh_strategy: RefreshStrategy::Polling(PollingStrategy { interval_seconds: 60 }),
        fallback_config: None,
        enable_experiments: true,
    },
));
provider.init().await.unwrap();
```

## Configuration Options

### SuperpositionOptions

Core connection settings for the Superposition service:

```rust
let superposition_options = SuperpositionOptions {
    endpoint: "https://superposition.example.com".to_string(),
    token: "your_token_here".to_string(),
    org_id: "your_organization_id".to_string(),
    workspace_id: "your_workspace_id".to_string(),
};
```

### Refresh Strategies

#### Polling Strategy

Automatically fetches updates at regular intervals:

```rust
let polling_strategy = RefreshStrategy::Polling(PollingStrategy {
    interval: 60, // Check for updates every 60 seconds
    timeout: Some(30), // 30 second timeout for requests
});
```

#### On-Demand Strategy

Fetches updates only when requested, with TTL-based caching:

```rust
let on_demand_strategy = RefreshStrategy::OnDemand(OnDemandStrategy {
    ttl: 300, // Cache for 5 minutes
    timeout: Some(30), // 30 second timeout
    use_stale_on_error: Some(true), // Use cached data if fetch fails
});
```

### Configuration Options

```rust
let cac_options = ConfigurationOptions {
    fallback_config: Some(fallback_map), // Optional fallback configuration
    evaluation_cache: Some(EvaluationCacheOptions {
        ttl: Some(60), // Cache evaluations for 1 minute
        size: Some(500), // Maximum 500 cached evaluations
    }),
    refresh_strategy: RefreshStrategy::Polling(PollingStrategy::default()),
};
```

## Advanced Usage

### With Experimentation

Enable A/B testing and experimentation features:

```rust
use superposition_provider::ExperimentationOptions;

let options = SuperpositionProviderOptions {

    endpoint: "http://localhost:8080/".to_string(),
    token: "your_token_here".to_string(),
    org_id: "your_org_id".to_string(),
    workspace_id: "your_workspace_id".to_string(),
    refresh_strategy: RefreshStrategy::OnDemand(OnDemandStrategy::default()),
    evaluation_cache: None,
    fallback_config: None,
    experimentation_options: Some(ExperimentationOptions {
        refresh_strategy: RefreshStrategy::OnDemand(OnDemandStrategy::default()),
        evaluation_cache: Some(EvaluationCacheOptions::default()),
        default_toss: Some(50)
    })
};
```

### Using Evaluation Context

Pass context for dynamic configuration evaluation:

```rust
use open_feature::EvaluationContext;
use std::collections::HashMap;

// Create evaluation context
let mut context = EvaluationContext::default();
context.targeting_key = Some("user_123".to_string());

// Add custom fields
let mut custom_fields = HashMap::new();
custom_fields.insert(
    "user_tier".to_string(),
    open_feature::EvaluationContextFieldValue::String("premium".to_string())
);
custom_fields.insert(
    "feature_enabled".to_string(),
    open_feature::EvaluationContextFieldValue::Bool(true)
);
context.custom_fields = custom_fields;

// Evaluate with context
let value = client.get_string_value("feature_config", Some(context), None).await?;
```

### Fallback Configuration

Provide fallback values for when the service is unavailable:

```rust
use serde_json::{Map, Value, json};

let mut fallback_config = Map::new();
fallback_config.insert("default_timeout".to_string(), json!(30));
fallback_config.insert("max_retries".to_string(), json!(3));
fallback_config.insert("feature_enabled".to_string(), json!(false));

let cac_options = ConfigurationOptions {
    fallback_config: Some(fallback_config),
    evaluation_cache: Some(EvaluationCacheOptions::default()),
    refresh_strategy: RefreshStrategy::OnDemand(OnDemandStrategy::default()),
};
```

## Supported Value Types

The provider supports all OpenFeature value types:

```rust
// Boolean flags
let enabled: bool = client.get_bool_value("feature_enabled", None, None).await?;

// String configuration
let api_url: String = client.get_string_value("api_endpoint", None, None).await?;

// Integer values
let timeout: i64 = client.get_int_value("request_timeout", None, None).await?;

// Float values
let ratio: f64 = client.get_float_value("success_ratio", None, None).await?;

// Complex objects (StructValue)
let config: open_feature::StructValue = client.get_struct_value("complex_config", None, None).await?;
```

## Error Handling

The provider handles various error scenarios gracefully:

```rust
match client.get_string_value("my_flag", None, None).await {
    Ok(value) => println!("Flag value: {}", value),
    Err(error) => {
        match error.code {
            open_feature::EvaluationErrorCode::FlagNotFound => {
                println!("Flag not found, using default");
            },
            open_feature::EvaluationErrorCode::InvalidContext => {
                println!("Invalid context provided");
            },
            _ => println!("Other error: {:?}", error),
        }
    }
}
```

## Performance Considerations

-   **Caching**: Enable evaluation caching for frequently accessed flags
-   **Refresh Strategy**: Choose polling for real-time updates or on-demand for better performance
-   **Fallback Config**: Always provide fallback configuration for critical features
-   **Context Size**: Keep evaluation context minimal for better performance

## Logging

The provider uses the `log` crate for debugging. Enable logging to see detailed information:

```rust
env_logger::init();
```

Set the log level with the `RUST_LOG` environment variable:

```bash
RUST_LOG=debug cargo run
```

## Examples

The `examples/` directory contains several examples demonstrating different usage patterns:

- **`local_http.rs`**: LocalResolutionProvider with HTTP data source and polling
- **`all_features.rs`**: Using AllFeatureProvider trait for bulk configuration resolution

Run an example:
```bash
For the HTTP example, set environment variables:
```bash
export SUPERPOSITION_ENDPOINT="http://localhost:8080"
export SUPERPOSITION_TOKEN="your_token"
export SUPERPOSITION_ORG_ID="your_org"
export SUPERPOSITION_WORKSPACE_ID="your_workspace"
cargo run --example local_http
```
