---
sidebar_position: 2
title: Rust
---

# Rust — Superposition OpenFeature Provider

The Rust provider is the native implementation of the Superposition OpenFeature provider. It offers three provider variants for different use cases: a high-level provider with built-in caching, a local resolution provider with pluggable data sources, and a stateless remote API provider.

- **Crate:** [`superposition_provider`](https://crates.io/crates/superposition_provider)

## Installation

Add the following to your `Cargo.toml`:

```toml
[dependencies]
superposition_provider = "<version>"
open-feature = "0.2.5"
tokio = { version = "1", features = ["full"] }
env_logger = "0.10"
```

## Quick Start — High-Level Provider

This is the most common usage — the provider connects to a Superposition server, polls for config updates, and evaluates flags locally with in-memory caching.

```rust
use superposition_provider::{
    SuperpositionProvider, SuperpositionProviderOptions,
    ExperimentationOptions, RefreshStrategy, PollingStrategy,
};
use open_feature::OpenFeature;
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    env_logger::init();

    let mut api = OpenFeature::singleton_mut().await;

    let options = SuperpositionProviderOptions {
        endpoint: "http://localhost:8080/".to_string(),
        token: "your_token_here".to_string(),
        org_id: "localorg".to_string(),
        workspace_id: "test".to_string(),
        fallback_config: None,
        evaluation_cache: None,
        refresh_strategy: RefreshStrategy::Polling(PollingStrategy {
            interval: 60,       // seconds between polls
            timeout: Some(30),  // HTTP request timeout in seconds
        }),
        experimentation_options: Some(ExperimentationOptions {
            refresh_strategy: RefreshStrategy::Polling(PollingStrategy {
                interval: 1,
                timeout: None,
            }),
            evaluation_cache: None,
            default_toss: None,
        }),
    };

    api.set_provider(SuperpositionProvider::new(options)).await;
    let client = api.create_client();

    // Wait for the provider to initialize and fetch config
    sleep(Duration::from_secs(3)).await;

    // Evaluate feature flags
    let string_val = client
        .get_string_value("currency", None, None)
        .await
        .unwrap();
    println!("currency = {}", string_val);

    let int_val = client
        .get_int_value("price", None, None)
        .await
        .unwrap();
    println!("price = {}", int_val);

    let bool_val = client
        .get_bool_value("dark_mode", None, None)
        .await
        .unwrap();
    println!("dark_mode = {}", bool_val);
}
```

## Configuration Options

### `SuperpositionProviderOptions`

| Field                      | Type                              | Required | Description                                          |
| -------------------------- | --------------------------------- | -------- | ---------------------------------------------------- |
| `endpoint`                 | `String`                          | Yes      | Superposition server URL                             |
| `token`                    | `String`                          | Yes      | Authentication token (bearer)                        |
| `org_id`                   | `String`                          | Yes      | Organisation ID                                      |
| `workspace_id`             | `String`                          | Yes      | Workspace ID                                         |
| `refresh_strategy`         | `RefreshStrategy`                 | Yes      | How configs are refreshed                            |
| `fallback_config`          | `Option<Map<String, Value>>`      | No       | Fallback config when server is unreachable           |
| `evaluation_cache`         | `Option<EvaluationCacheOptions>`  | No       | Evaluation result cache settings                     |
| `experimentation_options`  | `Option<ExperimentationOptions>`  | No       | Experimentation / A/B testing settings               |

### Refresh Strategies

The `RefreshStrategy` enum supports four variants:

```rust
// Polling — periodically fetches updates from the server
RefreshStrategy::Polling(PollingStrategy {
    interval: 60,       // seconds between polls (default: 60)
    timeout: Some(30),  // HTTP request timeout in seconds (default: 30)
})

// On-Demand — fetches on first access, then caches with a TTL
RefreshStrategy::OnDemand(OnDemandStrategy {
    ttl: 300,                   // cache TTL in seconds (default: 300)
    use_stale_on_error: true,   // serve stale data on fetch error (default: true)
    timeout: Some(30),          // HTTP timeout in seconds (default: 30)
})

// Watch — uses file-system notifications (for FileDataSource only)
RefreshStrategy::Watch(WatchStrategy {
    debounce_ms: 500,  // debounce interval in milliseconds (default: 500)
})

// Manual — no automatic refresh; user triggers refresh
RefreshStrategy::Manual
```

### `ExperimentationOptions`

| Field              | Type                             | Required | Description                         |
| ------------------ | -------------------------------- | -------- | ----------------------------------- |
| `refresh_strategy` | `RefreshStrategy`                | Yes      | How experiment data is refreshed    |
| `evaluation_cache` | `Option<EvaluationCacheOptions>` | No       | Cache for experiment evaluations    |
| `default_toss`     | `Option<i64>`                    | No       | Default toss value for experiments  |

### `EvaluationCacheOptions`

| Field  | Type            | Default | Description                     |
| ------ | --------------- | ------- | ------------------------------- |
| `ttl`  | `u64`           | `60`    | Cache time-to-live in seconds   |
| `size` | `u64`           | `500`   | Maximum number of cache entries |

## Provider Variants

### 1. `SuperpositionProvider` (High-Level)

The recommended provider for most use cases. Wraps `ConfigurationClient` and `ExperimentationClient`, uses HTTP internally with built-in polling/on-demand refresh.

```rust
let provider = SuperpositionProvider::new(options);
api.set_provider(provider).await;
```

### 2. `LocalResolutionProvider` (Pluggable Data Sources)

A generic provider that accepts pluggable data sources (HTTP, file, or custom). Supports all refresh strategies including `Watch` and `Manual`. Ideal for local-first evaluation with optional server sync.

```rust
use superposition_provider::{
    data_source::http::HttpDataSource,
    data_source::file::FileDataSource,
    local_provider::LocalResolutionProvider,
    SuperpositionOptions,
};

let provider = LocalResolutionProvider::new(
    Box::new(primary_source),         // primary data source
    Some(Box::new(fallback_source)),  // optional fallback
    RefreshStrategy::Polling(PollingStrategy { interval: 10, timeout: Some(10) }),
);
```

### 3. `SuperpositionAPIProvider` (Remote / Stateless)

A stateless provider that makes an HTTP API call on every evaluation. No caching — useful for serverless or low-traffic scenarios.

```rust
use superposition_provider::{
    remote_provider::SuperpositionAPIProvider,
    SuperpositionOptions,
};

let provider = SuperpositionAPIProvider::new(SuperpositionOptions::new(
    "http://localhost:8080".to_string(),
    "token".to_string(),
    "org_id".to_string(),
    "workspace_id".to_string(),
));
```

## Local File Resolution (SuperTOML)

Resolve configs from a local `.toml` file without needing a server:

```rust
use std::path::PathBuf;
use open_feature::EvaluationContext;
use superposition_provider::{
    data_source::file::FileDataSource,
    local_provider::LocalResolutionProvider,
    traits::AllFeatureProvider,
    OnDemandStrategy, RefreshStrategy,
};

#[tokio::main]
async fn main() {
    env_logger::init();

    let file_source = FileDataSource::new(PathBuf::from("config.toml"));

    let provider = LocalResolutionProvider::new(
        Box::new(file_source),
        None,
        RefreshStrategy::OnDemand(OnDemandStrategy {
            ttl: 60,
            ..Default::default()
        }),
    );
    provider.init(EvaluationContext::default()).await.unwrap();

    let context = EvaluationContext::default()
        .with_custom_field("os", "linux")
        .with_custom_field("city", "Boston");

    let config = provider.resolve_all_features(context).await.unwrap();
    println!("Config: {:?}", config);

    provider.close_provider().await.unwrap();
}
```

:::note
The `FileDataSource` supports the `Watch` refresh strategy for automatic reloading on file changes. It does **not** support experimentation.
:::

## Local HTTP with File Fallback

Use the server as the primary data source with a local file as fallback for resilience:

```rust
use superposition_provider::{
    data_source::http::HttpDataSource,
    data_source::file::FileDataSource,
    local_provider::LocalResolutionProvider,
    PollingStrategy, RefreshStrategy, SuperpositionOptions,
};
use open_feature::OpenFeature;
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    env_logger::init();

    let http_source = HttpDataSource::new(SuperpositionOptions::new(
        "http://localhost:8080".to_string(),
        "token".to_string(),
        "localorg".to_string(),
        "dev".to_string(),
    ));

    let file_source = FileDataSource::new("config.toml".into());

    let provider = LocalResolutionProvider::new(
        Box::new(http_source),
        Some(Box::new(file_source)),
        RefreshStrategy::Polling(PollingStrategy {
            interval: 10,
            timeout: Some(10),
        }),
    );

    let mut api = OpenFeature::singleton_mut().await;
    api.set_provider(provider).await;
    let client = api.create_client();

    sleep(Duration::from_secs(2)).await;

    let value = client
        .get_string_value("currency", None, None)
        .await
        .unwrap();
    println!("currency = {}", value);
}
```

## Evaluation Context

Pass dimensions and a targeting key for experiment bucketing:

```rust
use open_feature::EvaluationContext;

let context = EvaluationContext::default()
    .with_targeting_key("user-42")
    .with_custom_field("city", "Berlin")
    .with_custom_field("os", "android");

let value = client
    .get_string_value("currency", Some(&context), None)
    .await
    .unwrap();
```

## Supported Value Types

| Method             | Return Type   |
| ------------------ | ------------- |
| `get_bool_value`   | `bool`        |
| `get_int_value`    | `i64`         |
| `get_float_value`  | `f64`         |
| `get_string_value` | `String`      |
| `get_struct_value` | `StructValue` |

The `LocalResolutionProvider` also implements the `AllFeatureProvider` trait:

```rust
let all_config = provider.resolve_all_features(context).await.unwrap();
```

And `FeatureExperimentMeta` for experiment metadata:

```rust
let variants = provider.get_applicable_variants(context).await.unwrap();
```

## Logging

The provider uses the `log` crate. Enable with `env_logger`:

```bash
RUST_LOG=debug cargo run
```

## Examples

- [`example.rs`](https://github.com/juspay/superposition/blob/main/crates/superposition_provider/example.rs) — Basic OpenFeature usage
- [`local_file_example.rs`](https://github.com/juspay/superposition/blob/main/crates/superposition_provider/examples/local_file_example.rs) — Local file resolution
- [`local_http_example.rs`](https://github.com/juspay/superposition/blob/main/crates/superposition_provider/examples/local_http_example.rs) — Local HTTP resolution
- [`local_with_fallback_example.rs`](https://github.com/juspay/superposition/blob/main/crates/superposition_provider/examples/local_with_fallback_example.rs) — HTTP with file fallback
- [`file_watch_example.rs`](https://github.com/juspay/superposition/blob/main/crates/superposition_provider/examples/file_watch_example.rs) — File watch with auto-reload
