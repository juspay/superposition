---
sidebar_position: 2
title: Rust
---

# Rust — Superposition OpenFeature Provider

The Rust provider is the native implementation of the Superposition OpenFeature provider. It offers two provider variants:

- **`LocalResolutionProvider`** — Fetches config from a data source (HTTP server or local file), caches it locally, and evaluates flags in-process. Supports polling, on-demand, file-watch, and manual refresh strategies. This is the **recommended provider** for most use cases.
- **`SuperpositionAPIProvider`** — A stateless remote provider that makes an HTTP API call to the Superposition server on every evaluation. No local caching — useful for serverless or low-traffic scenarios.

**Crate:** [`superposition_provider`](https://crates.io/crates/superposition_provider)

## Installation

Add the following to your `Cargo.toml`:

```toml
[dependencies]
superposition_provider = "<version>"
open-feature = "0.2.5"
tokio = { version = "1", features = ["full"] }
env_logger = "0.10"
```

## Quick Start

This is the most common usage — the provider connects to a Superposition server via HTTP, polls for config updates, and evaluates flags locally.

```rust
use open_feature::{EvaluationContext, OpenFeature};
use superposition_provider::{
    data_source::http::HttpDataSource,
    local_provider::LocalResolutionProvider,
    PollingStrategy, RefreshStrategy, SuperpositionOptions,
};
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    env_logger::init();

    // 1. Create an HTTP data source pointing to your Superposition server
    let http_source = HttpDataSource::new(SuperpositionOptions::new(
        "http://localhost:8080".to_string(),
        "your_token_here".to_string(),
        "localorg".to_string(),
        "test".to_string(),
    ));

    // 2. Create the provider with a polling refresh strategy
    let provider = LocalResolutionProvider::new(
        Box::new(http_source),
        None, // no fallback data source
        RefreshStrategy::Polling(PollingStrategy {
            interval: 60,       // seconds between polls
            timeout: Some(30),  // HTTP request timeout in seconds
        }),
    );

    // 3. Register with OpenFeature and create a client
    let mut api = OpenFeature::singleton_mut().await;
    api.set_provider(provider).await;
    let client = api.create_client();

    // Allow time for the provider to initialize
    sleep(Duration::from_secs(2)).await;

    // 4. Evaluate feature flags
    let context = EvaluationContext::default()
        .with_targeting_key("user-42")
        .with_custom_field("city", "Berlin");

    let string_val = client
        .get_string_value("currency", Some(&context), None)
        .await
        .unwrap();
    println!("currency = {}", string_val);

    let int_val = client
        .get_int_value("price", Some(&context), None)
        .await
        .unwrap();
    println!("price = {}", int_val);

    let bool_val = client
        .get_bool_value("dark_mode", Some(&context), None)
        .await
        .unwrap();
    println!("dark_mode = {}", bool_val);
}
```

## Configuration Options

### `SuperpositionOptions`

Connection options shared by `HttpDataSource` and `SuperpositionAPIProvider`:

| Field          | Type     | Required | Description                    |
| -------------- | -------- | -------- | ------------------------------ |
| `endpoint`     | `String` | Yes      | Superposition server URL       |
| `token`        | `String` | Yes      | Authentication token (bearer)  |
| `org_id`       | `String` | Yes      | Organisation ID                |
| `workspace_id` | `String` | Yes      | Workspace ID                   |

```rust
let options = SuperpositionOptions::new(
    "http://localhost:8080".to_string(),
    "your_token".to_string(),
    "localorg".to_string(),
    "test".to_string(),
);
```

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
    ttl: 300,                        // cache TTL in seconds (default: 300)
    use_stale_on_error: Some(true),  // serve stale data on fetch error (default: Some(true))
    timeout: Some(30),               // HTTP timeout in seconds (default: Some(30))
})

// Watch — uses file-system notifications (for FileDataSource only)
RefreshStrategy::Watch(WatchStrategy {
    debounce_ms: Some(500),  // debounce interval in milliseconds (default: Some(500))
})

// Manual — no automatic refresh; user triggers refresh
RefreshStrategy::Manual
```

### `ExperimentationOptions`

| Field              | Type                             | Required | Description                         |
| ------------------ | -------------------------------- | -------- | ----------------------------------- |
| `refresh_strategy` | `RefreshStrategy`                | Yes      | How experiment data is refreshed    |
| `evaluation_cache` | `Option<EvaluationCacheOptions>` | No       | Cache for experiment evaluations    |
| `default_toss`     | `Option<u32>`                    | No       | Default toss value for experiments  |

`ExperimentationOptions` supports a builder pattern:

```rust
let exp_options = ExperimentationOptions::new(
    RefreshStrategy::Polling(PollingStrategy { interval: 5, timeout: Some(3) }),
)
.with_evaluation_cache(EvaluationCacheOptions::default())
.with_default_toss(50);
```

### `EvaluationCacheOptions`

| Field  | Type             | Default     | Description                     |
| ------ | ---------------- | ----------- | ------------------------------- |
| `ttl`  | `Option<u64>`    | `Some(60)`  | Cache time-to-live in seconds   |
| `size` | `Option<usize>`  | `Some(500)` | Maximum number of cache entries |

## Provider Variants

### 1. `LocalResolutionProvider` (Recommended)

Fetches config from a pluggable data source (HTTP or file), caches locally, and evaluates flags in-process. Supports all four refresh strategies. Accepts an optional fallback data source.

```rust
use open_feature::EvaluationContext;
use superposition_provider::{
    data_source::http::HttpDataSource,
    local_provider::LocalResolutionProvider,
    traits::{AllFeatureProvider, FeatureExperimentMeta},
    PollingStrategy, RefreshStrategy, SuperpositionOptions,
};

let http_source = HttpDataSource::new(SuperpositionOptions::new(
    "http://localhost:8080".to_string(),
    "token".to_string(),
    "localorg".to_string(),
    "dev".to_string(),
));

let provider = LocalResolutionProvider::new(
    Box::new(http_source),
    None, // optional fallback: Option<Box<dyn SuperpositionDataSource>>
    RefreshStrategy::Polling(PollingStrategy { interval: 30, timeout: Some(10) }),
);

// Initialize the provider (fetches initial config)
provider.init(EvaluationContext::default()).await.unwrap();

// Resolve all features
let context = EvaluationContext::default()
    .with_targeting_key("user-1234")
    .with_custom_field("dimension", "d2");

let all_config = provider.resolve_all_features(context.clone()).await.unwrap();
println!("All config: {:?}", all_config);

// Get applicable experiment variants
let variants = provider.get_applicable_variants(context, None).await.unwrap();
println!("Variants: {:?}", variants);

// Cleanup
provider.close_provider().await.unwrap();
```

**Key capabilities:**

- **Pluggable data sources** — use `HttpDataSource` for server-backed, `FileDataSource` for local TOML files, or implement the `SuperpositionDataSource` trait for custom sources
- **Optional fallback** — provide a secondary data source (e.g. a local file) that is used when the primary source fails
- **Manual refresh** — call `provider.refresh().await` to trigger a config refresh on demand
- **Chainable** — `LocalResolutionProvider` itself implements `SuperpositionDataSource`, so it can be used as a data source for another provider

### 2. `SuperpositionAPIProvider` (Remote / Stateless)

A stateless provider that calls the Superposition server on every evaluation. No local caching — each flag evaluation makes an HTTP request. Best for serverless, low-traffic, or scenarios where you always want the latest config.

```rust
use open_feature::{EvaluationContext, OpenFeature};
use superposition_provider::{
    remote_provider::SuperpositionAPIProvider,
    SuperpositionOptions,
};

let provider = SuperpositionAPIProvider::new(SuperpositionOptions::new(
    "http://localhost:8080".to_string(),
    "token".to_string(),
    "localorg".to_string(),
    "dev".to_string(),
));

// Use with OpenFeature
let mut api = OpenFeature::singleton_mut().await;
api.set_provider(provider).await;
let client = api.create_client();

let context = EvaluationContext::default()
    .with_custom_field("city", "Berlin");

let value = client
    .get_string_value("currency", Some(&context), None)
    .await
    .unwrap();
println!("currency = {}", value);
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

Use the server as the primary data source with a local TOML file as fallback. If the HTTP source fails during initialization, the provider loads config from the fallback file instead — ensuring your application always starts with a valid configuration.

```rust
use std::path::PathBuf;
use open_feature::{EvaluationContext, OpenFeature};
use superposition_provider::{
    data_source::file::FileDataSource,
    data_source::http::HttpDataSource,
    local_provider::LocalResolutionProvider,
    PollingStrategy, RefreshStrategy, SuperpositionOptions,
};
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    env_logger::init();

    // Primary: HTTP data source (Superposition server)
    let http_source = HttpDataSource::new(SuperpositionOptions::new(
        "http://localhost:8080".to_string(),
        "token".to_string(),
        "localorg".to_string(),
        "dev".to_string(),
    ));

    // Fallback: local TOML config file
    let file_source = FileDataSource::new(PathBuf::from("config.toml"));

    let provider = LocalResolutionProvider::new(
        Box::new(http_source),
        Some(Box::new(file_source)), // used when HTTP source is unavailable
        RefreshStrategy::Polling(PollingStrategy {
            interval: 10,
            timeout: Some(10),
        }),
    );

    // Register with OpenFeature
    let mut api = OpenFeature::singleton_mut().await;
    api.set_provider(provider).await;
    let client = api.create_client();

    sleep(Duration::from_secs(2)).await;

    let context = EvaluationContext::default()
        .with_targeting_key("user-456")
        .with_custom_field("os", "linux")
        .with_custom_field("city", "Berlin");

    let currency = client
        .get_string_value("currency", Some(&context), None)
        .await
        .unwrap();
    println!("currency = {}", currency);

    let price = client
        .get_int_value("price", Some(&context), None)
        .await
        .unwrap();
    println!("price = {}", price);
}
```

:::tip
The fallback is only consulted during initialization or when the primary source fails. Once the primary source succeeds, the provider uses its data exclusively. Polling continues to try the primary source on each interval.
:::

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

The `LocalResolutionProvider` and `SuperpositionAPIProvider` also implement the `AllFeatureProvider` trait:

```rust
// Resolve all features
let all_config = provider.resolve_all_features(context.clone()).await.unwrap();

// Resolve with a prefix filter (e.g. only keys starting with "payment.")
let filtered = provider
    .resolve_all_features_with_filter(
        context.clone(),
        Some(vec!["payment.".to_string()]),
    )
    .await
    .unwrap();
```

And `FeatureExperimentMeta` for experiment metadata:

```rust
// Get applicable experiment variant IDs
let variants = provider
    .get_applicable_variants(
        context,
        None, // optional prefix filter: Option<Vec<String>>
    )
    .await
    .unwrap();
```

## Logging

The provider uses the `log` crate. Enable with `env_logger`:

```bash
RUST_LOG=debug cargo run
```

## Examples

- [`polling_example.rs`](https://github.com/juspay/superposition/blob/main/crates/superposition_provider/examples/polling_example.rs) — Polling with OpenFeature client
- [`local_http_example.rs`](https://github.com/juspay/superposition/blob/main/crates/superposition_provider/examples/local_http_example.rs) — HTTP data source with `AllFeatureProvider` trait
- [`local_file_example.rs`](https://github.com/juspay/superposition/blob/main/crates/superposition_provider/examples/local_file_example.rs) — Local TOML file resolution
- [`local_file_watch_example.rs`](https://github.com/juspay/superposition/blob/main/crates/superposition_provider/examples/local_file_watch_example.rs) — File watch with auto-reload
- [`local_with_fallback_example.rs`](https://github.com/juspay/superposition/blob/main/crates/superposition_provider/examples/local_with_fallback_example.rs) — HTTP primary with file fallback
