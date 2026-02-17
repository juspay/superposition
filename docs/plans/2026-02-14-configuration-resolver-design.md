# Configuration Resolver (Superposition Provider Refactor)

## Overview

Refactor the `superposition_provider` crate to introduce a trait-based architecture with pluggable data sources, supporting both local (in-process) and remote configuration resolution. The existing implementation (`provider.rs`, `client.rs`) is kept for comparison.

## Goals

1. Define clean trait abstractions: `AllFeatureProvider`, `FeatureExperimentMeta`, `SuperpositionDataSource`
2. Support pluggable data sources (HTTP, File/TOML) via `SuperpositionDataSource`
3. Implement `LocalResolutionProvider` with primary + fallback data sources and multiple refresh strategies
4. Implement `SuperpositionAPIProvider` for remote resolution
5. All providers implement OpenFeature's `FeatureProvider` trait
6. Provide examples for HTTP, File, and primary+fallback configurations

## Module Layout

```
crates/superposition_provider/src/
├── lib.rs                         # Re-exports
├── types.rs                       # Error types, options (existing, extended)
├── traits.rs                      # AllFeatureProvider + FeatureExperimentMeta traits
├── data_source.rs                 # SuperpositionDataSource trait, ConfigData, ExperimentData
├── data_source/
│   ├── http.rs                    # HttpDataSource
│   └── file.rs                    # FileDataSource (TOML)
├── local_provider.rs              # LocalResolutionProvider
├── remote_provider.rs             # SuperpositionAPIProvider
├── utils.rs                       # Existing ConversionUtils (kept)
├── provider.rs                    # Existing provider (kept for comparison)
└── client.rs                      # Existing client (kept for comparison)
```

Uses Rust 2018 module style (`data_source.rs` + `data_source/` directory, no `mod.rs`).

## Core Traits

### AllFeatureProvider

Bulk configuration resolution. Returns all (or filtered) resolved config values.

```rust
#[async_trait]
pub trait AllFeatureProvider: Send + Sync {
    async fn resolve_all_features(
        &self,
        context: &EvaluationContext,
    ) -> Result<Map<String, Value>>;

    async fn resolve_all_features_with_filter(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>>;
}
```

### FeatureExperimentMeta

Experiment variant resolution.

```rust
#[async_trait]
pub trait FeatureExperimentMeta: Send + Sync {
    async fn get_applicable_variants(
        &self,
        context: &EvaluationContext,
    ) -> Result<Vec<String>>;
}
```

### SuperpositionDataSource

Abstraction for raw data fetching. Each source implements its own filtering logic.

```rust
pub struct ConfigData {
    pub config: Config,
    pub fetched_at: DateTime<Utc>,
}

pub struct ExperimentData {
    pub experiments: Experiments,
    pub experiment_groups: ExperimentGroups,
    pub fetched_at: DateTime<Utc>,
}

#[async_trait]
pub trait SuperpositionDataSource: Send + Sync {
    async fn fetch_config(&self) -> Result<ConfigData>;

    async fn fetch_filtered_config(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<ConfigData>;

    async fn fetch_active_experiments(&self) -> Result<Option<ExperimentData>>;

    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>>;

    async fn fetch_matching_active_experiments(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>>;

    fn supports_experiments(&self) -> bool;

    async fn close(&self) -> Result<()>;
}
```

- `fetch_candidate_active_experiments` - partial context matching (uses `Contextual::filter_by_eval`)
- `fetch_matching_active_experiments` - exact context matching (uses `Contextual::filter_exact_match`)

## Data Source Implementations

### HttpDataSource

Wraps `superposition_sdk::Client`. Takes `SuperpositionOptions`.

- `fetch_config` → `client.get_config()`
- `fetch_filtered_config` → fetches full config, filters locally via `Config::filter_by_prefix` / `Config::filter_by_dimensions`
- `fetch_active_experiments` → `client.list_experiment()` + `client.list_experiment_groups()`
- `fetch_candidate_active_experiments` → fetches all, filters via `Contextual::filter_by_eval`
- `fetch_matching_active_experiments` → fetches all, filters via `Contextual::filter_exact_match`
- `supports_experiments` → `true`
- `close` → no-op

### FileDataSource

Reads TOML config via `superposition_core::toml::parse_toml_config`. Takes `PathBuf`.

- `fetch_config` → reads file, parses TOML
- `fetch_filtered_config` → parses full config, filters locally
- All experiment methods → `Ok(None)`
- `supports_experiments` → `false`
- `close` → no-op

## LocalResolutionProvider

In-process resolver with primary + optional fallback data source.

```rust
pub struct LocalResolutionProvider {
    primary: Box<dyn SuperpositionDataSource>,
    fallback: Option<Box<dyn SuperpositionDataSource>>,
    refresh_strategy: RefreshStrategy,
    cached_config: Arc<RwLock<Option<ConfigData>>>,
    cached_experiments: Arc<RwLock<Option<ExperimentData>>>,
    polling_task: RwLock<Option<JoinHandle<()>>>,
}
```

### Initialization

1. Fetch from primary data source
2. If primary fails and fallback exists, fetch from fallback
3. If both fail, return error
4. Start refresh strategy

### Refresh Strategies

- **Polling** - spawns tokio task that periodically fetches from primary
- **OnDemand** - lazy fetch with TTL; uses stale data if fetch fails (configurable)
- **Manual** - no automatic refresh; caller invokes `refresh()` explicitly

On refresh failure: keep last known good data (never overwrite cache with error).

### Trait Implementations

- **AllFeatureProvider**: reads cached config, calls `eval_config`. If experiments cached, calls `get_applicable_variants` and injects `variantIds` into context before evaluation.
- **FeatureExperimentMeta**: reads cached experiments/groups, calls `get_applicable_variants`.
- **FeatureProvider** (OpenFeature): delegates to `resolve_all_features`, extracts single key.

### Public API

```rust
impl LocalResolutionProvider {
    pub fn new(primary, fallback, refresh_strategy) -> Self;
    pub async fn init(&self) -> Result<()>;
    pub async fn refresh(&self) -> Result<()>;
    pub async fn close(&self) -> Result<()>;
}
```

## SuperpositionAPIProvider (Remote Resolution)

Sends context over network, gets resolved config back.

```rust
pub struct SuperpositionAPIProvider {
    options: SuperpositionOptions,
    cache: Option<RwLock<ResponseCache>>,
}
```

- Uses `superposition_sdk::Client` to call resolve APIs
- Optional local response cache keyed on hash of (context + targeting_key) with TTL eviction
- Implements `AllFeatureProvider`, `FeatureExperimentMeta`, and OpenFeature `FeatureProvider`

## EvaluationContext

Re-uses `open_feature::EvaluationContext` directly. No custom wrapper type. All traits and providers accept `&EvaluationContext` from the OpenFeature SDK.

## Examples

Three example binaries in `crates/superposition_provider/examples/`:

### `local_http_example.rs`
LocalResolutionProvider with HTTP source, polling refresh.

### `local_file_example.rs`
LocalResolutionProvider with TOML file source, on-demand refresh.

### `local_with_fallback_example.rs`
LocalResolutionProvider with HTTP primary + TOML fallback. Demonstrates:
- Bulk resolution (`resolve_all_features`)
- Filtered resolution (`resolve_all_features_with_filter`)
- Experiment variants (`get_applicable_variants`)
- Manual refresh (`refresh()`)
