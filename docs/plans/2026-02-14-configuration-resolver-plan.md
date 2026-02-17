# Configuration Resolver Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refactor the `superposition_provider` crate to use a trait-based architecture with pluggable data sources (HTTP, File), supporting both local and remote configuration resolution.

**Architecture:** Three core traits (`AllFeatureProvider`, `FeatureExperimentMeta`, `SuperpositionDataSource`) with two provider implementations (`LocalResolutionProvider`, `SuperpositionAPIProvider`). Data sources are pluggable via `SuperpositionDataSource` trait. All providers implement OpenFeature's `FeatureProvider`.

**Tech Stack:** Rust, async-trait, tokio, serde_json, open-feature, superposition_core (eval_config, get_applicable_variants, parse_toml_config), superposition_sdk (HTTP client), superposition_types (Config, Contextual)

**Design doc:** `docs/plans/2026-02-14-configuration-resolver-design.md`

---

### Task 1: Define core traits (`traits.rs`)

**Files:**
- Create: `crates/superposition_provider/src/traits.rs`
- Modify: `crates/superposition_provider/src/lib.rs`

**Step 1: Create `traits.rs` with `AllFeatureProvider` and `FeatureExperimentMeta` traits**

```rust
use async_trait::async_trait;
use open_feature::EvaluationContext;
use serde_json::{Map, Value};

use crate::types::Result;

/// Trait for bulk configuration resolution
///
/// Provides methods to resolve all feature flags at once,
/// which is more efficient than resolving them one by one.
#[async_trait]
pub trait AllFeatureProvider: Send + Sync {
    /// Resolve all features for the given evaluation context
    async fn resolve_all_features(
        &self,
        context: &EvaluationContext,
    ) -> Result<Map<String, Value>>;

    /// Resolve features matching prefix filters
    ///
    /// If prefix_filter is None, behaves like resolve_all_features
    async fn resolve_all_features_with_filter(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>>;
}

/// Trait for experiment metadata and variant resolution
#[async_trait]
pub trait FeatureExperimentMeta: Send + Sync {
    /// Get applicable variant IDs for the given context
    async fn get_applicable_variants(
        &self,
        context: &EvaluationContext,
    ) -> Result<Vec<String>>;
}
```

**Step 2: Add `traits` module to `lib.rs`**

Add `pub mod traits;` to `crates/superposition_provider/src/lib.rs` and re-export: `pub use traits::*;`

**Step 3: Verify it compiles**

Run: `cargo check -p superposition_provider`
Expected: compiles with no errors

**Step 4: Commit**

```bash
git add crates/superposition_provider/src/traits.rs crates/superposition_provider/src/lib.rs
git commit -m "feat: add AllFeatureProvider and FeatureExperimentMeta traits"
```

---

### Task 2: Define `SuperpositionDataSource` trait (`data_source.rs`)

**Files:**
- Create: `crates/superposition_provider/src/data_source.rs`
- Modify: `crates/superposition_provider/src/lib.rs`

**Step 1: Create `data_source.rs` with trait, `ConfigData`, and `ExperimentData`**

```rust
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde_json::{Map, Value};
use superposition_core::experiment::ExperimentGroups;
use superposition_core::Experiments;
use superposition_types::Config;

use crate::types::Result;

pub mod file;
pub mod http;

/// Data fetched from a configuration source
#[derive(Debug, Clone)]
pub struct ConfigData {
    pub config: Config,
    pub fetched_at: DateTime<Utc>,
}

impl ConfigData {
    pub fn new(config: Config) -> Self {
        Self {
            config,
            fetched_at: Utc::now(),
        }
    }
}

/// Experiment data fetched from a source
#[derive(Debug, Clone)]
pub struct ExperimentData {
    pub experiments: Experiments,
    pub experiment_groups: ExperimentGroups,
    pub fetched_at: DateTime<Utc>,
}

impl ExperimentData {
    pub fn new(experiments: Experiments, experiment_groups: ExperimentGroups) -> Self {
        Self {
            experiments,
            experiment_groups,
            fetched_at: Utc::now(),
        }
    }
}

/// Trait for abstracting data sources for Superposition configuration and experiments
///
/// Allows plugging different data sources (HTTP, File, Redis, etc.)
/// into the Superposition provider system. Each implementation handles
/// its own filtering logic.
#[async_trait]
pub trait SuperpositionDataSource: Send + Sync {
    /// Fetch the latest configuration from the data source
    async fn fetch_config(&self) -> Result<ConfigData>;

    /// Fetch configuration with context/prefix filters
    ///
    /// Each data source implements its own filtering strategy.
    /// HTTP may use server-side filtering; File filters after parse.
    async fn fetch_filtered_config(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<ConfigData>;

    /// Fetch all active experiment data
    ///
    /// Returns None if the data source doesn't support experiments
    async fn fetch_active_experiments(&self) -> Result<Option<ExperimentData>>;

    /// Fetch active experiments filtered with partial context matching
    ///
    /// Uses partial/candidate matching - experiments whose context
    /// could potentially match the given dimensions.
    /// Returns None if the data source doesn't support experiments.
    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>>;

    /// Fetch active experiments filtered with exact context matching
    ///
    /// Uses exact matching - only experiments whose context
    /// exactly matches the given dimensions.
    /// Returns None if the data source doesn't support experiments.
    async fn fetch_matching_active_experiments(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>>;

    /// Check if this data source supports experiments
    fn supports_experiments(&self) -> bool;

    /// Close and cleanup resources used by this data source
    async fn close(&self) -> Result<()>;
}
```

**Step 2: Create placeholder files for submodules**

Create empty `crates/superposition_provider/src/data_source/http.rs` and `crates/superposition_provider/src/data_source/file.rs` (just comments for now so it compiles).

**Step 3: Add `data_source` module to `lib.rs`**

Add `pub mod data_source;` and re-export: `pub use data_source::{ConfigData, ExperimentData, SuperpositionDataSource};`

**Step 4: Verify it compiles**

Run: `cargo check -p superposition_provider`
Expected: compiles with no errors

**Step 5: Commit**

```bash
git add crates/superposition_provider/src/data_source.rs crates/superposition_provider/src/data_source/ crates/superposition_provider/src/lib.rs
git commit -m "feat: add SuperpositionDataSource trait with ConfigData and ExperimentData"
```

---

### Task 3: Implement `HttpDataSource`

**Files:**
- Modify: `crates/superposition_provider/src/data_source/http.rs`

**Reference files:**
- `crates/superposition_provider/src/client.rs` (existing HTTP fetch logic in `CacConfig::get_config_static`, `ExperimentationConfig::get_experiments_static`, `ExperimentationConfig::get_experiment_groups_static`)
- `crates/superposition_provider/src/utils.rs` (`ConversionUtils::convert_get_config_response`, `convert_experiments_response`, `convert_experiment_groups_response`)

**Step 1: Implement `HttpDataSource` struct and `SuperpositionDataSource` trait**

```rust
use std::collections::HashSet;

use async_trait::async_trait;
use log::info;
use serde_json::{Map, Value};
use superposition_types::logic::{apply, partial_apply};

use crate::data_source::{ConfigData, ExperimentData, SuperpositionDataSource};
use crate::types::{Result, SuperpositionError, SuperpositionOptions};
use crate::utils::ConversionUtils;

/// HTTP-based data source using the Superposition SDK
///
/// Fetches configuration and experiment data from the Superposition
/// service over HTTP.
pub struct HttpDataSource {
    options: SuperpositionOptions,
}

impl HttpDataSource {
    pub fn new(options: SuperpositionOptions) -> Self {
        Self { options }
    }

    fn create_client(&self) -> superposition_sdk::Client {
        use superposition_sdk::{Client, Config as SdkConfig};

        let sdk_config = SdkConfig::builder()
            .endpoint_url(&self.options.endpoint)
            .bearer_token(self.options.token.clone().into())
            .behavior_version_latest()
            .build();

        Client::from_conf(sdk_config)
    }

    async fn fetch_experiments_and_groups(&self) -> Result<ExperimentData> {
        use superposition_sdk::types::ExperimentStatusType;

        let client = self.create_client();

        let (experiments_result, groups_result) = tokio::join!(
            client
                .list_experiment()
                .workspace_id(&self.options.workspace_id)
                .org_id(&self.options.org_id)
                .all(true)
                .status(ExperimentStatusType::Created)
                .status(ExperimentStatusType::Inprogress)
                .send(),
            client
                .list_experiment_groups()
                .workspace_id(&self.options.workspace_id)
                .org_id(&self.options.org_id)
                .all(true)
                .send()
        );

        let experiments_response = experiments_result.map_err(|e| {
            SuperpositionError::NetworkError(format!("Failed to list experiments: {}", e))
        })?;
        let groups_response = groups_result.map_err(|e| {
            SuperpositionError::NetworkError(format!("Failed to list experiment groups: {}", e))
        })?;

        let experiments = ConversionUtils::convert_experiments_response(&experiments_response)?;
        let groups = ConversionUtils::convert_experiment_groups_response(&groups_response)?;

        Ok(ExperimentData::new(experiments, groups))
    }

    /// Filter experiments by partial context match (any dimension matches)
    fn filter_experiments_candidate(
        exp_data: &ExperimentData,
        context: &Map<String, Value>,
    ) -> ExperimentData {
        let filtered_experiments = exp_data
            .experiments
            .iter()
            .filter(|exp| partial_apply(&exp.context, context))
            .cloned()
            .collect();

        let filtered_groups = exp_data
            .experiment_groups
            .iter()
            .filter(|group| partial_apply(&group.context, context))
            .cloned()
            .collect();

        ExperimentData::new(filtered_experiments, filtered_groups)
    }

    /// Filter experiments by exact context match (all dimensions must match)
    fn filter_experiments_matching(
        exp_data: &ExperimentData,
        context: &Map<String, Value>,
    ) -> ExperimentData {
        let filtered_experiments = exp_data
            .experiments
            .iter()
            .filter(|exp| apply(&exp.context, context))
            .cloned()
            .collect();

        let filtered_groups = exp_data
            .experiment_groups
            .iter()
            .filter(|group| apply(&group.context, context))
            .cloned()
            .collect();

        ExperimentData::new(filtered_experiments, filtered_groups)
    }

    /// Filter experiments by prefix - keep experiments that have variants
    /// with override keys matching any of the given prefixes
    fn filter_experiments_by_prefix(
        exp_data: &ExperimentData,
        prefixes: &[String],
    ) -> ExperimentData {
        let filtered_experiments = exp_data
            .experiments
            .iter()
            .filter(|exp| {
                exp.variants.iter().any(|variant| {
                    let overrides: Map<String, Value> = variant.overrides.clone().into();
                    overrides.keys().any(|key| {
                        prefixes.iter().any(|prefix| key.starts_with(prefix))
                    })
                })
            })
            .cloned()
            .collect();

        // Keep all groups - they reference experiments by ID
        ExperimentData::new(filtered_experiments, exp_data.experiment_groups.clone())
    }
}

#[async_trait]
impl SuperpositionDataSource for HttpDataSource {
    async fn fetch_config(&self) -> Result<ConfigData> {
        let client = self.create_client();

        info!("HttpDataSource: fetching config");

        let response = client
            .get_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id)
            .send()
            .await
            .map_err(|e| {
                SuperpositionError::NetworkError(format!("Failed to get config: {}", e))
            })?;

        let config = ConversionUtils::convert_get_config_response(&response)?;
        Ok(ConfigData::new(config))
    }

    async fn fetch_filtered_config(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<ConfigData> {
        let config_data = self.fetch_config().await?;
        let mut config = config_data.config;

        if let Some(ctx) = context {
            if !ctx.is_empty() {
                config = config.filter_by_dimensions(ctx);
            }
        }

        if let Some(prefixes) = prefix_filter {
            if !prefixes.is_empty() {
                config = config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
            }
        }

        Ok(ConfigData::new(config))
    }

    async fn fetch_active_experiments(&self) -> Result<Option<ExperimentData>> {
        let data = self.fetch_experiments_and_groups().await?;
        Ok(Some(data))
    }

    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>> {
        let mut data = self.fetch_experiments_and_groups().await?;

        if let Some(ctx) = context {
            if !ctx.is_empty() {
                data = Self::filter_experiments_candidate(&data, ctx);
            }
        }

        if let Some(prefixes) = prefix_filter {
            if !prefixes.is_empty() {
                data = Self::filter_experiments_by_prefix(&data, prefixes);
            }
        }

        Ok(Some(data))
    }

    async fn fetch_matching_active_experiments(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>> {
        let mut data = self.fetch_experiments_and_groups().await?;

        if let Some(ctx) = context {
            if !ctx.is_empty() {
                data = Self::filter_experiments_matching(&data, ctx);
            }
        }

        if let Some(prefixes) = prefix_filter {
            if !prefixes.is_empty() {
                data = Self::filter_experiments_by_prefix(&data, prefixes);
            }
        }

        Ok(Some(data))
    }

    fn supports_experiments(&self) -> bool {
        true
    }

    async fn close(&self) -> Result<()> {
        Ok(())
    }
}
```

**Step 2: Verify it compiles**

Run: `cargo check -p superposition_provider`
Expected: compiles with no errors (may need minor adjustments for `partial_apply` / `apply` import paths)

**Step 3: Commit**

```bash
git add crates/superposition_provider/src/data_source/http.rs
git commit -m "feat: implement HttpDataSource for SuperpositionDataSource trait"
```

---

### Task 4: Implement `FileDataSource`

**Files:**
- Modify: `crates/superposition_provider/src/data_source/file.rs`

**Reference files:**
- `crates/superposition_core/src/toml.rs` (`parse_toml_config`)

**Step 1: Implement `FileDataSource`**

```rust
use std::collections::HashSet;
use std::path::PathBuf;

use async_trait::async_trait;
use log::info;
use serde_json::{Map, Value};
use superposition_core::toml::parse_toml_config;

use crate::data_source::{ConfigData, ExperimentData, SuperpositionDataSource};
use crate::types::{Result, SuperpositionError};

/// File-based data source using TOML configuration files
///
/// Reads configuration from a TOML file using `superposition_core::toml::parse_toml_config`.
/// Does not support experiments - all experiment methods return `Ok(None)`.
pub struct FileDataSource {
    file_path: PathBuf,
}

impl FileDataSource {
    pub fn new(file_path: PathBuf) -> Self {
        Self { file_path }
    }
}

#[async_trait]
impl SuperpositionDataSource for FileDataSource {
    async fn fetch_config(&self) -> Result<ConfigData> {
        info!("FileDataSource: reading config from {:?}", self.file_path);

        let content = tokio::fs::read_to_string(&self.file_path)
            .await
            .map_err(|e| {
                SuperpositionError::ConfigError(format!(
                    "Failed to read config file {:?}: {}",
                    self.file_path, e
                ))
            })?;

        let config = parse_toml_config(&content).map_err(|e| {
            SuperpositionError::ConfigError(format!(
                "Failed to parse TOML config from {:?}: {}",
                self.file_path, e
            ))
        })?;

        Ok(ConfigData::new(config))
    }

    async fn fetch_filtered_config(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<ConfigData> {
        let config_data = self.fetch_config().await?;
        let mut config = config_data.config;

        if let Some(ctx) = context {
            if !ctx.is_empty() {
                config = config.filter_by_dimensions(ctx);
            }
        }

        if let Some(prefixes) = prefix_filter {
            if !prefixes.is_empty() {
                config = config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
            }
        }

        Ok(ConfigData::new(config))
    }

    async fn fetch_active_experiments(&self) -> Result<Option<ExperimentData>> {
        Ok(None)
    }

    async fn fetch_candidate_active_experiments(
        &self,
        _context: Option<&Map<String, Value>>,
        _prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>> {
        Ok(None)
    }

    async fn fetch_matching_active_experiments(
        &self,
        _context: Option<&Map<String, Value>>,
        _prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>> {
        Ok(None)
    }

    fn supports_experiments(&self) -> bool {
        false
    }

    async fn close(&self) -> Result<()> {
        Ok(())
    }
}
```

**Step 2: Verify it compiles**

Run: `cargo check -p superposition_provider`
Expected: compiles with no errors

**Step 3: Commit**

```bash
git add crates/superposition_provider/src/data_source/file.rs
git commit -m "feat: implement FileDataSource for SuperpositionDataSource trait"
```

---

### Task 5: Add `Manual` variant to `RefreshStrategy`

**Files:**
- Modify: `crates/superposition_provider/src/types.rs`

**Step 1: Add `Manual` variant to `RefreshStrategy` enum**

Add to the existing `RefreshStrategy` enum in `types.rs`:

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RefreshStrategy {
    Polling(PollingStrategy),
    OnDemand(OnDemandStrategy),
    Manual,
}
```

**Step 2: Verify it compiles**

Run: `cargo check -p superposition_provider`
Expected: compiles (existing match statements in `client.rs` will need a `Manual` arm - add a no-op arm if needed)

**Step 3: Commit**

```bash
git add crates/superposition_provider/src/types.rs crates/superposition_provider/src/client.rs
git commit -m "feat: add Manual variant to RefreshStrategy"
```

---

### Task 6: Implement `LocalResolutionProvider`

**Files:**
- Create: `crates/superposition_provider/src/local_provider.rs`
- Modify: `crates/superposition_provider/src/lib.rs`

**Reference files:**
- `crates/superposition_provider/src/provider.rs` (existing `SuperpositionProvider` for pattern reference)
- `crates/superposition_provider/src/client.rs` (existing refresh/polling logic)
- `crates/superposition_core/src/config.rs` (`eval_config`)
- `crates/superposition_core/src/experiment.rs` (`get_applicable_variants`)

**Step 1: Create `local_provider.rs` with struct and construction/init/refresh/close**

```rust
use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use log::{debug, error, info, warn};
use open_feature::{
    provider::{FeatureProvider, ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationError, EvaluationErrorCode, EvaluationResult, StructValue,
};
use serde_json::{Map, Value};
use superposition_core::{eval_config, get_applicable_variants, MergeStrategy};
use superposition_types::DimensionInfo;
use tokio::sync::RwLock;
use tokio::task::JoinHandle;
use tokio::time::{sleep, Duration};

use crate::data_source::{ConfigData, ExperimentData, SuperpositionDataSource};
use crate::traits::{AllFeatureProvider, FeatureExperimentMeta};
use crate::types::*;
use crate::utils::ConversionUtils;

/// Local (in-process) resolution provider
///
/// Caches raw configuration and experiment data from a primary data source
/// (with optional fallback), and resolves configuration locally using
/// `superposition_core::eval_config`.
pub struct LocalResolutionProvider {
    primary: Arc<dyn SuperpositionDataSource>,
    fallback: Option<Arc<dyn SuperpositionDataSource>>,
    refresh_strategy: RefreshStrategy,
    cached_config: Arc<RwLock<Option<ConfigData>>>,
    cached_experiments: Arc<RwLock<Option<ExperimentData>>>,
    polling_task: RwLock<Option<JoinHandle<()>>>,
    metadata: ProviderMetadata,
    status: RwLock<ProviderStatus>,
}

impl LocalResolutionProvider {
    pub fn new(
        primary: Box<dyn SuperpositionDataSource>,
        fallback: Option<Box<dyn SuperpositionDataSource>>,
        refresh_strategy: RefreshStrategy,
    ) -> Self {
        Self {
            primary: Arc::from(primary),
            fallback: fallback.map(Arc::from),
            refresh_strategy,
            cached_config: Arc::new(RwLock::new(None)),
            cached_experiments: Arc::new(RwLock::new(None)),
            polling_task: RwLock::new(None),
            metadata: ProviderMetadata {
                name: "LocalResolutionProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
        }
    }

    /// Initialize the provider: fetch initial data and start refresh strategy
    pub async fn init(&self) -> Result<()> {
        info!("Initializing LocalResolutionProvider...");

        // Try primary first
        let config_result = self.primary.fetch_config().await;
        let exp_result = self.primary.fetch_active_experiments().await;

        match config_result {
            Ok(config_data) => {
                let mut cached = self.cached_config.write().await;
                *cached = Some(config_data);
                info!("Config fetched from primary data source");
            }
            Err(primary_err) => {
                warn!("Primary data source failed: {}", primary_err);
                // Try fallback
                if let Some(fallback) = &self.fallback {
                    match fallback.fetch_config().await {
                        Ok(config_data) => {
                            let mut cached = self.cached_config.write().await;
                            *cached = Some(config_data);
                            info!("Config fetched from fallback data source");
                        }
                        Err(fallback_err) => {
                            error!("Fallback data source also failed: {}", fallback_err);
                            return Err(SuperpositionError::ConfigError(format!(
                                "Both primary ({}) and fallback ({}) data sources failed",
                                primary_err, fallback_err
                            )));
                        }
                    }
                } else {
                    return Err(primary_err);
                }
            }
        }

        // Handle experiments (best-effort, only if primary supports them)
        match exp_result {
            Ok(Some(exp_data)) => {
                let mut cached = self.cached_experiments.write().await;
                *cached = Some(exp_data);
                info!("Experiments fetched from primary data source");
            }
            Ok(None) => {
                debug!("Primary data source does not support experiments");
            }
            Err(e) => {
                warn!("Failed to fetch experiments from primary: {}", e);
                // Try fallback for experiments
                if let Some(fallback) = &self.fallback {
                    if fallback.supports_experiments() {
                        match fallback.fetch_active_experiments().await {
                            Ok(Some(exp_data)) => {
                                let mut cached = self.cached_experiments.write().await;
                                *cached = Some(exp_data);
                                info!("Experiments fetched from fallback data source");
                            }
                            _ => {
                                warn!("Fallback also failed to fetch experiments");
                            }
                        }
                    }
                }
            }
        }

        // Start refresh strategy
        match &self.refresh_strategy {
            RefreshStrategy::Polling(strategy) => {
                info!("Starting polling with interval={}s", strategy.interval);
                let task = self.start_polling(strategy.interval).await;
                let mut polling_task = self.polling_task.write().await;
                *polling_task = Some(task);
            }
            RefreshStrategy::OnDemand(strategy) => {
                info!("Using on-demand strategy with ttl={}s", strategy.ttl);
            }
            RefreshStrategy::Manual => {
                info!("Using manual refresh strategy");
            }
        }

        let mut status = self.status.write().await;
        *status = ProviderStatus::Ready;

        info!("LocalResolutionProvider initialized successfully");
        Ok(())
    }

    /// Manually refresh data from the primary data source
    pub async fn refresh(&self) -> Result<()> {
        info!("Manual refresh triggered");
        self.do_refresh().await
    }

    /// Close the provider and clean up resources
    pub async fn close(&self) -> Result<()> {
        let mut polling_task = self.polling_task.write().await;
        if let Some(task) = polling_task.take() {
            task.abort();
        }

        self.primary.close().await?;
        if let Some(fallback) = &self.fallback {
            fallback.close().await?;
        }

        let mut cached_config = self.cached_config.write().await;
        *cached_config = None;
        let mut cached_experiments = self.cached_experiments.write().await;
        *cached_experiments = None;

        let mut status = self.status.write().await;
        *status = ProviderStatus::NotReady;

        Ok(())
    }

    async fn do_refresh(&self) -> Result<()> {
        // Fetch config - keep last known good on failure
        match self.primary.fetch_config().await {
            Ok(config_data) => {
                let mut cached = self.cached_config.write().await;
                *cached = Some(config_data);
                debug!("Config refreshed from primary");
            }
            Err(e) => {
                warn!("Refresh failed for config, keeping last known good: {}", e);
            }
        }

        // Fetch experiments - keep last known good on failure
        if self.primary.supports_experiments() {
            match self.primary.fetch_active_experiments().await {
                Ok(Some(exp_data)) => {
                    let mut cached = self.cached_experiments.write().await;
                    *cached = Some(exp_data);
                    debug!("Experiments refreshed from primary");
                }
                Ok(None) => {}
                Err(e) => {
                    warn!(
                        "Refresh failed for experiments, keeping last known good: {}",
                        e
                    );
                }
            }
        }

        Ok(())
    }

    async fn start_polling(&self, interval: u64) -> JoinHandle<()> {
        let primary = self.primary.clone();
        let cached_config = self.cached_config.clone();
        let cached_experiments = self.cached_experiments.clone();

        tokio::spawn(async move {
            loop {
                sleep(Duration::from_secs(interval)).await;

                match primary.fetch_config().await {
                    Ok(config_data) => {
                        let mut cached = cached_config.write().await;
                        *cached = Some(config_data);
                        debug!("Config updated via polling");
                    }
                    Err(e) => {
                        error!("Polling config refresh failed: {}", e);
                    }
                }

                if primary.supports_experiments() {
                    match primary.fetch_active_experiments().await {
                        Ok(Some(exp_data)) => {
                            let mut cached = cached_experiments.write().await;
                            *cached = Some(exp_data);
                            debug!("Experiments updated via polling");
                        }
                        Ok(None) => {}
                        Err(e) => {
                            error!("Polling experiments refresh failed: {}", e);
                        }
                    }
                }
            }
        })
    }

    async fn ensure_fresh_data(&self) -> Result<()> {
        if let RefreshStrategy::OnDemand(strategy) = &self.refresh_strategy {
            let should_refresh = {
                let cached = self.cached_config.read().await;
                match cached.as_ref() {
                    Some(data) => {
                        let elapsed = chrono::Utc::now() - data.fetched_at;
                        elapsed.num_seconds() > strategy.ttl as i64
                    }
                    None => true,
                }
            };

            if should_refresh {
                debug!("On-demand TTL expired, refreshing");
                if let Err(e) = self.do_refresh().await {
                    if !strategy.use_stale_on_error.unwrap_or(false) {
                        return Err(e);
                    }
                    warn!("On-demand refresh failed, using stale data: {}", e);
                }
            }
        }
        Ok(())
    }

    fn get_context_from_evaluation_context(
        evaluation_context: &EvaluationContext,
    ) -> (Map<String, Value>, Option<String>) {
        let context = evaluation_context
            .custom_fields
            .iter()
            .map(|(k, v)| {
                (
                    k.clone(),
                    ConversionUtils::convert_evaluation_context_value_to_serde_value(v),
                )
            })
            .collect();

        (context, evaluation_context.targeting_key.clone())
    }

    async fn get_dimensions_info(&self) -> HashMap<String, DimensionInfo> {
        let cached = self.cached_config.read().await;
        cached
            .as_ref()
            .map(|d| d.config.dimensions.clone())
            .unwrap_or_default()
    }

    async fn eval_with_context(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        self.ensure_fresh_data().await?;

        let (mut query_data, targeting_key) =
            Self::get_context_from_evaluation_context(context);

        let dimensions_info = self.get_dimensions_info().await;

        // Get experiment variants if available
        let cached_experiments = self.cached_experiments.read().await;
        if let Some(exp_data) = cached_experiments.as_ref() {
            let variant_ids = get_applicable_variants(
                &dimensions_info,
                exp_data.experiments.clone(),
                &exp_data.experiment_groups,
                &query_data,
                &targeting_key.unwrap_or_default(),
                prefix_filter.map(|p| p.to_vec()),
            )
            .map_err(|e| {
                SuperpositionError::ConfigError(format!(
                    "Failed to get applicable variants: {}",
                    e
                ))
            })?;

            query_data.insert(
                "variantIds".to_string(),
                Value::Array(variant_ids.into_iter().map(Value::String).collect()),
            );
        }
        drop(cached_experiments);

        // Evaluate config
        let cached_config = self.cached_config.read().await;
        match cached_config.as_ref() {
            Some(config_data) => eval_config(
                (*config_data.config.default_configs).clone(),
                &config_data.config.contexts,
                &config_data.config.overrides,
                &config_data.config.dimensions,
                &query_data,
                MergeStrategy::MERGE,
                prefix_filter.map(|p| p.to_vec()),
            )
            .map_err(|e| {
                SuperpositionError::ConfigError(format!("Failed to evaluate config: {}", e))
            }),
            None => Err(SuperpositionError::ConfigError(
                "No cached config available".into(),
            )),
        }
    }
}
```

**Step 2: Implement `AllFeatureProvider` for `LocalResolutionProvider`**

```rust
#[async_trait]
impl AllFeatureProvider for LocalResolutionProvider {
    async fn resolve_all_features(
        &self,
        context: &EvaluationContext,
    ) -> Result<Map<String, Value>> {
        self.eval_with_context(context, None).await
    }

    async fn resolve_all_features_with_filter(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        self.eval_with_context(context, prefix_filter).await
    }
}
```

**Step 3: Implement `FeatureExperimentMeta` for `LocalResolutionProvider`**

```rust
#[async_trait]
impl FeatureExperimentMeta for LocalResolutionProvider {
    async fn get_applicable_variants(
        &self,
        context: &EvaluationContext,
    ) -> Result<Vec<String>> {
        self.ensure_fresh_data().await?;

        let (query_data, targeting_key) =
            Self::get_context_from_evaluation_context(context);
        let dimensions_info = self.get_dimensions_info().await;

        let cached_experiments = self.cached_experiments.read().await;
        match cached_experiments.as_ref() {
            Some(exp_data) => get_applicable_variants(
                &dimensions_info,
                exp_data.experiments.clone(),
                &exp_data.experiment_groups,
                &query_data,
                &targeting_key.unwrap_or_default(),
                None,
            )
            .map_err(|e| {
                SuperpositionError::ConfigError(format!(
                    "Failed to get applicable variants: {}",
                    e
                ))
            }),
            None => Ok(vec![]),
        }
    }
}
```

**Step 4: Implement `FeatureProvider` (OpenFeature) for `LocalResolutionProvider`**

Follow the same pattern as `crates/superposition_provider/src/provider.rs`:

```rust
#[async_trait]
impl FeatureProvider for LocalResolutionProvider {
    async fn initialize(&mut self, _context: &EvaluationContext) {
        info!("Initializing LocalResolutionProvider via OpenFeature...");
        {
            let mut status = self.status.write().await;
            *status = ProviderStatus::NotReady;
        }
        if (self.init().await).is_err() {
            let mut status = self.status.write().await;
            *status = ProviderStatus::Error;
            return;
        }
        let mut status = self.status.write().await;
        *status = ProviderStatus::Ready;
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(bool_val) = value.as_bool() {
                        return Ok(ResolutionDetails::new(bool_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating boolean flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Error evaluating flag: {}", e)),
                })
            }
        }
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(str_val) = value.as_str() {
                        return Ok(ResolutionDetails::new(str_val.to_owned()));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating string flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Error evaluating flag: {}", e)),
                })
            }
        }
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(int_val) = value.as_i64() {
                        return Ok(ResolutionDetails::new(int_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating integer flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Error evaluating flag: {}", e)),
                })
            }
        }
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(float_val) = value.as_f64() {
                        return Ok(ResolutionDetails::new(float_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating float flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Error evaluating flag: {}", e)),
                })
            }
        }
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    match ConversionUtils::serde_value_to_struct_value(value) {
                        Ok(struct_value) => {
                            return Ok(ResolutionDetails::new(struct_value));
                        }
                        Err(e) => {
                            return Err(EvaluationError {
                                code: EvaluationErrorCode::ParseError,
                                message: Some(format!("Failed to parse struct value: {}", e)),
                            });
                        }
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
            Err(e) => {
                error!("Error evaluating struct flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Error evaluating flag: {}", e)),
                })
            }
        }
    }

    fn metadata(&self) -> &ProviderMetadata {
        &self.metadata
    }

    fn status(&self) -> ProviderStatus {
        match self.status.try_read() {
            Ok(status) => match *status {
                ProviderStatus::Ready => ProviderStatus::Ready,
                ProviderStatus::Error => ProviderStatus::Error,
                ProviderStatus::NotReady => ProviderStatus::NotReady,
                ProviderStatus::STALE => ProviderStatus::STALE,
            },
            Err(_) => ProviderStatus::NotReady,
        }
    }
}
```

**Step 5: Add module to `lib.rs`**

Add `pub mod local_provider;` and re-export: `pub use local_provider::LocalResolutionProvider;`

**Step 6: Verify it compiles**

Run: `cargo check -p superposition_provider`
Expected: compiles with no errors

**Step 7: Commit**

```bash
git add crates/superposition_provider/src/local_provider.rs crates/superposition_provider/src/lib.rs
git commit -m "feat: implement LocalResolutionProvider with primary/fallback and refresh strategies"
```

---

### Task 7: Implement `SuperpositionAPIProvider` (Remote Resolution)

**Files:**
- Create: `crates/superposition_provider/src/remote_provider.rs`
- Modify: `crates/superposition_provider/src/lib.rs`

**Step 1: Create `remote_provider.rs`**

Note: This provider calls a resolve API endpoint that accepts context and returns already-resolved config. The exact SDK method for remote resolution needs to be verified against the `superposition_sdk` crate. If a dedicated resolve endpoint exists, use it. Otherwise, this provider can use `get_config` + local eval as a fallback (similar to LocalResolutionProvider but without caching raw data).

```rust
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};

use async_trait::async_trait;
use log::{debug, error, info};
use open_feature::{
    provider::{FeatureProvider, ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationError, EvaluationErrorCode, EvaluationResult, StructValue,
};
use serde_json::{Map, Value};
use tokio::sync::RwLock;

use crate::traits::{AllFeatureProvider, FeatureExperimentMeta};
use crate::types::*;
use crate::utils::ConversionUtils;

struct CacheEntry {
    value: Map<String, Value>,
    created_at: Instant,
}

struct ResponseCache {
    entries: HashMap<String, CacheEntry>,
    max_entries: usize,
    ttl: Duration,
}

impl ResponseCache {
    fn new(max_entries: usize, ttl: Duration) -> Self {
        Self {
            entries: HashMap::new(),
            max_entries,
            ttl,
        }
    }

    fn get(&self, key: &str) -> Option<&Map<String, Value>> {
        self.entries.get(key).and_then(|entry| {
            if entry.created_at.elapsed() < self.ttl {
                Some(&entry.value)
            } else {
                None
            }
        })
    }

    fn put(&mut self, key: String, value: Map<String, Value>) {
        // Evict expired entries if at capacity
        if self.entries.len() >= self.max_entries {
            self.entries
                .retain(|_, entry| entry.created_at.elapsed() < self.ttl);
        }
        // If still at capacity, remove oldest
        if self.entries.len() >= self.max_entries {
            if let Some(oldest_key) = self
                .entries
                .iter()
                .min_by_key(|(_, entry)| entry.created_at)
                .map(|(k, _)| k.clone())
            {
                self.entries.remove(&oldest_key);
            }
        }
        self.entries.insert(
            key,
            CacheEntry {
                value,
                created_at: Instant::now(),
            },
        );
    }

    fn cache_key(context: &EvaluationContext) -> String {
        // Hash the context for cache key
        let mut parts: Vec<String> = Vec::new();
        if let Some(tk) = &context.targeting_key {
            parts.push(format!("tk:{}", tk));
        }
        let mut field_keys: Vec<&String> = context.custom_fields.keys().collect();
        field_keys.sort();
        for key in field_keys {
            let value = &context.custom_fields[key];
            parts.push(format!(
                "{}:{}",
                key,
                ConversionUtils::convert_evaluation_context_value_to_serde_value(value)
            ));
        }
        parts.join("|")
    }
}

/// Remote resolution provider using the Superposition API
///
/// Sends evaluation context over the network and receives resolved
/// configuration from the Superposition service.
pub struct SuperpositionAPIProvider {
    options: SuperpositionOptions,
    cache: Option<Arc<RwLock<ResponseCache>>>,
    metadata: ProviderMetadata,
    status: RwLock<ProviderStatus>,
}

impl SuperpositionAPIProvider {
    pub fn new(options: SuperpositionOptions) -> Self {
        Self {
            options,
            cache: None,
            metadata: ProviderMetadata {
                name: "SuperpositionAPIProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::Ready),
        }
    }

    pub fn with_cache(options: SuperpositionOptions, cache_options: CacheOptions) -> Self {
        let cache = ResponseCache::new(
            cache_options.size.unwrap_or(1000),
            Duration::from_secs(cache_options.ttl.unwrap_or(300)),
        );
        Self {
            options,
            cache: Some(Arc::new(RwLock::new(cache))),
            metadata: ProviderMetadata {
                name: "SuperpositionAPIProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::Ready),
        }
    }

    fn create_client(&self) -> superposition_sdk::Client {
        use superposition_sdk::{Client, Config as SdkConfig};

        let sdk_config = SdkConfig::builder()
            .endpoint_url(&self.options.endpoint)
            .bearer_token(self.options.token.clone().into())
            .behavior_version_latest()
            .build();

        Client::from_conf(sdk_config)
    }

    async fn resolve_remote(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        // Check cache first
        if let Some(cache) = &self.cache {
            let cache_key = ResponseCache::cache_key(context);
            let cache_read = cache.read().await;
            if let Some(cached) = cache_read.get(&cache_key) {
                debug!("Cache hit for context");
                let result = if let Some(prefixes) = prefix_filter {
                    cached
                        .iter()
                        .filter(|(k, _)| prefixes.iter().any(|p| k.starts_with(p)))
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect()
                } else {
                    cached.clone()
                };
                return Ok(result);
            }
            drop(cache_read);
        }

        let client = self.create_client();

        let (query_data, targeting_key) = Self::get_context_from_evaluation_context(context);

        // Use the SDK resolve API
        // Build query parameters from the context
        let mut request = client
            .get_resolved_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id);

        // Add targeting key if present
        if let Some(tk) = &targeting_key {
            request = request.targeting_key(tk.as_str());
        }

        // Add context fields as query parameters
        // The resolve API typically accepts context as a JSON body or query params
        // Using the SDK's built-in method for passing context
        for (key, value) in &query_data {
            request = request.context(key.clone(), value.to_string());
        }

        if let Some(prefixes) = prefix_filter {
            for prefix in prefixes {
                request = request.filter_keys(prefix.clone());
            }
        }

        let response = request.send().await.map_err(|e| {
            SuperpositionError::NetworkError(format!("Failed to resolve config: {}", e))
        })?;

        // Convert response to Map<String, Value>
        let config = ConversionUtils::convert_get_config_response(&response)?;

        // For remote resolution, the response should already be resolved
        // We return the default_configs as the resolved values
        let result: Map<String, Value> = (*config.default_configs).clone();

        // Cache the result
        if let Some(cache) = &self.cache {
            let cache_key = ResponseCache::cache_key(context);
            let mut cache_write = cache.write().await;
            cache_write.put(cache_key, result.clone());
        }

        Ok(result)
    }

    fn get_context_from_evaluation_context(
        evaluation_context: &EvaluationContext,
    ) -> (Map<String, Value>, Option<String>) {
        let context = evaluation_context
            .custom_fields
            .iter()
            .map(|(k, v)| {
                (
                    k.clone(),
                    ConversionUtils::convert_evaluation_context_value_to_serde_value(v),
                )
            })
            .collect();

        (context, evaluation_context.targeting_key.clone())
    }
}

#[async_trait]
impl AllFeatureProvider for SuperpositionAPIProvider {
    async fn resolve_all_features(
        &self,
        context: &EvaluationContext,
    ) -> Result<Map<String, Value>> {
        self.resolve_remote(context, None).await
    }

    async fn resolve_all_features_with_filter(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        self.resolve_remote(context, prefix_filter).await
    }
}

#[async_trait]
impl FeatureExperimentMeta for SuperpositionAPIProvider {
    async fn get_applicable_variants(
        &self,
        _context: &EvaluationContext,
    ) -> Result<Vec<String>> {
        // Remote resolution handles experiments server-side
        // The resolved config already includes experiment results
        // Return empty - variants are already applied in the resolved config
        Ok(vec![])
    }
}

#[async_trait]
impl FeatureProvider for SuperpositionAPIProvider {
    async fn initialize(&mut self, _context: &EvaluationContext) {
        let mut status = self.status.write().await;
        *status = ProviderStatus::Ready;
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(bool_val) = value.as_bool() {
                        return Ok(ResolutionDetails::new(bool_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found".to_string()),
                })
            }
            Err(e) => Err(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some(format!("Error: {}", e)),
            }),
        }
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(str_val) = value.as_str() {
                        return Ok(ResolutionDetails::new(str_val.to_owned()));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found".to_string()),
                })
            }
            Err(e) => Err(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some(format!("Error: {}", e)),
            }),
        }
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(int_val) = value.as_i64() {
                        return Ok(ResolutionDetails::new(int_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found".to_string()),
                })
            }
            Err(e) => Err(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some(format!("Error: {}", e)),
            }),
        }
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(float_val) = value.as_f64() {
                        return Ok(ResolutionDetails::new(float_val));
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found".to_string()),
                })
            }
            Err(e) => Err(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some(format!("Error: {}", e)),
            }),
        }
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    match ConversionUtils::serde_value_to_struct_value(value) {
                        Ok(struct_value) => {
                            return Ok(ResolutionDetails::new(struct_value));
                        }
                        Err(e) => {
                            return Err(EvaluationError {
                                code: EvaluationErrorCode::ParseError,
                                message: Some(format!("Failed to parse struct value: {}", e)),
                            });
                        }
                    }
                }
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found".to_string()),
                })
            }
            Err(e) => Err(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some(format!("Error: {}", e)),
            }),
        }
    }

    fn metadata(&self) -> &ProviderMetadata {
        &self.metadata
    }

    fn status(&self) -> ProviderStatus {
        match self.status.try_read() {
            Ok(status) => match *status {
                ProviderStatus::Ready => ProviderStatus::Ready,
                ProviderStatus::Error => ProviderStatus::Error,
                ProviderStatus::NotReady => ProviderStatus::NotReady,
                ProviderStatus::STALE => ProviderStatus::STALE,
            },
            Err(_) => ProviderStatus::NotReady,
        }
    }
}
```

**Step 2: Add module to `lib.rs`**

Add `pub mod remote_provider;` and re-export: `pub use remote_provider::SuperpositionAPIProvider;`

**Step 3: Verify it compiles**

Run: `cargo check -p superposition_provider`
Expected: compiles (the `get_resolved_config` SDK method may need adjustment based on actual SDK API - check `superposition_sdk` for the correct method name and parameters)

**Step 4: Commit**

```bash
git add crates/superposition_provider/src/remote_provider.rs crates/superposition_provider/src/lib.rs
git commit -m "feat: implement SuperpositionAPIProvider for remote resolution"
```

---

### Task 8: Update `lib.rs` with final re-exports

**Files:**
- Modify: `crates/superposition_provider/src/lib.rs`

**Step 1: Update `lib.rs` to export all new modules**

```rust
// Existing modules (kept for comparison)
pub mod client;
pub mod provider;
pub mod types;
pub mod utils;

// New trait-based architecture
pub mod data_source;
pub mod local_provider;
pub mod remote_provider;
pub mod traits;

// Re-exports - existing
pub use client::*;
pub use provider::*;
pub use types::*;

// Re-exports - new
pub use data_source::{ConfigData, ExperimentData, SuperpositionDataSource};
pub use local_provider::LocalResolutionProvider;
pub use remote_provider::SuperpositionAPIProvider;
pub use traits::*;

pub use open_feature::{
    provider::{ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext,
};
```

**Step 2: Verify it compiles**

Run: `cargo check -p superposition_provider`
Expected: compiles with no errors

**Step 3: Commit**

```bash
git add crates/superposition_provider/src/lib.rs
git commit -m "feat: update lib.rs with re-exports for new trait-based architecture"
```

---

### Task 9: Create examples

**Files:**
- Create: `crates/superposition_provider/examples/local_http_example.rs`
- Create: `crates/superposition_provider/examples/local_file_example.rs`
- Create: `crates/superposition_provider/examples/local_with_fallback_example.rs`

**Step 1: Create `local_http_example.rs`**

```rust
use open_feature::EvaluationContext;
use superposition_provider::{
    data_source::http::HttpDataSource,
    local_provider::LocalResolutionProvider,
    traits::{AllFeatureProvider, FeatureExperimentMeta},
    PollingStrategy, RefreshStrategy, SuperpositionOptions,
};

#[tokio::main]
async fn main() {
    env_logger::init();

    let http_source = HttpDataSource::new(SuperpositionOptions::new(
        "http://localhost:8080".to_string(),
        "token".to_string(),
        "org1".to_string(),
        "workspace1".to_string(),
    ));

    let provider = LocalResolutionProvider::new(
        Box::new(http_source),
        None,
        RefreshStrategy::Polling(PollingStrategy {
            interval: 30,
            timeout: Some(10),
        }),
    );
    provider.init().await.unwrap();

    let context = EvaluationContext::default()
        .with_targeting_key("user-123")
        .with_custom_field("os", "android");

    // AllFeatureProvider usage
    let all_config = provider.resolve_all_features(&context).await.unwrap();
    println!("All config: {:?}", all_config);

    // FeatureExperimentMeta usage
    let variants = provider.get_applicable_variants(&context).await.unwrap();
    println!("Variants: {:?}", variants);

    provider.close().await.unwrap();
}
```

**Step 2: Create `local_file_example.rs`**

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

    let file_source = FileDataSource::new(PathBuf::from("./config.toml"));

    let provider = LocalResolutionProvider::new(
        Box::new(file_source),
        None,
        RefreshStrategy::OnDemand(OnDemandStrategy {
            ttl: 60,
            ..Default::default()
        }),
    );
    provider.init().await.unwrap();

    let context = EvaluationContext::default()
        .with_custom_field("os", "linux");

    let config = provider.resolve_all_features(&context).await.unwrap();
    println!("Config: {:?}", config);

    provider.close().await.unwrap();
}
```

**Step 3: Create `local_with_fallback_example.rs`**

```rust
use std::path::PathBuf;

use open_feature::EvaluationContext;
use superposition_provider::{
    data_source::file::FileDataSource,
    data_source::http::HttpDataSource,
    local_provider::LocalResolutionProvider,
    traits::{AllFeatureProvider, FeatureExperimentMeta},
    PollingStrategy, RefreshStrategy, SuperpositionOptions,
};

#[tokio::main]
async fn main() {
    env_logger::init();

    // Primary: HTTP data source
    let http_source = HttpDataSource::new(SuperpositionOptions::new(
        "http://localhost:8080".to_string(),
        "token".to_string(),
        "org1".to_string(),
        "workspace1".to_string(),
    ));

    // Fallback: TOML file data source
    let file_source = FileDataSource::new(PathBuf::from("./config.toml"));

    let provider = LocalResolutionProvider::new(
        Box::new(http_source),
        Some(Box::new(file_source)),
        RefreshStrategy::Polling(PollingStrategy {
            interval: 30,
            timeout: Some(10),
        }),
    );
    provider.init().await.unwrap();

    let context = EvaluationContext::default()
        .with_targeting_key("user-456")
        .with_custom_field("os", "android")
        .with_custom_field("app_version", "3.2.1");

    // Bulk resolution
    let all_config = provider.resolve_all_features(&context).await.unwrap();
    println!("All config: {:?}", all_config);

    // Filtered resolution
    let filtered = provider
        .resolve_all_features_with_filter(
            &context,
            Some(&["payment.".to_string(), "ui.".to_string()]),
        )
        .await
        .unwrap();
    println!("Filtered config: {:?}", filtered);

    // Experiment variants
    let variants = provider.get_applicable_variants(&context).await.unwrap();
    println!("Variants: {:?}", variants);

    // Manual refresh
    provider.refresh().await.unwrap();

    provider.close().await.unwrap();
}
```

**Step 4: Add `env_logger` dev-dependency to `Cargo.toml`**

Add to `[dev-dependencies]` in `crates/superposition_provider/Cargo.toml`:
```toml
[dev-dependencies]
env_logger = "0.11"
```

**Step 5: Verify examples compile**

Run: `cargo check -p superposition_provider --examples`
Expected: compiles with no errors

**Step 6: Commit**

```bash
git add crates/superposition_provider/examples/ crates/superposition_provider/Cargo.toml
git commit -m "feat: add examples for LocalResolutionProvider with HTTP, File, and fallback"
```

---

### Task 10: Final verification

**Step 1: Run full crate check**

Run: `cargo check -p superposition_provider`
Expected: compiles with no errors and no warnings

**Step 2: Run existing tests**

Run: `cargo test -p superposition_provider -- --skip test_rust_provider_integration`
Expected: existing unit tests pass

**Step 3: Check examples compile**

Run: `cargo check -p superposition_provider --examples`
Expected: compiles

**Step 4: Run workspace check to ensure no regressions**

Run: `cargo check --workspace`
Expected: no compilation errors in any crate

**Step 5: Commit any final fixes**

```bash
git add -A
git commit -m "fix: final adjustments for configuration resolver implementation"
```
