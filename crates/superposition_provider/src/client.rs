use std::collections::HashMap;
use std::sync::Arc;

use log::{debug, error, info, warn};
use serde_json::Value;
use superposition_core::experiment::ExperimentGroups;
use superposition_core::{
    eval_config, get_applicable_variants, Experiments, MergeStrategy,
};
use superposition_types::{Config, DimensionInfo};
use tokio::join;
use tokio::sync::RwLock;
use tokio::task::JoinHandle;
use tokio::time::{sleep, Duration};

use crate::types::*;
use crate::utils::ConversionUtils;

pub use open_feature::{
    provider::{ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext,
};

#[derive(Debug)]
pub struct CacConfig {
    superposition_options: SuperpositionOptions,
    options: ConfigurationOptions,
    fallback_config: Option<serde_json::Map<String, Value>>,
    cached_config: Arc<RwLock<Option<Config>>>,
    last_updated: Arc<RwLock<Option<chrono::DateTime<chrono::Utc>>>>,
    evaluation_cache: RwLock<HashMap<String, HashMap<String, Value>>>,
    polling_task: RwLock<Option<JoinHandle<()>>>,
}

impl CacConfig {
    pub fn new(
        superposition_options: SuperpositionOptions,
        options: ConfigurationOptions,
    ) -> Self {
        Self {
            superposition_options,
            fallback_config: options.fallback_config.clone(),
            options,
            cached_config: Arc::new(RwLock::new(None)),
            last_updated: Arc::new(RwLock::new(None)),
            evaluation_cache: RwLock::new(HashMap::new()),
            polling_task: RwLock::new(None),
        }
    }

    pub async fn create_config(&self) -> Result<()> {
        info!("Creating CAC configuration...");

        // Fetch initial config
        let latest_config = self.get_config(&self.superposition_options).await;
        match latest_config {
            Ok(config) => {
                let mut cached_config = self.cached_config.write().await;
                *cached_config = Some(config);
                let mut last_updated = self.last_updated.write().await;
                *last_updated = Some(chrono::Utc::now());
                info!("CAC config fetched successfully");
            }
            Err(e) => {
                let mut cached_config = self.cached_config.write().await;
                if cached_config.is_none() {
                    // If no cached config, use fallback if available
                    if let Some(fallback) = &self.fallback_config {
                        *cached_config =
                            Some(ConversionUtils::convert_value_to_config(fallback)?);
                        info!("Using fallback config due to initial fetch failure");
                    }
                } else {
                    error!("Failed to fetch initial config: {}", e);
                    return Err(e);
                }
            }
        }

        // Start refresh strategy
        match &self.options.refresh_strategy {
            RefreshStrategy::Polling(polling_strategy) => {
                info!(
                    "Using PollingStrategy: interval={}s, timeout={}s",
                    polling_strategy.interval,
                    polling_strategy.timeout.unwrap_or(30)
                );
                let task = self.start_polling(polling_strategy.interval).await;
                let mut polling_task = self.polling_task.write().await;
                *polling_task = Some(task);
            }
            RefreshStrategy::OnDemand(on_demand_strategy) => {
                info!(
                    "Using OnDemandStrategy: ttl={}s, use_stale_on_error={}, timeout={}s",
                    on_demand_strategy.ttl,
                    on_demand_strategy.use_stale_on_error.unwrap_or(false),
                    on_demand_strategy.timeout.unwrap_or(30)
                );
            }
            RefreshStrategy::Manual => {
                info!("Using Manual refresh strategy");
            }
        }

        Ok(())
    }

    async fn start_polling(&self, interval: u64) -> JoinHandle<()> {
        let superposition_options = self.superposition_options.clone();
        let cached_config = self.cached_config.clone();
        let last_updated = self.last_updated.clone();

        tokio::spawn(async move {
            loop {
                match Self::get_config_static(&superposition_options).await {
                    Ok(config) => {
                        let mut cached = cached_config.write().await;
                        *cached = Some(config);
                        let mut updated = last_updated.write().await;
                        *updated = Some(chrono::Utc::now());
                        debug!("CAC config updated via polling");
                    }
                    Err(e) => {
                        error!("Polling error: {}", e);
                    }
                }
                sleep(Duration::from_secs(interval)).await;
            }
        })
    }

    pub async fn on_demand_config(&self, ttl: u64, use_stale: bool) -> Result<Config> {
        let now = chrono::Utc::now();
        let last_updated;
        {
            last_updated = self.last_updated.read().await;
        }
        let should_refresh = match *last_updated {
            Some(last) => (now - last).num_seconds() > ttl as i64,
            None => true,
        };

        if should_refresh {
            debug!("TTL expired. Fetching config on-demand");
            match self.get_config(&self.superposition_options).await {
                Ok(config) => {
                    let mut cached_config = self.cached_config.write().await;
                    *cached_config = Some(config.clone());
                    let mut last_updated_mut = self.last_updated.write().await;
                    *last_updated_mut = Some(chrono::Utc::now());
                    info!("Config fetched successfully on-demand");
                    return Ok(config);
                }
                Err(e) => {
                    warn!("On-demand fetch failed: {}", e);
                    if !use_stale {
                        return Err(e);
                    }
                    info!("Using stale config due to error");
                }
            }
        }

        // Return cached config
        let cached_config = self.cached_config.read().await;
        match cached_config.as_ref() {
            Some(config) => Ok(config.clone()),
            None => Err(SuperpositionError::ConfigError(
                "No cached config available".into(),
            )),
        }
    }

    async fn get_config(&self, options: &SuperpositionOptions) -> Result<Config> {
        Self::get_config_static(options).await
    }

    async fn get_config_static(options: &SuperpositionOptions) -> Result<Config> {
        use superposition_sdk::{Client, Config as SdkConfig};

        info!("Fetching config from Superposition service using SDK");

        // Create SDK config
        let sdk_config = SdkConfig::builder()
            .endpoint_url(&options.endpoint)
            .bearer_token(options.token.clone().into())
            .behavior_version_latest()
            .build();

        // Create Superposition client
        let client = Client::from_conf(sdk_config);

        // Call the get_config API
        let response = client
            .get_config()
            .workspace_id(&options.workspace_id)
            .org_id(&options.org_id)
            .send()
            .await
            .map_err(|e| {
                let error = format!("Failed to get config: {}", e);
                SuperpositionError::NetworkError(error)
            })?;

        // Use ConversionUtils to convert to proper Config type
        let config = ConversionUtils::convert_get_config_response(&response)?;

        info!("Successfully fetched and converted config with {} contexts, {} overrides, {} default configs",
              config.contexts.len(), config.overrides.len(), config.default_configs.len());

        Ok(config)
    }

    pub async fn get_cached_config(&self) -> Option<Config> {
        let cached_config = self.cached_config.read().await;
        cached_config.clone()
    }

    /// Evaluate configuration for given context and return resolved values
    pub async fn evaluate_config(
        &self,
        query_data: &serde_json::Map<String, Value>,
        prefix_filter: Option<&[String]>,
    ) -> Result<serde_json::Map<String, Value>> {
        let cached_config = self.cached_config.read().await;
        match cached_config.as_ref() {
            Some(cached_config) => {
                // Use ConversionUtils to evaluate config
                eval_config(
                    (*cached_config.default_configs).clone(),
                    &cached_config.contexts,
                    &cached_config.overrides,
                    &cached_config.dimensions,
                    query_data,
                    MergeStrategy::MERGE,
                    prefix_filter.map(|p| p.to_vec()),
                )
                .map_err(|e| {
                    SuperpositionError::ConfigError(format!(
                        "Failed to evaluate config: {}",
                        e
                    ))
                })
            }
            None => Err(SuperpositionError::ConfigError(
                "No cached config available".into(),
            )),
        }
    }

    pub async fn close(&self) -> Result<()> {
        // Stop polling task
        let mut polling_task = self.polling_task.write().await;
        if let Some(task) = polling_task.take() {
            task.abort();
        }

        // Clear caches
        let mut cached_config = self.cached_config.write().await;
        *cached_config = None;
        let mut evaluation_cache = self.evaluation_cache.write().await;
        evaluation_cache.clear();

        Ok(())
    }
}

/// Experimentation Configuration client
#[derive(Debug)]
pub struct ExperimentationConfig {
    superposition_options: SuperpositionOptions,
    options: ExperimentationOptions,
    cached_experiments: Arc<RwLock<Option<Experiments>>>,
    cached_experiment_groups: Arc<RwLock<Option<ExperimentGroups>>>,
    last_updated: Arc<RwLock<Option<chrono::DateTime<chrono::Utc>>>>,
    evaluation_cache: RwLock<HashMap<String, HashMap<String, Value>>>,
    polling_task: RwLock<Option<JoinHandle<()>>>,
}

impl ExperimentationConfig {
    pub fn new(
        superposition_options: SuperpositionOptions,
        options: ExperimentationOptions,
    ) -> Self {
        Self {
            superposition_options,
            options,
            cached_experiments: Arc::new(RwLock::new(None)),
            cached_experiment_groups: Arc::new(RwLock::new(None)),
            last_updated: Arc::new(RwLock::new(None)),
            evaluation_cache: RwLock::new(HashMap::new()),
            polling_task: RwLock::new(None),
        }
    }

    pub async fn create_config(&self) -> Result<()> {
        info!("Creating Experimentation configuration...");

        // Fetch initial experiments and experiment groups
        let (latest_experiments, latest_experiment_groups) = join!(
            self.get_experiments(&self.superposition_options),
            self.get_experiment_groups(&self.superposition_options)
        );
        match (latest_experiments, latest_experiment_groups) {
            (Ok(Some(experiments)), Ok(Some(experiment_groups))) => {
                let mut cached_experiments = self.cached_experiments.write().await;
                *cached_experiments = Some(experiments);
                let mut cached_experiment_groups =
                    self.cached_experiment_groups.write().await;
                *cached_experiment_groups = Some(experiment_groups);
                let mut last_updated = self.last_updated.write().await;
                *last_updated = Some(chrono::Utc::now());
                info!("Experiments fetched successfully");
            }
            (Ok(None), Ok(None)) => {
                warn!("No experiments or experiment groups returned from initial fetch");
            }
            (Err(e), _) | (_, Err(e)) => {
                error!(
                    "Failed to fetch initial experiments or experiment groups: {}",
                    e
                );
                return Err(e);
            }
            (_, _) => {
                error!("Failed to fetch either experiments or experiment groups");
                return Err(SuperpositionError::ConfigError(
                    "Failed to fetch either experiments or experiment groups".into(),
                ));
            }
        }

        // Start refresh strategy
        match &self.options.refresh_strategy {
            RefreshStrategy::Polling(polling_strategy) => {
                info!(
                    "Using PollingStrategy for experiments: interval={}s",
                    polling_strategy.interval
                );
                let task = self.start_polling(polling_strategy.interval).await;
                let mut polling_task = self.polling_task.write().await;
                *polling_task = Some(task);
            }
            RefreshStrategy::OnDemand(on_demand_strategy) => {
                info!(
                    "Using OnDemandStrategy for experiments: ttl={}s",
                    on_demand_strategy.ttl
                );
            }
            RefreshStrategy::Manual => {
                info!("Using Manual refresh strategy for experiments");
            }
        }

        Ok(())
    }

    async fn start_polling(&self, interval: u64) -> JoinHandle<()> {
        let superposition_options = self.superposition_options.clone();
        let cached_experiments = self.cached_experiments.clone();
        let cached_experiment_groups = self.cached_experiment_groups.clone();
        let last_updated = self.last_updated.clone();

        tokio::spawn(async move {
            loop {
                let (experiments_result, groups_result) = join!(
                    Self::get_experiments_static(&superposition_options),
                    Self::get_experiment_groups_static(&superposition_options)
                );
                match (experiments_result, groups_result) {
                    (Ok(Some(experiments)), Ok(Some(experiment_groups))) => {
                        let mut cached = cached_experiments.write().await;
                        *cached = Some(experiments);
                        let mut cached_groups = cached_experiment_groups.write().await;
                        *cached_groups = Some(experiment_groups);
                        let mut updated = last_updated.write().await;
                        *updated = Some(chrono::Utc::now());
                        debug!("Experiments and Experiment Groups updated via polling");
                    }
                    (Ok(None), Ok(None)) => {
                        warn!(
                            "No experiments or experiment groups returned from polling"
                        );
                    }
                    (Err(e), _) | (_, Err(e)) => {
                        error!("Polling error: {}", e);
                    }
                    _ => {}
                }
                sleep(Duration::from_secs(interval)).await;
            }
        })
    }

    pub async fn on_demand_config(
        &self,
        ttl: u64,
        use_stale: bool,
    ) -> Result<Experiments> {
        let now = chrono::Utc::now();
        let last_updated = self.last_updated.read().await;

        let should_refresh = match *last_updated {
            Some(last) => (now - last).num_seconds() > ttl as i64,
            None => true,
        };

        if should_refresh {
            debug!("TTL expired. Fetching experiments and experiment groups on-demand");
            let (experiments_result, groups_result) = join!(
                self.get_experiments(&self.superposition_options),
                self.get_experiment_groups(&self.superposition_options)
            );
            match (experiments_result, groups_result) {
                (Ok(Some(experiments)), Ok(Some(experiment_groups))) => {
                    let mut cached_experiments = self.cached_experiments.write().await;
                    *cached_experiments = Some(experiments.clone());
                    let mut cached_experiment_groups =
                        self.cached_experiment_groups.write().await;
                    *cached_experiment_groups = Some(experiment_groups);
                    let mut last_updated_mut = self.last_updated.write().await;
                    *last_updated_mut = Some(chrono::Utc::now());
                    info!("Experiments and Experiment Groups fetched successfully on-demand");
                    return Ok(experiments);
                }
                (Err(e), _) | (_, Err(e)) => {
                    warn!(
                        "On-demand experiments and experiment groups fetch failed: {}",
                        e
                    );
                    if !use_stale {
                        return Err(e);
                    }
                    info!("Using stale experiments and experiment groups due to error");
                }
                _ => {}
            }
        }

        // Return cached experiments
        let cached_experiments = self.cached_experiments.read().await;
        match cached_experiments.as_ref() {
            Some(experiments) => Ok(experiments.clone()),
            None => Ok(vec![]), // Return empty if no experiments cached
        }
    }

    async fn get_experiments(
        &self,
        options: &SuperpositionOptions,
    ) -> Result<Option<Experiments>> {
        Self::get_experiments_static(options).await
    }

    async fn get_experiment_groups(
        &self,
        options: &SuperpositionOptions,
    ) -> Result<Option<ExperimentGroups>> {
        Self::get_experiment_groups_static(options).await
    }

    async fn get_experiments_static(
        options: &SuperpositionOptions,
    ) -> Result<Option<Experiments>> {
        use superposition_sdk::{
            types::ExperimentStatusType, Client, Config as SdkConfig,
        };

        info!("Fetching experiments from Superposition service using SDK");

        // Create SDK config
        let sdk_config = SdkConfig::builder()
            .endpoint_url(&options.endpoint)
            .bearer_token(options.token.clone().into())
            .behavior_version_latest()
            .build();

        // Create Superposition client
        let client = Client::from_conf(sdk_config);

        let response = client
            .list_experiment()
            .workspace_id(&options.workspace_id)
            .org_id(&options.org_id)
            .all(true)
            .status(ExperimentStatusType::Created)
            .status(ExperimentStatusType::Inprogress)
            .send()
            .await
            .map_err(|e| {
                SuperpositionError::NetworkError(format!(
                    "Failed to list experiments: {}",
                    e
                ))
            })?;

        let experiments = ConversionUtils::convert_experiments_response(&response)?;

        info!(
            "Successfully fetched and converted {} experiments",
            experiments.len()
        );
        Ok(Some(experiments))
    }

    async fn get_experiment_groups_static(
        options: &SuperpositionOptions,
    ) -> Result<Option<ExperimentGroups>> {
        use superposition_sdk::{Client, Config as SdkConfig};

        info!("Fetching experiment groups from Superposition service using SDK");

        // Create SDK config
        let sdk_config = SdkConfig::builder()
            .endpoint_url(&options.endpoint)
            .bearer_token(options.token.clone().into())
            .behavior_version_latest()
            .build();

        // Create Superposition client
        let client = Client::from_conf(sdk_config);

        let response = client
            .list_experiment_groups()
            .workspace_id(&options.workspace_id)
            .org_id(&options.org_id)
            .all(true)
            .send()
            .await
            .map_err(|e| {
                SuperpositionError::NetworkError(format!(
                    "Failed to list experiment groups: {}",
                    e
                ))
            })?;

        let experiment_groups =
            ConversionUtils::convert_experiment_groups_response(&response)?;

        info!(
            "Successfully fetched and converted {} experiment groups",
            experiment_groups.len()
        );
        Ok(Some(experiment_groups))
    }

    pub async fn get_cached_experiments(&self) -> Option<Experiments> {
        let cached_experiments = self.cached_experiments.read().await;
        cached_experiments.clone()
    }

    pub async fn get_cached_experiment_groups(&self) -> Option<ExperimentGroups> {
        let cached_experiment_groups = self.cached_experiment_groups.read().await;
        cached_experiment_groups.clone()
    }

    pub async fn close(&self) -> Result<()> {
        // Stop polling task
        let mut polling_task = self.polling_task.write().await;
        if let Some(task) = polling_task.take() {
            task.abort();
        }

        // Clear caches
        let mut cached_experiments = self.cached_experiments.write().await;
        *cached_experiments = None;
        let mut evaluation_cache = self.evaluation_cache.write().await;
        evaluation_cache.clear();

        Ok(())
    }

    pub async fn get_applicable_variants(
        &self,
        dimensions_info: &HashMap<String, DimensionInfo>,
        contexts: &serde_json::Map<String, Value>,
        identifier: Option<String>,
    ) -> Result<Vec<String>> {
        let cached_experiments = self.cached_experiments.read().await;
        let cached_experiment_groups = self.cached_experiment_groups.read().await;

        match (
            cached_experiments.as_ref(),
            cached_experiment_groups.as_ref(),
        ) {
            (Some(experiments), Some(experiment_groups)) => {
                // Use get_applicable_variants from superposition_core
                get_applicable_variants(
                    dimensions_info,
                    experiments.clone(),
                    experiment_groups,
                    contexts,
                    &identifier.unwrap_or_default(),
                    None,
                )
                .map_err(|e| {
                    SuperpositionError::ConfigError(format!(
                        "Failed to get applicable variants: {}",
                        e
                    ))
                })
            }
            _ => Err(SuperpositionError::ConfigError(
                "No cached experiments or experiment groups available".into(),
            )),
        }
    }
}
