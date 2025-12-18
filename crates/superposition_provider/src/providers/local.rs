use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use log::{debug, error, info, warn};
use open_feature::{
    provider::{FeatureProvider, ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationError, EvaluationErrorCode, EvaluationResult, StructValue,
};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_core::config::MergeStrategy;
use superposition_core::experiment::{ExperimentGroups, Experiments};
use superposition_types::{Config, DimensionInfo};
use tokio::sync::RwLock;
use tokio::task::JoinHandle;
use tokio::time::sleep;

use crate::data_source::SuperpositionDataSource;
use crate::traits::{
    AllFeatureProvider, AllFeatureProviderMetadata, ExperimentMeta, FeatureExperimentMeta,
};
use crate::types::{RefreshStrategy, Result, SuperpositionError};
use crate::utils::ConversionUtils;

/// Options for configuring the LocalResolutionProvider
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LocalResolutionProviderOptions {
    /// Strategy for refreshing configuration data
    pub refresh_strategy: RefreshStrategy,
    /// Fallback configuration to use when data source is unavailable
    pub fallback_config: Option<Map<String, Value>>,
    /// Enable experiment support
    pub enable_experiments: bool,
}

impl Default for LocalResolutionProviderOptions {
    fn default() -> Self {
        Self {
            refresh_strategy: RefreshStrategy::OnDemand(crate::types::OnDemandStrategy {
                ttl: 60,
                timeout: None,
                use_stale_on_error: None,
            }),
            fallback_config: None,
            enable_experiments: true,
        }
    }
}

/// Provider that performs configuration resolution locally using superposition_core
///
/// This provider fetches configuration from a data source (HTTP, File, etc.)
/// and performs resolution locally using the superposition_core evaluation engine.
pub struct LocalResolutionProvider {
    metadata: AllFeatureProviderMetadata,
    of_metadata: ProviderMetadata,
    status: RwLock<ProviderStatus>,
    data_source: Arc<dyn SuperpositionDataSource>,
    options: LocalResolutionProviderOptions,
    cached_config: Arc<RwLock<Option<Config>>>,
    cached_experiments: Arc<RwLock<Option<Experiments>>>,
    cached_experiment_groups: Arc<RwLock<Option<ExperimentGroups>>>,
    last_config_update: Arc<RwLock<Option<DateTime<Utc>>>>,
    last_experiments_update: Arc<RwLock<Option<DateTime<Utc>>>>,
    polling_task: RwLock<Option<JoinHandle<()>>>,
}

impl LocalResolutionProvider {
    /// Create a new LocalResolutionProvider
    ///
    /// # Arguments
    ///
    /// * `data_source` - The data source to fetch configuration from
    /// * `options` - Provider options
    pub fn new(
        data_source: Arc<dyn SuperpositionDataSource>,
        options: LocalResolutionProviderOptions,
    ) -> Self {
        info!(
            "Creating LocalResolutionProvider with data source: {}",
            data_source.source_name()
        );

        Self {
            metadata: AllFeatureProviderMetadata::new(
                "LocalResolutionProvider".to_string(),
                env!("CARGO_PKG_VERSION").to_string(),
            ),
            of_metadata: ProviderMetadata {
                name: "LocalResolutionProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
            data_source,
            options,
            cached_config: Arc::new(RwLock::new(None)),
            cached_experiments: Arc::new(RwLock::new(None)),
            cached_experiment_groups: Arc::new(RwLock::new(None)),
            last_config_update: Arc::new(RwLock::new(None)),
            last_experiments_update: Arc::new(RwLock::new(None)),
            polling_task: RwLock::new(None),
        }
    }

    /// Initialize the provider and start background tasks if needed
    pub async fn init(&self) -> Result<()> {
        info!("Initializing LocalResolutionProvider");

        // Initial fetch
        self.refresh_config().await?;
        if self.options.enable_experiments && self.data_source.supports_experiments() {
            self.refresh_experiments().await?;
        }

        // Start polling task if using polling strategy
        if let RefreshStrategy::Polling(polling_strategy) = &self.options.refresh_strategy {
            let interval_secs = polling_strategy.interval;
            let data_source = self.data_source.clone();
            let cached_config = self.cached_config.clone();
            let cached_experiments = self.cached_experiments.clone();
            let cached_experiment_groups = self.cached_experiment_groups.clone();
            let last_config_update = self.last_config_update.clone();
            let last_experiments_update = self.last_experiments_update.clone();
            let enable_experiments = self.options.enable_experiments;
            let supports_experiments = self.data_source.supports_experiments();

            let handle = tokio::spawn(async move {
                info!(
                    "Starting polling task with interval: {} seconds",
                    interval_secs
                );
                loop {
                    sleep(Duration::from_secs(interval_secs)).await;

                    // Fetch config
                    match data_source.fetch_config().await {
                        Ok(config_data) => {
                            let mut cache = cached_config.write().await;
                            *cache = Some(config_data.config);
                            let mut last_update = last_config_update.write().await;
                            *last_update = Some(config_data.fetched_at);
                            debug!("Polling: Config updated successfully");
                        }
                        Err(e) => {
                            error!("Polling: Failed to fetch config: {}", e);
                        }
                    }

                    // Fetch experiments if enabled
                    if enable_experiments && supports_experiments {
                        match data_source.fetch_experiments().await {
                            Ok(Some(exp_data)) => {
                                let mut exp_cache = cached_experiments.write().await;
                                *exp_cache = Some(exp_data.experiments);
                                let mut group_cache = cached_experiment_groups.write().await;
                                *group_cache = Some(exp_data.experiment_groups);
                                let mut last_update = last_experiments_update.write().await;
                                *last_update = Some(exp_data.fetched_at);
                                debug!("Polling: Experiments updated successfully");
                            }
                            Ok(None) => {
                                debug!("Polling: No experiments available");
                            }
                            Err(e) => {
                                error!("Polling: Failed to fetch experiments: {}", e);
                            }
                        }
                    }
                }
            });

            let mut task = self.polling_task.write().await;
            *task = Some(handle);
        }

        // Update status
        let mut status = self.status.write().await;
        *status = ProviderStatus::Ready;

        info!("LocalResolutionProvider initialized successfully");
        Ok(())
    }

    /// Refresh configuration from data source
    async fn refresh_config(&self) -> Result<()> {
        debug!("Refreshing config from data source");
        let config_data = self.data_source.fetch_config().await?;

        let mut cache = self.cached_config.write().await;
        *cache = Some(config_data.config);
        let mut last_update = self.last_config_update.write().await;
        *last_update = Some(config_data.fetched_at);

        Ok(())
    }

    /// Refresh experiments from data source
    async fn refresh_experiments(&self) -> Result<()> {
        debug!("Refreshing experiments from data source");

        match self.data_source.fetch_experiments().await? {
            Some(exp_data) => {
                let mut exp_cache = self.cached_experiments.write().await;
                *exp_cache = Some(exp_data.experiments);
                let mut group_cache = self.cached_experiment_groups.write().await;
                *group_cache = Some(exp_data.experiment_groups);
                let mut last_update = self.last_experiments_update.write().await;
                *last_update = Some(exp_data.fetched_at);
                Ok(())
            }
            None => {
                debug!("No experiments available from data source");
                Ok(())
            }
        }
    }

    /// Check if config needs refresh based on TTL
    async fn should_refresh_config(&self) -> bool {
        if let RefreshStrategy::OnDemand(strategy) = &self.options.refresh_strategy {
            let last_update = self.last_config_update.read().await;
            match *last_update {
                Some(last) => {
                    let elapsed = Utc::now() - last;
                    elapsed.num_seconds() > strategy.ttl as i64
                }
                None => true,
            }
        } else {
            false
        }
    }

    /// Check if experiments need refresh based on TTL
    async fn should_refresh_experiments(&self) -> bool {
        if let RefreshStrategy::OnDemand(strategy) = &self.options.refresh_strategy {
            let last_update = self.last_experiments_update.read().await;
            match *last_update {
                Some(last) => {
                    let elapsed = Utc::now() - last;
                    elapsed.num_seconds() > strategy.ttl as i64
                }
                None => true,
            }
        } else {
            false
        }
    }

    /// Ensure config is up to date
    async fn ensure_config(&self) -> Result<()> {
        if self.should_refresh_config().await {
            self.refresh_config().await?;
        }
        Ok(())
    }

    /// Ensure experiments are up to date
    async fn ensure_experiments(&self) -> Result<()> {
        if self.options.enable_experiments
            && self.data_source.supports_experiments()
            && self.should_refresh_experiments().await
        {
            self.refresh_experiments().await?;
        }
        Ok(())
    }

    /// Get cached config or fallback
    async fn get_config(&self) -> Result<Config> {
        let cache = self.cached_config.read().await;
        match cache.as_ref() {
            Some(config) => Ok(config.clone()),
            None => {
                if let Some(fallback) = &self.options.fallback_config {
                    warn!("Using fallback config");
                    Ok(Config {
                        contexts: vec![],
                        overrides: HashMap::new(),
                        default_configs: fallback.clone(),
                        dimensions: HashMap::new(),
                    })
                } else {
                    Err(SuperpositionError::ConfigError(
                        "No config available and no fallback configured".to_string(),
                    ))
                }
            }
        }
    }

    /// Get dimensions info
    async fn get_dimensions_info(&self) -> HashMap<String, DimensionInfo> {
        let cache = self.cached_config.read().await;
        cache
            .as_ref()
            .map(|c| c.dimensions.clone())
            .unwrap_or_default()
    }

    /// Convert EvaluationContext to query data map
    fn get_context_from_evaluation_context(
        &self,
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

    /// Resolve configuration for given context
    async fn resolve_config(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        // Ensure config is up to date
        self.ensure_config().await?;

        // Get config
        let config = self.get_config().await?;

        // Convert evaluation context to query data
        let (mut query_data, targeting_key) = self.get_context_from_evaluation_context(context);

        // Get applicable variants if experiments are enabled
        if self.options.enable_experiments && self.data_source.supports_experiments() {
            self.ensure_experiments().await?;

            let experiments = self.cached_experiments.read().await;
            let experiment_groups = self.cached_experiment_groups.read().await;

            if let (Some(exps), Some(groups)) = (experiments.as_ref(), experiment_groups.as_ref()) {
                let identifier = targeting_key.as_deref().unwrap_or("");
                let dimensions = self.get_dimensions_info().await;

                match superposition_core::experiment::get_applicable_variants(
                    &dimensions,
                    exps,
                    groups,
                    &query_data,
                    identifier,
                    prefix_filter.map(|p| p.to_vec()),
                ) {
                    Ok(variant_ids) => {
                        if !variant_ids.is_empty() {
                            debug!("Injecting variant IDs: {:?}", variant_ids);
                            query_data.insert(
                                "variantIds".to_string(),
                                Value::Array(
                                    variant_ids.into_iter().map(Value::String).collect(),
                                ),
                            );
                        }
                    }
                    Err(e) => {
                        error!("Failed to get applicable variants: {}", e);
                    }
                }
            }
        }

        // Evaluate config
        let resolved_config = superposition_core::config::eval_config(
            config.default_configs.clone(),
            &config.contexts,
            &config.overrides,
            &config.dimensions,
            &query_data,
            MergeStrategy::MERGE,
            prefix_filter.map(|p| p.to_vec()),
        )
        .map_err(|e| SuperpositionError::ConfigError(format!("Failed to evaluate config: {}", e)))?;

        Ok(resolved_config)
    }

    /// Shutdown the provider and cleanup resources
    pub async fn shutdown(&self) -> Result<()> {
        info!("Shutting down LocalResolutionProvider");

        // Stop polling task if running
        let mut task = self.polling_task.write().await;
        if let Some(handle) = task.take() {
            handle.abort();
            debug!("Polling task stopped");
        }

        // Close data source
        self.data_source.close().await?;

        // Update status
        let mut status = self.status.write().await;
        *status = ProviderStatus::NotReady;

        info!("LocalResolutionProvider shutdown complete");
        Ok(())
    }
}

#[async_trait]
impl AllFeatureProvider for LocalResolutionProvider {
    async fn resolve_all_features(
        &self,
        context: &EvaluationContext,
    ) -> Result<Map<String, Value>> {
        debug!("Resolving all features");
        self.resolve_config(context, None).await
    }

    async fn resolve_all_features_with_filter(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        debug!("Resolving features with filter: {:?}", prefix_filter);
        self.resolve_config(context, prefix_filter).await
    }

    fn metadata(&self) -> &AllFeatureProviderMetadata {
        &self.metadata
    }
}

#[async_trait]
impl FeatureExperimentMeta for LocalResolutionProvider {
    async fn get_applicable_variants(
        &self,
        context: &EvaluationContext,
    ) -> Result<Vec<String>> {
        debug!("Getting applicable variants");

        if !self.options.enable_experiments || !self.data_source.supports_experiments() {
            return Ok(vec![]);
        }

        // Ensure experiments are up to date
        self.ensure_experiments().await?;

        // Get experiments
        let experiments = self.cached_experiments.read().await;
        let experiment_groups = self.cached_experiment_groups.read().await;

        match (experiments.as_ref(), experiment_groups.as_ref()) {
            (Some(exps), Some(groups)) => {
                let (query_data, targeting_key) = self.get_context_from_evaluation_context(context);
                let identifier = targeting_key.as_deref().unwrap_or("");
                let dimensions = self.get_dimensions_info().await;

                superposition_core::experiment::get_applicable_variants(
                    &dimensions,
                    exps,
                    groups,
                    &query_data,
                    identifier,
                    None,
                )
                .map_err(|e| {
                    SuperpositionError::ConfigError(format!(
                        "Failed to get applicable variants: {}",
                        e
                    ))
                })
            }
            _ => Ok(vec![]),
        }
    }

    async fn get_experiment_metadata(
        &self,
        context: &EvaluationContext,
    ) -> Result<Vec<ExperimentMeta>> {
        debug!("Getting experiment metadata");

        if !self.options.enable_experiments || !self.data_source.supports_experiments() {
            return Ok(vec![]);
        }

        // Get applicable variants
        let variant_ids = self.get_applicable_variants(context).await?;

        // Get experiments to map variant IDs to experiment info
        let experiments = self.cached_experiments.read().await;

        let metadata: Vec<ExperimentMeta> = match experiments.as_ref() {
            Some(exps) => {
                // For each variant ID, find the corresponding experiment
                variant_ids
                    .into_iter()
                    .filter_map(|variant_id| {
                        // Find the experiment that contains this variant
                        exps.iter().find_map(|exp| {
                            exp.variants
                                .iter()
                                .find(|v| v.id == variant_id)
                                .map(|variant| ExperimentMeta {
                                    experiment_id: exp.id.clone(),
                                    variant_id: variant.id.clone(),
                                    experiment_name: None, // FfiExperiment doesn't have a name field
                                    variant_name: None,
                                })
                        })
                    })
                    .collect()
            }
            None => vec![],
        };

        Ok(metadata)
    }

    async fn get_experiment_variant(
        &self,
        experiment_id: &str,
        context: &EvaluationContext,
    ) -> Result<Option<String>> {
        debug!("Getting variant for experiment: {}", experiment_id);

        if !self.options.enable_experiments || !self.data_source.supports_experiments() {
            return Ok(None);
        }

        // Get all experiment metadata
        let metadata = self.get_experiment_metadata(context).await?;

        // Find the variant for the requested experiment
        Ok(metadata
            .into_iter()
            .find(|m| m.experiment_id == experiment_id)
            .map(|m| m.variant_id))
    }
}

#[async_trait]
impl FeatureProvider for LocalResolutionProvider {
    fn metadata(&self) -> &ProviderMetadata {
        &self.of_metadata
    }

    async fn initialize(&mut self, _context: &EvaluationContext) {
        if let Err(e) = self.init().await {
            error!("Initialization failed: {}", e);
        }
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        let resolved = self
            .resolve_config(evaluation_context, None)
            .await
            .map_err(|e| EvaluationError {
                code: EvaluationErrorCode::General(format!("Resolution failed: {}", e)),
                message: Some(format!("Resolution failed: {}", e)),
            })?;

        match resolved.get(flag_key) {
            Some(Value::Bool(b)) => Ok(ResolutionDetails::new(*b)),
            Some(_) => Err(EvaluationError {
                code: EvaluationErrorCode::TypeMismatch,
                message: Some("Expected bool".to_string()),
            }),
            None => Err(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some(format!("Flag '{}' not found", flag_key)),
            }),
        }
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        let resolved = self
            .resolve_config(evaluation_context, None)
            .await
            .map_err(|e| EvaluationError {
                code: EvaluationErrorCode::General(format!("Resolution failed: {}", e)),
                message: Some(format!("Resolution failed: {}", e)),
            })?;

        match resolved.get(flag_key) {
            Some(Value::String(s)) => Ok(ResolutionDetails::new(s.clone())),
            Some(_) => Err(EvaluationError {
                code: EvaluationErrorCode::TypeMismatch,
                message: Some("Expected string".to_string()),
            }),
            None => Err(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some(format!("Flag '{}' not found", flag_key)),
            }),
        }
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        let resolved = self
            .resolve_config(evaluation_context, None)
            .await
            .map_err(|e| EvaluationError {
                code: EvaluationErrorCode::General(format!("Resolution failed: {}", e)),
                message: Some(format!("Resolution failed: {}", e)),
            })?;

        match resolved.get(flag_key) {
            Some(Value::Number(n)) => {
                if let Some(i) = n.as_i64() {
                    Ok(ResolutionDetails::new(i))
                } else {
                    Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some("Number is not an integer".to_string()),
                    })
                }
            }
            Some(_) => Err(EvaluationError {
                code: EvaluationErrorCode::TypeMismatch,
                message: Some("Expected int".to_string()),
            }),
            None => Err(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some(format!("Flag '{}' not found", flag_key)),
            }),
        }
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        let resolved = self
            .resolve_config(evaluation_context, None)
            .await
            .map_err(|e| EvaluationError {
                code: EvaluationErrorCode::General(format!("Resolution failed: {}", e)),
                message: Some(format!("Resolution failed: {}", e)),
            })?;

        match resolved.get(flag_key) {
            Some(Value::Number(n)) => {
                if let Some(f) = n.as_f64() {
                    Ok(ResolutionDetails::new(f))
                } else {
                    Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some("Number is not a float".to_string()),
                    })
                }
            }
            Some(_) => Err(EvaluationError {
                code: EvaluationErrorCode::TypeMismatch,
                message: Some("Expected float".to_string()),
            }),
            None => Err(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some(format!("Flag '{}' not found", flag_key)),
            }),
        }
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        let resolved = self
            .resolve_config(evaluation_context, None)
            .await
            .map_err(|e| EvaluationError {
                code: EvaluationErrorCode::General(format!("Resolution failed: {}", e)),
                message: Some(format!("Resolution failed: {}", e)),
            })?;

        match resolved.get(flag_key) {
            Some(value) => {
                match ConversionUtils::serde_value_to_struct_value(value) {
                    Ok(struct_value) => Ok(ResolutionDetails::new(struct_value)),
                    Err(e) => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Failed to convert to struct: {}", e)),
                    }),
                }
            }
            None => Err(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some(format!("Flag '{}' not found", flag_key)),
            }),
        }
    }
}
