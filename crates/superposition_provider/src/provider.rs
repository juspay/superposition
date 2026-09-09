use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use async_trait::async_trait;
use log::{error, info};
use open_feature::{
    provider::FeatureProvider,
    provider::{ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationError, EvaluationErrorCode, EvaluationResult,
    StructValue,
};
use serde_json::{Map, Value};
use superposition_core::eval_cache::{pairs_from_json_map, EvaluationCache};
use superposition_core::MergeStrategy;
use superposition_types::{Config, ConfigFilter, DimensionInfo, PrefixList};
use tokio::sync::RwLock;

use crate::types::*;
use crate::{
    client::{CacConfig, ExperimentationConfig},
    conversions,
};

#[derive(Debug, Clone)]
pub struct SuperpositionProvider {
    metadata: ProviderMetadata,
    status: Arc<RwLock<ProviderStatus>>,
    cac_config: Option<CacConfig>,
    exp_config: Option<ExperimentationConfig>,
    evaluation_cache: Arc<Mutex<EvaluationCache>>,
}
impl SuperpositionProvider {
    pub fn new(provider_options: SuperpositionProviderOptions) -> Self {
        // Create CAC config
        let superposition_options = SuperpositionOptions::new(
            provider_options.endpoint,
            AuthMethod::Token(provider_options.token),
            provider_options.org_id,
            provider_options.workspace_id,
        );
        let cac_options = ConfigurationOptions::new(
            provider_options.refresh_strategy,
            provider_options.fallback_config.clone(),
        );

        let cac_config =
            CacConfig::new(superposition_options.clone(), cac_options.clone());

        let exp_config =
            provider_options
                .experimentation_options
                .as_ref()
                .map(|exp_opts| {
                    ExperimentationConfig::new(
                        superposition_options.clone(),
                        exp_opts.clone(),
                    )
                });

        let evaluation_cache = EvaluationCache::new(
            provider_options
                .evaluation_cache_options
                .map(|o| o.max_size_mb)
                .unwrap_or(0),
        );

        Self {
            metadata: ProviderMetadata {
                name: "SuperpositionProvider".to_string(),
            },
            status: Arc::new(RwLock::new(ProviderStatus::NotReady)),
            cac_config: Some(cac_config),
            exp_config,
            evaluation_cache: Arc::new(Mutex::new(evaluation_cache)),
        }
    }

    /// Runs `f` with the evaluation cache locked; a poisoned lock disables
    /// caching for the call rather than failing the resolution.
    fn with_eval_cache<T>(&self, f: impl FnOnce(&mut EvaluationCache) -> T) -> Option<T> {
        self.evaluation_cache
            .lock()
            .ok()
            .map(|mut cache| f(&mut cache))
    }

    async fn get_dimensions_info(&self) -> HashMap<String, DimensionInfo> {
        match &self.cac_config {
            Some(cac_config) => cac_config.get_dimensions().await,
            None => HashMap::new(),
        }
    }

    pub async fn init(&self) -> Result<()> {
        // Initialize CAC config
        if let Some(cac_config) = &self.cac_config {
            match cac_config.create_config().await {
                Ok(_) => info!("CAC configuration initialized successfully"),
                Err(e) => {
                    error!("Failed to initialize CAC configuration: {}", e);
                    return Err(SuperpositionError::ConfigError(format!(
                        "Failed to initialize CAC configuration: {}",
                        e
                    )));
                }
            }
        }

        // Initialize experimentation config if available
        if let Some(exp_config) = &self.exp_config {
            match exp_config.create_config().await {
                Ok(_) => info!("Experimentation configuration initialized successfully"),
                Err(e) => {
                    error!("Failed to initialize experimentation configuration: {}", e);
                    return Err(SuperpositionError::ConfigError(format!(
                        "Failed to initialize experimentation configuration: {}",
                        e
                    )));
                }
            }
        };
        Ok(())
    }

    pub async fn resolve_full_config(
        &self,
        evaluation_context: &EvaluationContext,
    ) -> Result<serde_json::Map<String, Value>> {
        self.eval_config(evaluation_context).await
    }

    async fn eval_config(
        &self,
        evaluation_context: &EvaluationContext,
    ) -> Result<serde_json::Map<String, Value>> {
        // Get cached config from CAC
        let (mut context, targeting_key) =
            conversions::evaluation_context_to_query(evaluation_context.clone());

        // Key the evaluation by the current data generations: any config or
        // experiment refresh changes them, so stale entries become unreachable.
        let data_version = (
            self.cac_config.as_ref().map_or(0, |c| c.generation()),
            self.exp_config.as_ref().map_or(0, |c| c.generation()),
        );
        let cache_key = self
            .with_eval_cache(|cache| {
                cache.is_enabled().then(|| {
                    EvaluationCache::key(
                        data_version,
                        &pairs_from_json_map(&context),
                        MergeStrategy::MERGE,
                        None,
                        None,
                        targeting_key.as_deref(),
                    )
                })
            })
            .flatten();

        if let Some(hit) =
            cache_key.and_then(|key| self.with_eval_cache(|c| c.get(&key)).flatten())
        {
            return Ok(hit);
        }

        // Dimensions are only needed to resolve experiment variants, so avoid
        // fetching (and cloning) them entirely when experimentation is off.
        let variant_ids = if let Some(exp_config) = &self.exp_config {
            let dimensions_info = self.get_dimensions_info().await;
            exp_config
                .get_applicable_variants(&dimensions_info, context.clone(), targeting_key)
                .await?
        } else {
            vec![]
        };

        context.insert(
            "variantIds".to_string(),
            Value::Array(variant_ids.into_iter().map(Value::String).collect()),
        );

        let result = match &self.cac_config {
            Some(cac_config) => cac_config.evaluate_config(context, None, None).await?,
            None => {
                return Err(SuperpositionError::ConfigError(
                    "CAC config not initialized".into(),
                ))
            }
        };

        if let Some(key) = cache_key {
            self.with_eval_cache(|cache| cache.insert(key, result.clone()));
        }

        Ok(result)
    }

    pub async fn get_cached_config(
        &self,
        dimension_filter: Option<Map<String, Value>>,
        prefix_filters: Option<Vec<String>>,
        exclude_prefix_filters: Option<Vec<String>>,
    ) -> Result<Config> {
        let Some(cac_client) = &self.cac_config else {
            return Err(SuperpositionError::ConfigError(
                "CAC client not initialized".into(),
            ));
        };

        let Some(mut cached_config) = cac_client.get_cached_config().await else {
            return Err(SuperpositionError::ConfigError(
                    "No cached config available, please check if the config settings are configured correctly".into(),
                ));
        };

        let prefix = PrefixList::from(prefix_filters);
        let exclude_prefix = PrefixList::from(exclude_prefix_filters);
        if !prefix.is_empty() || !exclude_prefix.is_empty() {
            cached_config = cached_config.filter_by_prefix(&prefix, &exclude_prefix);
        }

        if let Some(dimension_filter) =
            dimension_filter.filter(|query_map| !query_map.is_empty())
        {
            cached_config = cached_config.filter_by_dimensions(dimension_filter);
        };

        Ok(cached_config)
    }
}

#[async_trait]
impl FeatureProvider for SuperpositionProvider {
    async fn initialize(&mut self, _context: &EvaluationContext) {
        info!("Initializing SuperpositionProvider...");
        {
            let status = self.status.read().await;
            if *status == ProviderStatus::Ready {
                info!("SuperpositionProvider is already initialized");
                return;
            }
        }
        if (self.init().await).is_err() {
            let mut status = self.status.write().await;
            *status = ProviderStatus::Error;
            return;
        }

        let mut status = self.status.write().await;
        *status = ProviderStatus::Ready;

        info!("SuperpositionProvider initialized successfully");
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        match self.eval_config(evaluation_context).await {
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
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        match self.eval_config(evaluation_context).await {
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
                error!("Error evaluating String flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        match self.eval_config(evaluation_context).await {
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
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        match self.eval_config(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    if let Some(int_val) = value.as_f64() {
                        return Ok(ResolutionDetails::new(int_val));
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
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        match self.eval_config(evaluation_context).await {
            Ok(mut config) => {
                if let Some(value) = config.remove(flag_key) {
                    // Use the conversion utility we added earlier
                    match conversions::value_to_struct(value) {
                        Ok(struct_value) => {
                            return Ok(ResolutionDetails::new(struct_value));
                        }
                        Err(e) => {
                            error!("Error converting value to StructValue: {}", e);
                            return Err(EvaluationError {
                                code: EvaluationErrorCode::ParseError,
                                message: Some(format!(
                                    "Failed to parse struct value: {}",
                                    e
                                )),
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
                error!("Error evaluating Object flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some("Flag not found in configuration".to_string()),
                })
            }
        }
    }

    fn metadata(&self) -> &ProviderMetadata {
        &self.metadata
    }

    fn status(&self) -> ProviderStatus {
        // Since we can't await in a non-async function, we need to handle this differently
        // We'll use try_read() which returns immediately
        match self.status.try_read() {
            Ok(status) => match *status {
                ProviderStatus::Ready => ProviderStatus::Ready,
                ProviderStatus::Error => ProviderStatus::Error,
                ProviderStatus::NotReady => ProviderStatus::NotReady,
                ProviderStatus::STALE => ProviderStatus::STALE,
            },
            Err(_) => ProviderStatus::NotReady, // Default if lock is held
        }
    }
}
