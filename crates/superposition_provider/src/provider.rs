use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
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
use superposition_types::{Config, DimensionInfo};
use tokio::sync::RwLock;

use crate::client::{CacConfig, ExperimentationConfig};
use crate::types::*;
use crate::utils::ConversionUtils;

#[derive(Debug, Clone)]
pub struct SuperpositionProvider {
    metadata: ProviderMetadata,
    status: Arc<RwLock<ProviderStatus>>,
    cac_config: Option<CacConfig>,
    exp_config: Option<ExperimentationConfig>,
}
impl SuperpositionProvider {
    pub fn new(provider_options: SuperpositionProviderOptions) -> Self {
        // Create CAC config
        let superposition_options = SuperpositionOptions::new(
            provider_options.endpoint,
            provider_options.token,
            provider_options.org_id,
            provider_options.workspace_id,
        );
        let cac_options = ConfigurationOptions::new(
            provider_options.refresh_strategy,
            provider_options.evaluation_cache,
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

        Self {
            metadata: ProviderMetadata {
                name: "SuperpositionProvider".to_string(),
            },
            status: Arc::new(RwLock::new(ProviderStatus::NotReady)),
            cac_config: Some(cac_config),
            exp_config,
        }
    }

    fn get_context_from_evaluation_context(
        &self,
        evaluation_context: &EvaluationContext,
    ) -> (serde_json::Map<String, Value>, Option<String>) {
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
        match &self.cac_config {
            Some(cac_config) => cac_config
                .get_cached_config()
                .await
                .map(|c| c.dimensions.clone())
                .unwrap_or_default(),
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
            self.get_context_from_evaluation_context(evaluation_context);

        let dimensions_info = self.get_dimensions_info().await;
        let variant_ids = if let Some(exp_config) = &self.exp_config {
            exp_config
                .get_applicable_variants(dimensions_info, context.clone(), targeting_key)
                .await?
        } else {
            vec![]
        };

        context.insert(
            "variantIds".to_string(),
            Value::Array(variant_ids.into_iter().map(Value::String).collect()),
        );

        match &self.cac_config {
            Some(cac_config) => cac_config.evaluate_config(context, None).await,
            None => Err(SuperpositionError::ConfigError(
                "CAC config not initialized".into(),
            )),
        }
    }

    pub async fn get_cached_config(
        &self,
        dimension_filter: Option<Map<String, Value>>,
        prefix_filters: Option<Vec<String>>,
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

        if let Some(prefix) = prefix_filters.filter(|f| !f.is_empty()) {
            cached_config = cached_config.filter_by_prefix(&HashSet::from_iter(prefix));
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
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
                    // Use the conversion utility we added earlier
                    match ConversionUtils::serde_value_to_struct_value(value) {
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
