use std::collections::HashMap;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use log::{error, info};
use open_feature::{
    provider::FeatureProvider,
    provider::{ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationError, EvaluationErrorCode, EvaluationResult,
    StructValue,
};
use serde_json::{json, Value};
use superposition_core::Experiments;
use superposition_types::DimensionInfo;
use tokio::sync::RwLock;

use crate::client::{CacClient, ExperimentationClient};
use crate::types::*;
use crate::utils::ConversionUtils;

pub type ResolutionResponse = (serde_json::Map<String, Value>, Vec<String>);

#[derive(Debug)]
pub struct SuperpositionProvider {
    metadata: ProviderMetadata,
    status: RwLock<ProviderStatus>,
    cac_client: Option<CacClient>,
    exp_client: Option<ExperimentationClient>,
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

        let cac_client =
            CacClient::new(superposition_options.clone(), cac_options.clone());

        let exp_client =
            provider_options
                .experimentation_options
                .as_ref()
                .map(|exp_opts| {
                    ExperimentationClient::new(
                        superposition_options.clone(),
                        exp_opts.clone(),
                    )
                });

        Self {
            metadata: ProviderMetadata {
                name: "SuperpositionProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
            cac_client: Some(cac_client),
            exp_client,
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
        match &self.cac_client {
            Some(client) => client
                .get_cached_config()
                .await
                .map(|c| c.dimensions.clone())
                .unwrap_or_default(),
            None => HashMap::new(),
        }
    }

    async fn resolve_value<T>(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
        converter: fn(&Value) -> EvaluationResult<T>,
    ) -> EvaluationResult<ResolutionDetails<T>> {
        let (config, variants) = self
            .eval_config(evaluation_context, None)
            .await
            .map_err(|e| {
                error!("Error evaluating flag {}: {}", flag_key, e);
                EvaluationError {
                    code: EvaluationErrorCode::General("EVALUATION_ERROR".to_string()),
                    message: Some(format!(
                        "could not evaluate config for the given context: {}",
                        e
                    )),
                }
            })?;
        let value = config
            .get(flag_key)
            .ok_or(EvaluationError {
                code: EvaluationErrorCode::FlagNotFound,
                message: Some("Flag not found in configuration".to_string()),
            })
            .and_then(converter)?;
        let mut resolution_details = ResolutionDetails::new(value);
        if !variants.is_empty() {
            resolution_details.variant = Some(variants.join(","))
        }
        Ok(resolution_details)
    }

    pub async fn init(&self) -> Result<()> {
        // Initialize CAC config
        if let Some(client) = &self.cac_client {
            match client.create_config().await {
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
        if let Some(client) = &self.exp_client {
            match client.create_config().await {
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
        prefix_filters: Option<Vec<String>>,
    ) -> Result<ResolutionResponse> {
        self.eval_config(evaluation_context, prefix_filters).await
    }

    pub async fn get_satisfied_experiments(
        &self,
        context: &EvaluationContext,
        filter_prefixes: Option<Vec<String>>,
    ) -> Result<Experiments> {
        let Some(ref exp_client) = self.exp_client else {
            return Err(SuperpositionError::ProviderError(
                "Experimentation config not initialized".into(),
            ));
        };
        let (context_map, _) = self.get_context_from_evaluation_context(context);
        exp_client
            .get_satisfied_experiments(&context_map, filter_prefixes)
            .await
    }

    pub async fn get_running_experiments_from_provider(&self) -> Result<Experiments> {
        let Some(exp_client) = &self.exp_client else {
            return Err(SuperpositionError::ProviderError(
                "Experimentation config not initialized".into(),
            ));
        };
        exp_client.get_cached_experiments().await.ok_or(
            SuperpositionError::ProviderError(
                "Could not retrieve running experiments".into(),
            ),
        )
    }

    pub async fn get_last_modified_time(&self) -> Result<DateTime<Utc>> {
        let Some(cac_client) = &self.cac_client else {
            return Err(SuperpositionError::ConfigError(
                "CAC client not initialized".into(),
            ));
        };
        let cac_last_modified = cac_client.last_updated.read().await.ok_or(
            SuperpositionError::ConfigError(
                "Could not retrieve last modified time".into(),
            ),
        )?;
        if let Some(exp_client) = &self.exp_client {
            let exp_last_modified = exp_client.last_updated.read().await;
            match *exp_last_modified {
                Some(exp_time) if exp_time > cac_last_modified => {
                    return Ok(exp_time);
                }
                _ => {
                    return Ok(cac_last_modified);
                }
            }
        }
        Ok(cac_last_modified)
    }

    async fn eval_config(
        &self,
        evaluation_context: &EvaluationContext,
        prefix_filters: Option<Vec<String>>,
    ) -> Result<ResolutionResponse> {
        // Get cached config from CAC
        let (mut context, targeting_key) =
            self.get_context_from_evaluation_context(evaluation_context);

        let dimensions_info = self.get_dimensions_info().await;
        let mut variant_ids = Vec::new();
        if targeting_key.is_some() {
            if let Some(exp_config) = &self.exp_client {
                let applicable_variant_ids = exp_config
                    .get_applicable_variants(&dimensions_info, &context, targeting_key)
                    .await?;

                context.insert("variantIds".to_string(), json!(applicable_variant_ids));
                variant_ids = applicable_variant_ids;
            } else {
                log::warn!("Targeting key is set, but experiments have not been defined in the superposition provider builder options")
            }
        }
        let Some(ref client) = self.cac_client else {
            return Err(SuperpositionError::ConfigError(
                "CAC config not initialized".into(),
            ));
        };
        let config = client.evaluate_config(&context, prefix_filters).await?;
        Ok((config, variant_ids))
    }
}

#[async_trait]
impl FeatureProvider for SuperpositionProvider {
    async fn initialize(&mut self, _context: &EvaluationContext) {
        info!("Initializing SuperpositionProvider...");
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

        info!("SuperpositionProvider initialized successfully");
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        self.resolve_value(flag_key, evaluation_context, |v| {
            v.as_bool().ok_or(EvaluationError {
                code: EvaluationErrorCode::TypeMismatch,
                message: Some(
                    "The value could not be parsed into the desired type".to_string(),
                ),
            })
        })
        .await
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        self.resolve_value(flag_key, evaluation_context, |v| {
            v.as_str().map(|s| s.to_string()).ok_or(EvaluationError {
                code: EvaluationErrorCode::TypeMismatch,
                message: Some(
                    "The value could not be parsed into the desired type".to_string(),
                ),
            })
        })
        .await
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        self.resolve_value(flag_key, evaluation_context, |v| {
            v.as_i64().ok_or(EvaluationError {
                code: EvaluationErrorCode::TypeMismatch,
                message: Some(
                    "The value could not be parsed into the desired type".to_string(),
                ),
            })
        })
        .await
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        self.resolve_value(flag_key, evaluation_context, |v| {
            v.as_f64().ok_or(EvaluationError {
                code: EvaluationErrorCode::TypeMismatch,
                message: Some(
                    "The value could not be parsed into the desired type".to_string(),
                ),
            })
        })
        .await
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        self.resolve_value(flag_key, evaluation_context, |v| {
            ConversionUtils::serde_value_to_struct_value(v).map_err(|e| EvaluationError {
                code: EvaluationErrorCode::TypeMismatch,
                message: Some(format!(
                    "The value could not be parsed into the desired type: {}",
                    e
                )),
            })
        })
        .await
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
