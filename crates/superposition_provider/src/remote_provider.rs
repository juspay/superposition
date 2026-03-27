use std::collections::HashMap;

use async_trait::async_trait;
use aws_smithy_types::Document;
use open_feature::{
    provider::FeatureProvider,
    provider::{ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationResult, StructValue,
};
use serde_json::{Map, Value};
use superposition_sdk::{Client, Config as SdkConfig};
use tokio::sync::RwLock;

use crate::traits::{AllFeatureProvider, FeatureExperimentMeta};
use crate::{conversions, types::*};

pub struct SuperpositionAPIProvider {
    options: SuperpositionOptions,
    global_context: RwLock<EvaluationContext>,
    metadata: ProviderMetadata,
    status: RwLock<ProviderStatus>,
    client: Client,
}

fn create_client(options: &SuperpositionOptions) -> Client {
    let sdk_config = SdkConfig::builder()
        .endpoint_url(&options.endpoint)
        .bearer_token(options.token.clone().into())
        .behavior_version_latest()
        .build();

    Client::from_conf(sdk_config)
}

impl SuperpositionAPIProvider {
    /// Create a new provider without response caching.
    pub fn new(options: SuperpositionOptions) -> Self {
        Self {
            client: create_client(&options),
            options,
            global_context: RwLock::new(EvaluationContext::default()),
            metadata: ProviderMetadata {
                name: "SuperpositionAPIProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
        }
    }

    async fn get_merged_context(
        &self,
        mut context: EvaluationContext,
    ) -> (HashMap<String, Document>, Option<String>) {
        let global_context = self.global_context.read().await;
        context.merge_missing(&global_context);

        conversions::evaluation_context_to_query_document(context)
    }

    async fn resolve_remote(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<Vec<String>>,
    ) -> Result<Map<String, Value>> {
        // TODO: Check if we need to add a separte check to verify the status of provider before doing stuff

        let (query_data, targeting_key) = self.get_merged_context(context).await;

        let response = self
            .client
            .get_resolved_config_with_identifier()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id)
            .set_context(Some(query_data))
            .set_identifier(targeting_key)
            .set_prefix(prefix_filter)
            .send()
            .await
            .map_err(|e| {
                SuperpositionError::NetworkError(format!(
                    "Failed to get resolved config: {}",
                    e
                ))
            })?;

        let config_value = conversions::document_to_value(response.config);

        let result = match config_value {
            Value::Object(map) => map,
            other => {
                log::warn!(
                    "SuperpositionAPIProvider: resolved config is not an object, wrapping: {:?}",
                    other
                );
                [("_value".to_string(), other)].into_iter().collect()
            }
        };

        Ok(result)
    }
}

#[async_trait]
impl AllFeatureProvider for SuperpositionAPIProvider {
    async fn resolve_all_features(
        &self,
        context: EvaluationContext,
    ) -> Result<Map<String, Value>> {
        self.resolve_remote(context, None).await
    }

    async fn resolve_all_features_with_filter(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<Vec<String>>,
    ) -> Result<Map<String, Value>> {
        self.resolve_remote(context, prefix_filter).await
    }
}

#[async_trait]
impl FeatureExperimentMeta for SuperpositionAPIProvider {
    async fn get_applicable_variants(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<Vec<String>>,
    ) -> Result<Vec<String>> {
        let (query_data, targeting_key) = self.get_merged_context(context).await;
        let Some(targeting_key) = targeting_key else {
            return Err(SuperpositionError::ProviderError(
                "Missing targeting key in evaluation context".to_string(),
            ));
        };

        let applicable_variants = self
            .client
            .applicable_variants()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id)
            .set_context(Some(query_data))
            .identifier(targeting_key)
            .set_prefix(prefix_filter)
            .send()
            .await
            .map_err(|e| {
                SuperpositionError::NetworkError(format!(
                    "Failed to get applicable variants: {e}",
                ))
            })?;

        Ok(applicable_variants.data.into_iter().map(|v| v.id).collect())
    }
}

#[async_trait]
impl FeatureProvider for SuperpositionAPIProvider {
    async fn initialize(&mut self, context: &EvaluationContext) {
        log::info!("Initializing SuperpositionAPIProvider...");
        {
            let mut global_context = self.global_context.write().await;
            *global_context = context.clone();
        }
        {
            let mut status = self.status.write().await;
            *status = ProviderStatus::Ready;
        }
        log::info!("SuperpositionAPIProvider initialized successfully");
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        self.resolve_bool(flag_key, evaluation_context.clone())
            .await
    }

    async fn resolve_string_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        self.resolve_string(flag_key, evaluation_context.clone())
            .await
    }

    async fn resolve_int_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        self.resolve_int(flag_key, evaluation_context.clone()).await
    }

    async fn resolve_float_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        self.resolve_float(flag_key, evaluation_context.clone())
            .await
    }

    async fn resolve_struct_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        self.resolve_struct(flag_key, evaluation_context.clone())
            .await
    }

    fn metadata(&self) -> &ProviderMetadata {
        &self.metadata
    }

    fn status(&self) -> ProviderStatus {
        match self.status.try_read() {
            // need to do this as ProviderStatus neither implements Copy nor Clone
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
