use std::collections::{HashMap, HashSet};

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

impl SuperpositionAPIProvider {
    /// Create a new provider without response caching.
    pub fn new(options: SuperpositionOptions) -> Self {
        Self {
            client: Client::from_conf(SdkConfig::from(&options)),
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
        query_data: HashMap<String, Document>,
        identifier: Option<String>,
        prefix_filter: Option<Vec<String>>,
        exclude_prefix_filter: Option<Vec<String>>,
    ) -> Result<Map<String, Value>> {
        // TODO: Check if we need to add a separte check to verify the status of provider before doing stuff

        let response = self
            .client
            .get_resolved_config_with_identifier()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id)
            .set_context(Some(query_data))
            .set_identifier(identifier)
            .set_prefix(prefix_filter)
            .set_exclude_prefix(exclude_prefix_filter)
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

    /// Resolves through `/config/resolve/detailed`, whose entries carry the condition of
    /// the context that last set each key alongside its value.
    async fn resolve_remote_detailed(
        &self,
        query_data: HashMap<String, Document>,
        prefix_filter: Option<Vec<String>>,
        exclude_prefix_filter: Option<Vec<String>>,
    ) -> Result<Map<String, Value>> {
        let response = self
            .client
            .get_detailed_resolved_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id)
            .set_context(Some(query_data))
            .set_prefix(prefix_filter)
            .set_exclude_prefix(exclude_prefix_filter)
            .send()
            .await
            .map_err(|e| {
                SuperpositionError::NetworkError(format!(
                    "Failed to get detailed resolved config: {e}"
                ))
            })?;

        match conversions::document_to_value(response.config) {
            Value::Object(map) => Ok(map),
            other => Err(SuperpositionError::SerializationError(format!(
                "Detailed resolved config is not an object: {other:?}"
            ))),
        }
    }

    async fn fetch_applicable_variants(
        &self,
        query_data: HashMap<String, Document>,
        identifier: String,
        prefix_filter: Option<Vec<String>>,
        exclude_prefix_filter: Option<Vec<String>>,
    ) -> Result<Vec<String>> {
        let applicable_variants = self
            .client
            .applicable_variants()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id)
            .set_context(Some(query_data))
            .identifier(identifier)
            .set_prefix(prefix_filter)
            .set_exclude_prefix(exclude_prefix_filter)
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
impl AllFeatureProvider for SuperpositionAPIProvider {
    async fn resolve_all_features_with_filter(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<Vec<String>>,
        exclude_prefix_filter: Option<Vec<String>>,
    ) -> Result<Map<String, Value>> {
        let (query_data, targeting_key) = self.get_merged_context(context).await;
        self.resolve_remote(
            query_data,
            targeting_key,
            prefix_filter,
            exclude_prefix_filter,
        )
        .await
    }

    async fn resolve_all_features_with_filter_details(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<Vec<String>>,
        exclude_prefix_filter: Option<Vec<String>>,
    ) -> Result<AllFeaturesResolutionDetails> {
        let (mut query_data, targeting_key) = self.get_merged_context(context).await;

        // Without an identifier, a `variantIds` already on the context is the caller's
        // own assignment and is resolved as given.
        if let Some(identifier) = targeting_key {
            let variant_ids = self
                .fetch_applicable_variants(
                    query_data.clone(),
                    identifier,
                    prefix_filter.clone(),
                    exclude_prefix_filter.clone(),
                )
                .await?;
            query_data.insert(
                "variantIds".to_string(),
                Document::Array(variant_ids.into_iter().map(Document::String).collect()),
            );
        }
        let variant_ids: Vec<String> = match query_data.get("variantIds") {
            Some(Document::Array(ids)) => ids
                .iter()
                .filter_map(|id| match id {
                    Document::String(id) => Some(id.clone()),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        };

        let detailed = self
            .resolve_remote_detailed(query_data, prefix_filter, exclude_prefix_filter)
            .await?;

        // Report only the variants that took effect, not every applicable one: the
        // detailed response names the context that last set each key, so a variant
        // whose keys were all overridden by later contexts never shows up there.
        let mut applied = HashSet::new();
        let value = detailed
            .into_iter()
            .map(|(key, entry)| {
                let variant_id = entry
                    .get("context")
                    .and_then(|condition| condition.get("variantIds"))
                    .and_then(Value::as_str);
                if let Some(variant_id) = variant_id {
                    applied.insert(variant_id.to_string());
                }
                let value = match entry {
                    Value::Object(mut entry) => {
                        entry.remove("value").unwrap_or(Value::Null)
                    }
                    other => other,
                };
                (key, value)
            })
            .collect();

        Ok(AllFeaturesResolutionDetails {
            value,
            variant_ids: variant_ids
                .into_iter()
                .filter(|id| applied.contains(id))
                .collect(),
        })
    }
}

#[async_trait]
impl FeatureExperimentMeta for SuperpositionAPIProvider {
    async fn get_applicable_variants(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<Vec<String>>,
        exclude_prefix_filter: Option<Vec<String>>,
    ) -> Result<Vec<String>> {
        let (query_data, targeting_key) = self.get_merged_context(context).await;
        self.fetch_applicable_variants(
            query_data,
            targeting_key.unwrap_or_default(),
            prefix_filter,
            exclude_prefix_filter,
        )
        .await
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
