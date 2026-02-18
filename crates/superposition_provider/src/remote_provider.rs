use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};

use async_trait::async_trait;
use aws_smithy_types::Document;
use log::{debug, error, info, warn};
use open_feature::{
    provider::FeatureProvider,
    provider::{ProviderMetadata, ProviderStatus, ResolutionDetails},
    EvaluationContext, EvaluationError, EvaluationErrorCode, EvaluationResult, StructValue,
};
use serde_json::{Map, Value};
use superposition_sdk::{Client, Config as SdkConfig};
use tokio::sync::RwLock;

use crate::traits::{AllFeatureProvider, FeatureExperimentMeta};
use crate::types::*;
use crate::utils::ConversionUtils;

// ---------------------------------------------------------------------------
// ResponseCache internals
// ---------------------------------------------------------------------------

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
        // If at capacity, evict expired entries first
        if self.entries.len() >= self.max_entries {
            let now = Instant::now();
            self.entries
                .retain(|_, entry| now.duration_since(entry.created_at) < self.ttl);
        }

        // If still at capacity, remove the oldest entry
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
        let mut parts: Vec<String> = Vec::new();

        // Include targeting_key
        if let Some(ref tk) = context.targeting_key {
            parts.push(format!("tk={}", tk));
        }

        // Include sorted custom_fields for deterministic keys
        let mut field_keys: Vec<&String> = context.custom_fields.keys().collect();
        field_keys.sort();
        for k in field_keys {
            if let Some(v) = context.custom_fields.get(k) {
                let serde_val =
                    ConversionUtils::convert_evaluation_context_value_to_serde_value(v);
                parts.push(format!("{}={}", k, serde_val));
            }
        }

        parts.join("|")
    }
}

// ---------------------------------------------------------------------------
// SuperpositionAPIProvider
// ---------------------------------------------------------------------------

pub struct SuperpositionAPIProvider {
    options: SuperpositionOptions,
    cache: Option<Arc<RwLock<ResponseCache>>>,
    metadata: ProviderMetadata,
    status: RwLock<ProviderStatus>,
}

impl SuperpositionAPIProvider {
    /// Create a new provider without response caching.
    pub fn new(options: SuperpositionOptions) -> Self {
        Self {
            options,
            cache: None,
            metadata: ProviderMetadata {
                name: "SuperpositionAPIProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
        }
    }

    /// Create a new provider with response caching.
    pub fn with_cache(options: SuperpositionOptions, cache_options: CacheOptions) -> Self {
        let max_entries = cache_options.size.unwrap_or(1000);
        let ttl = Duration::from_secs(cache_options.ttl.unwrap_or(300));
        let cache = ResponseCache::new(max_entries, ttl);

        Self {
            options,
            cache: Some(Arc::new(RwLock::new(cache))),
            metadata: ProviderMetadata {
                name: "SuperpositionAPIProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
        }
    }

    fn create_client(&self) -> Client {
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
        // 1. Check cache for the full (unfiltered) result
        if let Some(ref cache_arc) = self.cache {
            let cache_key = ResponseCache::cache_key(context);
            let cache = cache_arc.read().await;
            if let Some(cached_value) = cache.get(&cache_key) {
                debug!("SuperpositionAPIProvider: cache hit for key");
                let result = if let Some(prefixes) = prefix_filter {
                    filter_by_prefix(cached_value, prefixes)
                } else {
                    cached_value.clone()
                };
                return Ok(result);
            }
        }

        // 2. Create SDK client
        let client = self.create_client();

        // 3. Extract context and targeting_key
        let (query_data, _targeting_key) =
            ConversionUtils::evaluation_context_to_query(context);

        // 4. Build and send the get_resolved_config request
        // Always fetch WITHOUT prefix filter so we can cache the full result
        let mut builder = client
            .get_resolved_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id);

        // Set context dimensions from evaluation context
        let sdk_context: HashMap<String, Document> = query_data
            .iter()
            .map(|(k, v)| (k.clone(), serde_value_to_document(v)))
            .collect();
        builder = builder.set_context(Some(sdk_context));

        // NOTE: We intentionally do NOT set prefix filter on the SDK request
        // so we always get the full config and can cache it. Prefix filtering
        // is applied locally after caching.

        let response = builder.send().await.map_err(|e| {
            SuperpositionError::NetworkError(format!(
                "Failed to get resolved config: {}",
                e
            ))
        })?;

        // 5. Convert response Document to Map<String, Value>
        let config_doc = response.config();
        let config_value = ConversionUtils::document_to_value(config_doc).map_err(|e| {
            SuperpositionError::SerializationError(format!(
                "Failed to convert resolved config response: {}",
                e
            ))
        })?;

        let full_result = match config_value {
            Value::Object(map) => map,
            other => {
                warn!(
                    "SuperpositionAPIProvider: resolved config is not an object, wrapping: {:?}",
                    other
                );
                let mut map = Map::new();
                map.insert("_value".to_string(), other);
                map
            }
        };

        // Cache the full (unfiltered) result
        if let Some(ref cache_arc) = self.cache {
            let cache_key = ResponseCache::cache_key(context);
            let mut cache = cache_arc.write().await;
            cache.put(cache_key, full_result.clone());
        }

        // Apply prefix filtering locally
        let result = if let Some(prefixes) = prefix_filter {
            filter_by_prefix(&full_result, prefixes)
        } else {
            full_result
        };

        Ok(result)
    }

}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

/// Filter a config map to only include keys that start with one of the given prefixes.
fn filter_by_prefix(config: &Map<String, Value>, prefixes: &[String]) -> Map<String, Value> {
    if prefixes.is_empty() {
        return config.clone();
    }
    config
        .iter()
        .filter(|(key, _)| prefixes.iter().any(|prefix| key.starts_with(prefix)))
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect()
}

/// Convert a serde_json::Value to an aws_smithy_types::Document.
fn serde_value_to_document(value: &Value) -> Document {
    match value {
        Value::Null => Document::Null,
        Value::Bool(b) => Document::Bool(*b),
        Value::Number(n) => {
            if let Some(u) = n.as_u64() {
                Document::Number(aws_smithy_types::Number::PosInt(u))
            } else if let Some(i) = n.as_i64() {
                Document::Number(aws_smithy_types::Number::NegInt(i))
            } else if let Some(f) = n.as_f64() {
                Document::Number(aws_smithy_types::Number::Float(f))
            } else {
                Document::Null
            }
        }
        Value::String(s) => Document::String(s.clone()),
        Value::Array(arr) => {
            Document::Array(arr.iter().map(serde_value_to_document).collect())
        }
        Value::Object(obj) => {
            let map: HashMap<String, Document> = obj
                .iter()
                .map(|(k, v)| (k.clone(), serde_value_to_document(v)))
                .collect();
            Document::Object(map)
        }
    }
}

// ---------------------------------------------------------------------------
// Trait implementations
// ---------------------------------------------------------------------------

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
        Ok(vec![])
    }
}

#[async_trait]
impl FeatureProvider for SuperpositionAPIProvider {
    async fn initialize(&mut self, _context: &EvaluationContext) {
        info!("Initializing SuperpositionAPIProvider...");
        {
            let mut status = self.status.write().await;
            *status = ProviderStatus::Ready;
        }
        info!("SuperpositionAPIProvider initialized successfully");
    }

    async fn resolve_bool_value(
        &self,
        flag_key: &str,
        evaluation_context: &EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_bool() {
                    Some(bool_val) => Ok(ResolutionDetails::new(bool_val)),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not a boolean", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                error!("Error evaluating boolean flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
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
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_str() {
                    Some(str_val) => Ok(ResolutionDetails::new(str_val.to_owned())),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not a string", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                error!("Error evaluating String flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
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
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_i64() {
                    Some(int_val) => Ok(ResolutionDetails::new(int_val)),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not an integer", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                error!("Error evaluating integer flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
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
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_f64() {
                    Some(float_val) => Ok(ResolutionDetails::new(float_val)),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not a float", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                error!("Error evaluating float flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
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
            Ok(config) => match config.get(flag_key) {
                Some(value) => match ConversionUtils::serde_value_to_struct_value(value) {
                    Ok(struct_value) => Ok(ResolutionDetails::new(struct_value)),
                    Err(e) => {
                        error!("Error converting value to StructValue: {}", e);
                        Err(EvaluationError {
                            code: EvaluationErrorCode::TypeMismatch,
                            message: Some(format!(
                                "Flag '{}' is not a struct: {}",
                                flag_key, e
                            )),
                        })
                    }
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                error!("Error evaluating Object flag {}: {}", flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
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
