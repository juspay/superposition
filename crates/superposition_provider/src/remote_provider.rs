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
        // 1. Check cache (if enabled)
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
            Self::get_context_from_evaluation_context(context);

        // 4. Build and send the get_resolved_config request
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

        // Set prefix filter if provided (server-side filtering)
        if let Some(prefixes) = prefix_filter {
            for prefix in prefixes {
                builder = builder.prefix(prefix);
            }
        }

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

        let result = match config_value {
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

        // Cache the full result (before prefix filtering) if caching is enabled
        // For caching, we need the full result, so if prefix_filter was passed to the SDK
        // the cached result is already filtered.
        if let Some(ref cache_arc) = self.cache {
            let cache_key = ResponseCache::cache_key(context);
            let mut cache = cache_arc.write().await;
            cache.put(cache_key, result.clone());
        }

        Ok(result)
    }

    fn get_context_from_evaluation_context(
        ctx: &EvaluationContext,
    ) -> (Map<String, Value>, Option<String>) {
        let context = ctx
            .custom_fields
            .iter()
            .map(|(k, v)| {
                (
                    k.clone(),
                    ConversionUtils::convert_evaluation_context_value_to_serde_value(v),
                )
            })
            .collect();

        (context, ctx.targeting_key.clone())
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
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => {
                if let Some(value) = config.get(flag_key) {
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
