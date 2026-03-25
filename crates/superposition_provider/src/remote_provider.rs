use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};

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
        let mut field_keys: Vec<_> = context.custom_fields.keys().cloned().collect();
        field_keys.sort();
        for k in field_keys {
            if let Some(v) = context.custom_fields.get(&k) {
                let serde_val = conversions::evaluation_context_to_value(v.clone());
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
            cache: None,
            global_context: RwLock::new(EvaluationContext::default()),
            metadata: ProviderMetadata {
                name: "SuperpositionAPIProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
        }
    }

    /// Create a new provider with response caching.
    pub fn with_cache(
        options: SuperpositionOptions,
        cache_options: CacheOptions,
    ) -> Self {
        let max_entries = cache_options.size.unwrap_or(1000);
        let ttl = Duration::from_secs(cache_options.ttl.unwrap_or(300));
        let cache = ResponseCache::new(max_entries, ttl);

        Self {
            client: create_client(&options),
            options,
            cache: Some(Arc::new(RwLock::new(cache))),
            global_context: RwLock::new(EvaluationContext::default()),
            metadata: ProviderMetadata {
                name: "SuperpositionAPIProvider".to_string(),
            },
            status: RwLock::new(ProviderStatus::NotReady),
        }
    }

    async fn resolve_remote(
        &self,
        mut context: EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        let cache_key = ResponseCache::cache_key(&context);
        // 1. Check cache for the full (unfiltered) result
        if let Some(ref cache_arc) = self.cache {
            let cache = cache_arc.read().await;
            if let Some(cached_value) = cache.get(&cache_key) {
                log::debug!("SuperpositionAPIProvider: cache hit for key");
                let result = if let Some(prefixes) = prefix_filter {
                    filter_by_prefix(cached_value, prefixes)
                } else {
                    cached_value.clone()
                };
                return Ok(result);
            }
        }

        // 2. Create SDK client
        let client = &self.client;

        let global_context = self.global_context.read().await;
        context.merge_missing(&global_context);

        // 3. Extract context and targeting_key
        let (query_data, _targeting_key) =
            conversions::evaluation_context_to_query(context);

        // 4. Build and send the get_resolved_config request
        // Always fetch WITHOUT prefix filter so we can cache the full result
        let mut builder = client
            .get_resolved_config()
            .workspace_id(&self.options.workspace_id)
            .org_id(&self.options.org_id);

        // Set context dimensions from evaluation context
        let sdk_context: HashMap<String, Document> = query_data
            .into_iter()
            .map(|(k, v)| (k, conversions::value_to_document(v)))
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
        let config_value = conversions::document_to_value(response.config);

        let full_result = match config_value {
            Value::Object(map) => map,
            other => {
                log::warn!(
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
fn filter_by_prefix(
    config: &Map<String, Value>,
    prefixes: &[String],
) -> Map<String, Value> {
    if prefixes.is_empty() {
        return config.clone();
    }
    config
        .iter()
        .filter(|(key, _)| prefixes.iter().any(|prefix| key.starts_with(prefix)))
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect()
}

// ---------------------------------------------------------------------------
// Trait implementations
// ---------------------------------------------------------------------------

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
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>> {
        self.resolve_remote(context, prefix_filter).await
    }
}

// TODO: Pending
#[async_trait]
impl FeatureExperimentMeta for SuperpositionAPIProvider {
    async fn get_applicable_variants(
        &self,
        _context: EvaluationContext,
    ) -> Result<Vec<String>> {
        // Remote resolution handles experiments server-side
        Ok(vec![])
    }
}

#[async_trait]
impl FeatureProvider for SuperpositionAPIProvider {
    // TODO: use context and set as global context for the provider
    async fn initialize(&mut self, _context: &EvaluationContext) {
        log::info!("Initializing SuperpositionAPIProvider...");
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
