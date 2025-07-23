use serde::{Deserialize, Serialize};
use serde_json::Value;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SuperpositionError {
    #[error("Configuration error: {0}")]
    ConfigError(String),
    #[error("Network error: {0}")]
    NetworkError(String),
    #[error("Serialization error: {0}")]
    SerializationError(String),
    #[error("Provider error: {0}")]
    ProviderError(String),
}

pub type Result<T> = std::result::Result<T, SuperpositionError>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SuperpositionOptions {
    pub endpoint: String,
    pub token: String,
    pub org_id: String,
    pub workspace_id: String,
}

impl SuperpositionOptions {
    pub fn new(
        endpoint: String,
        token: String,
        org_id: String,
        workspace_id: String,
    ) -> Self {
        Self {
            endpoint,
            token,
            org_id,
            workspace_id,
        }
    }
}

/// Cache configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheOptions {
    pub ttl: Option<u64>,
    pub size: Option<usize>,
}

impl Default for CacheOptions {
    fn default() -> Self {
        Self {
            ttl: Some(300), // 5 minutes
            size: Some(1000),
        }
    }
}

/// Evaluation cache configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvaluationCacheOptions {
    pub ttl: Option<u64>,
    pub size: Option<usize>,
}

impl Default for EvaluationCacheOptions {
    fn default() -> Self {
        Self {
            ttl: Some(60), // 1 minute
            size: Some(500),
        }
    }
}

/// Polling strategy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PollingStrategy {
    pub interval: u64, // seconds
    pub timeout: Option<u64>,
}

impl Default for PollingStrategy {
    fn default() -> Self {
        Self {
            interval: 60, // 1 minute
            timeout: Some(30),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OnDemandStrategy {
    pub ttl: u64, // seconds
    pub timeout: Option<u64>,
    pub use_stale_on_error: Option<bool>,
}

impl Default for OnDemandStrategy {
    fn default() -> Self {
        Self {
            ttl: 300, // 5 minutes
            timeout: Some(30),
            use_stale_on_error: Some(true),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RefreshStrategy {
    Polling(PollingStrategy),
    OnDemand(OnDemandStrategy),
}

impl Default for RefreshStrategy {
    fn default() -> Self {
        RefreshStrategy::OnDemand(OnDemandStrategy::default())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigurationOptions {
    pub fallback_config: Option<serde_json::Map<String, Value>>,
    pub evaluation_cache: Option<EvaluationCacheOptions>,
    pub refresh_strategy: RefreshStrategy,
}

impl ConfigurationOptions {
    pub fn new(
        refresh_strategy: RefreshStrategy,
        evaluation_cache: Option<EvaluationCacheOptions>,
        fallback_config: Option<serde_json::Map<String, Value>>,
    ) -> Self {
        Self {
            fallback_config,
            evaluation_cache,
            refresh_strategy,
        }
    }
}

/// Experimentation options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExperimentationOptions {
    pub refresh_strategy: RefreshStrategy,
    pub evaluation_cache: Option<EvaluationCacheOptions>,
    pub default_toss: Option<u32>,
}

impl ExperimentationOptions {
    pub fn new(refresh_strategy: RefreshStrategy) -> Self {
        Self {
            refresh_strategy,
            evaluation_cache: Some(EvaluationCacheOptions::default()),
            default_toss: None,
        }
    }

    pub fn with_evaluation_cache(
        mut self,
        evaluation_cache: EvaluationCacheOptions,
    ) -> Self {
        self.evaluation_cache = Some(evaluation_cache);
        self
    }

    pub fn with_default_toss(mut self, default_toss: u32) -> Self {
        self.default_toss = Some(default_toss);
        self
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SuperpositionProviderOptions {
    pub endpoint: String,
    pub token: String,
    pub org_id: String,
    pub workspace_id: String,
    pub fallback_config: Option<serde_json::Map<String, Value>>,
    pub evaluation_cache: Option<EvaluationCacheOptions>,
    pub refresh_strategy: RefreshStrategy,
    pub experimentation_options: Option<ExperimentationOptions>,
}

impl SuperpositionProviderOptions {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        endpoint: String,
        token: String,
        org_id: String,
        workspace_id: String,
        fallback_config: Option<serde_json::Map<String, Value>>,
        evaluation_cache: Option<EvaluationCacheOptions>,
        refresh_strategy: RefreshStrategy,
        experimentation_options: Option<ExperimentationOptions>,
    ) -> Self {
        Self {
            endpoint,
            token,
            org_id,
            workspace_id,
            fallback_config,
            evaluation_cache,
            refresh_strategy,
            experimentation_options,
        }
    }
}
