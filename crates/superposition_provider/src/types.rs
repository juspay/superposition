use serde_json::Value;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum SuperpositionError {
    #[error("Configuration error: {0}")]
    ConfigError(String),
    #[error("Network error: {0}")]
    NetworkError(String),
    #[error("Serialization error: {0}")]
    SerializationError(String),
    #[error("Provider error: {0}")]
    ProviderError(String),
    #[error("Data source error: {0}")]
    DataSourceError(String),
    #[error("Refresh error: {0}")]
    RefreshError(String),
}

pub type Result<T> = std::result::Result<T, SuperpositionError>;

#[derive(Debug, Clone)]
pub enum AuthMethod {
    Token(String),
    Basic { username: String, password: String },
}

#[derive(Debug, Clone)]
pub struct SuperpositionOptions {
    pub endpoint: String,
    pub auth: AuthMethod,
    pub org_id: String,
    pub workspace_id: String,
}

impl SuperpositionOptions {
    pub fn new(
        endpoint: String,
        auth: AuthMethod,
        org_id: String,
        workspace_id: String,
    ) -> Result<Self> {
        if endpoint.trim().is_empty() {
            return Err(SuperpositionError::ConfigError(
                "endpoint is required".to_string(),
            ));
        }
        match &auth {
            AuthMethod::Token(token) => {
                if token.trim().is_empty() {
                    return Err(SuperpositionError::ConfigError(
                        "token is required".to_string(),
                    ));
                }
            }
            AuthMethod::Basic { username, password } => {
                if username.trim().is_empty() {
                    return Err(SuperpositionError::ConfigError(
                        "username is required".to_string(),
                    ));
                }
                if password.trim().is_empty() {
                    return Err(SuperpositionError::ConfigError(
                        "password is required".to_string(),
                    ));
                }
            }
        }
        if org_id.trim().is_empty() {
            return Err(SuperpositionError::ConfigError(
                "org_id is required".to_string(),
            ));
        }
        if workspace_id.trim().is_empty() {
            return Err(SuperpositionError::ConfigError(
                "workspace_id is required".to_string(),
            ));
        }
        Ok(Self {
            endpoint,
            auth,
            org_id,
            workspace_id,
        })
    }
}

impl From<&SuperpositionOptions> for superposition_sdk::Config {
    fn from(options: &SuperpositionOptions) -> Self {
        let sdk_config = superposition_sdk::Config::builder()
            .endpoint_url(&options.endpoint)
            .behavior_version_latest();

        let sdk_config = match &options.auth {
            AuthMethod::Token(token) => sdk_config
                .bearer_token(superposition_sdk::config::Token::new(token, None)),
            AuthMethod::Basic { username, password } => sdk_config.basic_auth_login(
                superposition_sdk::config::Login::new(username, password, None),
            ),
        };

        sdk_config.build()
    }
}

/// Polling strategy configuration.
///
/// Polling strategy configuration. All durations are milliseconds.
#[derive(Debug, Clone)]
pub struct PollingStrategy {
    /// How often to refresh, in milliseconds.
    pub interval_milliseconds: u64,
    /// How long a single refresh may take before it is abandoned, in milliseconds.
    /// `None` means unbounded.
    pub timeout_milliseconds: Option<u64>,
}

impl PollingStrategy {
    /// Build a polling strategy from millisecond durations.
    pub fn new(interval_milliseconds: u64) -> Self {
        Self {
            interval_milliseconds,
            timeout_milliseconds: None,
        }
    }

    pub fn with_timeout(mut self, timeout_milliseconds: u64) -> Self {
        self.timeout_milliseconds = Some(timeout_milliseconds);
        self
    }

    /// The refresh interval in milliseconds.
    pub fn interval_ms(&self) -> u64 {
        self.interval_milliseconds
    }

    /// The refresh timeout in milliseconds, if one is set.
    pub fn timeout_ms(&self) -> Option<u64> {
        self.timeout_milliseconds
    }
}

impl Default for PollingStrategy {
    fn default() -> Self {
        Self {
            interval_milliseconds: 60_000,      // 1 minute
            timeout_milliseconds: Some(30_000), // 30 seconds
        }
    }
}

/// On-demand strategy configuration. All durations are milliseconds.
#[derive(Debug, Clone)]
pub struct OnDemandStrategy {
    /// How long cached data stays fresh, in milliseconds.
    pub ttl_milliseconds: u64,
    /// How long a single refresh may take before it is abandoned, in milliseconds.
    /// `None` means unbounded.
    pub timeout_milliseconds: Option<u64>,
    pub use_stale_on_error: Option<bool>,
}

impl OnDemandStrategy {
    /// Build an on-demand strategy from millisecond durations.
    pub fn new(ttl_milliseconds: u64) -> Self {
        Self {
            ttl_milliseconds,
            timeout_milliseconds: None,
            use_stale_on_error: None,
        }
    }

    pub fn with_timeout(mut self, timeout_milliseconds: u64) -> Self {
        self.timeout_milliseconds = Some(timeout_milliseconds);
        self
    }

    pub fn with_use_stale_on_error(mut self, use_stale_on_error: bool) -> Self {
        self.use_stale_on_error = Some(use_stale_on_error);
        self
    }

    /// The cache TTL in milliseconds.
    pub fn ttl_ms(&self) -> u64 {
        self.ttl_milliseconds
    }

    /// The refresh timeout in milliseconds, if one is set.
    pub fn timeout_ms(&self) -> Option<u64> {
        self.timeout_milliseconds
    }

    /// Whether to serve stale data when a refresh fails.
    ///
    /// Read through this rather than off the field: unset means "unspecified", not "off". A call
    /// site reaching for `unwrap_or_default()` reads it as `false`, which contradicts [`Default`].
    pub fn use_stale_on_error(&self) -> bool {
        self.use_stale_on_error
            .unwrap_or(DEFAULT_USE_STALE_ON_ERROR)
    }
}

/// The single place this default is written. [`Default`] and
/// [`OnDemandStrategy::use_stale_on_error`] both read it, so they cannot drift apart.
const DEFAULT_USE_STALE_ON_ERROR: bool = true;

impl Default for OnDemandStrategy {
    fn default() -> Self {
        Self {
            ttl_milliseconds: 300_000,          // 5 minutes
            timeout_milliseconds: Some(30_000), // 30 seconds
            use_stale_on_error: Some(DEFAULT_USE_STALE_ON_ERROR),
        }
    }
}

/// Configuration for the watch refresh strategy.
#[derive(Debug, Clone)]
pub struct WatchStrategy {
    /// Debounce duration in milliseconds (default: 500).
    pub debounce_ms: Option<u64>,
}

impl Default for WatchStrategy {
    fn default() -> Self {
        Self {
            debounce_ms: Some(500),
        }
    }
}

/// A stream of change notifications from a data source.
pub struct WatchStream {
    pub receiver: tokio::sync::broadcast::Receiver<()>,
}

#[derive(Debug, Clone)]
pub enum RefreshStrategy {
    Polling(PollingStrategy),
    OnDemand(OnDemandStrategy),
    Watch(WatchStrategy),
    Manual,
}

impl Default for RefreshStrategy {
    fn default() -> Self {
        RefreshStrategy::OnDemand(OnDemandStrategy::default())
    }
}

#[derive(Debug, Clone)]
pub struct ConfigurationOptions {
    pub fallback_config: Option<serde_json::Map<String, Value>>,
    pub refresh_strategy: RefreshStrategy,
}

impl ConfigurationOptions {
    pub fn new(
        refresh_strategy: RefreshStrategy,
        fallback_config: Option<serde_json::Map<String, Value>>,
    ) -> Self {
        Self {
            fallback_config,
            refresh_strategy,
        }
    }
}

/// Experimentation options
#[derive(Debug, Clone)]
pub struct ExperimentationOptions {
    pub refresh_strategy: RefreshStrategy,
}

impl ExperimentationOptions {
    pub fn new(refresh_strategy: RefreshStrategy) -> Self {
        Self { refresh_strategy }
    }
}

#[derive(Debug, Clone)]
pub struct SuperpositionProviderOptions {
    pub endpoint: String,
    pub token: String,
    pub org_id: String,
    pub workspace_id: String,
    pub fallback_config: Option<serde_json::Map<String, Value>>,
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
        refresh_strategy: RefreshStrategy,
        experimentation_options: Option<ExperimentationOptions>,
    ) -> Self {
        Self {
            endpoint,
            token,
            org_id,
            workspace_id,
            fallback_config,
            refresh_strategy,
            experimentation_options,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accessors_return_the_millisecond_fields() {
        let polling = PollingStrategy::new(5_500).with_timeout(2_500);
        assert_eq!(polling.interval_ms(), 5_500);
        assert_eq!(polling.timeout_ms(), Some(2_500));

        let on_demand = OnDemandStrategy::new(60_000);
        assert_eq!(on_demand.ttl_ms(), 60_000);
        assert_eq!(on_demand.timeout_ms(), None);
    }

    #[test]
    fn defaults_are_unchanged() {
        assert_eq!(PollingStrategy::default().interval_ms(), 60_000);
        assert_eq!(PollingStrategy::default().timeout_ms(), Some(30_000));
        assert_eq!(OnDemandStrategy::default().ttl_ms(), 300_000);
        assert_eq!(OnDemandStrategy::default().timeout_ms(), Some(30_000));
        assert!(OnDemandStrategy::default().use_stale_on_error());
    }
}
