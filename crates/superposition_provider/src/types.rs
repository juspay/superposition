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
    ) -> Self {
        Self {
            endpoint,
            auth,
            org_id,
            workspace_id,
        }
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
/// Durations are milliseconds. The seconds-based `interval` and `timeout` fields still work and are
/// deprecated: a `_milliseconds` field wins when set, otherwise the old field is read as seconds.
///
/// `Default` deliberately leaves the `_milliseconds` fields as `None`, so that the common
/// `PollingStrategy { interval: 30, ..Default::default() }` still means 30 seconds. Defaulting them
/// to `Some(..)` would let the default silently override the caller's seconds value.
#[derive(Debug, Clone)]
pub struct PollingStrategy {
    #[deprecated(note = "seconds-based; use `interval_milliseconds`")]
    pub interval: u64,
    /// How often to refresh, in milliseconds. Wins over `interval` when set.
    pub interval_milliseconds: Option<u64>,
    #[deprecated(note = "seconds-based; use `timeout_milliseconds`")]
    pub timeout: Option<u64>,
    /// How long a single refresh may take before it is abandoned, in milliseconds.
    /// Wins over `timeout` when set.
    pub timeout_milliseconds: Option<u64>,
}

impl PollingStrategy {
    /// Build a polling strategy from millisecond durations.
    pub fn new(interval_milliseconds: u64, timeout_milliseconds: Option<u64>) -> Self {
        #[allow(deprecated)]
        Self {
            // Left unset: the accessors read the millisecond fields. Mirroring a value back would
            // round 5_500ms down to 5s.
            interval: 0,
            timeout: None,
            interval_milliseconds: Some(interval_milliseconds),
            timeout_milliseconds,
        }
    }

    /// The refresh interval in milliseconds, falling back to the deprecated seconds field.
    pub fn interval_ms(&self) -> u64 {
        #[allow(deprecated)]
        self.interval_milliseconds
            .unwrap_or_else(|| self.interval.saturating_mul(1000))
    }

    /// The refresh timeout in milliseconds, falling back to the deprecated seconds field.
    pub fn timeout_ms(&self) -> Option<u64> {
        #[allow(deprecated)]
        self.timeout_milliseconds
            .or_else(|| self.timeout.map(|secs| secs.saturating_mul(1000)))
    }
}

impl Default for PollingStrategy {
    fn default() -> Self {
        // Expressed in the deprecated fields on purpose — see the note on the struct.
        #[allow(deprecated)]
        Self {
            interval: 60, // 1 minute
            timeout: Some(30),
            interval_milliseconds: None,
            timeout_milliseconds: None,
        }
    }
}

/// On-demand strategy configuration. Durations are milliseconds; the seconds-based `ttl` and
/// `timeout` fields still work and are deprecated. See [`PollingStrategy`] for how the two interact.
#[derive(Debug, Clone)]
pub struct OnDemandStrategy {
    #[deprecated(note = "seconds-based; use `ttl_milliseconds`")]
    pub ttl: u64,
    /// How long cached data stays fresh, in milliseconds. Wins over `ttl` when set.
    pub ttl_milliseconds: Option<u64>,
    #[deprecated(note = "seconds-based; use `timeout_milliseconds`")]
    pub timeout: Option<u64>,
    /// How long a single refresh may take before it is abandoned, in milliseconds.
    /// Wins over `timeout` when set.
    pub timeout_milliseconds: Option<u64>,
    pub use_stale_on_error: Option<bool>,
}

impl OnDemandStrategy {
    /// Build an on-demand strategy from millisecond durations.
    pub fn new(
        ttl_milliseconds: u64,
        timeout_milliseconds: Option<u64>,
        use_stale_on_error: Option<bool>,
    ) -> Self {
        #[allow(deprecated)]
        Self {
            // Left unset: the accessors read the millisecond fields. Mirroring a value back would
            // round 5_500ms down to 5s.
            ttl: 0,
            timeout: None,
            ttl_milliseconds: Some(ttl_milliseconds),
            timeout_milliseconds,
            use_stale_on_error,
        }
    }

    /// The cache TTL in milliseconds, falling back to the deprecated seconds field.
    pub fn ttl_ms(&self) -> u64 {
        #[allow(deprecated)]
        self.ttl_milliseconds
            .unwrap_or_else(|| self.ttl.saturating_mul(1000))
    }

    /// The refresh timeout in milliseconds, falling back to the deprecated seconds field.
    pub fn timeout_ms(&self) -> Option<u64> {
        #[allow(deprecated)]
        self.timeout_milliseconds
            .or_else(|| self.timeout.map(|secs| secs.saturating_mul(1000)))
    }

    /// Whether to serve stale data when a refresh fails.
    ///
    /// Read through this rather than off the field: unset means "unspecified", not "off". A call
    /// site reaching for `unwrap_or_default()` reads it as `false`, which contradicts both
    /// [`Default`] and the Java and Python clients.
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
        // Expressed in the deprecated fields on purpose — see the note on PollingStrategy.
        #[allow(deprecated)]
        Self {
            ttl: 300, // 5 minutes
            timeout: Some(30),
            ttl_milliseconds: None,
            timeout_milliseconds: None,
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

    /// The regression this guards: `Default` must not fill in the millisecond fields, or it would
    /// silently override a caller who set only the deprecated seconds field.
    #[test]
    #[allow(deprecated)]
    fn a_seconds_field_with_default_still_means_seconds() {
        let polling = PollingStrategy {
            interval: 30, // 30 seconds
            ..Default::default()
        };
        assert_eq!(polling.interval_ms(), 30_000);
        assert_eq!(polling.timeout_ms(), Some(30_000)); // default 30s

        let on_demand = OnDemandStrategy {
            ttl: 60, // 60 seconds
            ..Default::default()
        };
        assert_eq!(on_demand.ttl_ms(), 60_000);
    }

    #[test]
    fn millisecond_fields_win_over_the_deprecated_ones() {
        let polling = PollingStrategy {
            interval_milliseconds: Some(1_500),
            ..Default::default()
        };
        assert_eq!(polling.interval_ms(), 1_500);

        assert_eq!(
            PollingStrategy::new(5_500, Some(2_500)).interval_ms(),
            5_500
        );
        assert_eq!(
            PollingStrategy::new(5_500, Some(2_500)).timeout_ms(),
            Some(2_500)
        );
    }

    #[test]
    fn defaults_are_unchanged() {
        assert_eq!(PollingStrategy::default().interval_ms(), 60_000);
        assert_eq!(PollingStrategy::default().timeout_ms(), Some(30_000));
        assert_eq!(OnDemandStrategy::default().ttl_ms(), 300_000);
        assert_eq!(OnDemandStrategy::default().timeout_ms(), Some(30_000));
    }
}
