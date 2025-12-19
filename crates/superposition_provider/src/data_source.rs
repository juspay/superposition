use async_trait::async_trait;
use chrono::{DateTime, Utc};
use superposition_core::experiment::ExperimentGroups;
use superposition_core::Experiments;
use superposition_types::Config;

use crate::types::Result;

mod http;

pub use http::HttpDataSource;

/// Data fetched from a configuration source
#[derive(Debug, Clone)]
pub struct ConfigData {
    pub config: Config,
    pub fetched_at: DateTime<Utc>,
}

impl ConfigData {
    pub fn new(config: Config) -> Self {
        Self {
            config,
            fetched_at: Utc::now(),
        }
    }
}

/// Experiment data fetched from a source
#[derive(Debug, Clone)]
pub struct ExperimentData {
    pub experiments: Experiments,
    pub experiment_groups: ExperimentGroups,
    pub fetched_at: DateTime<Utc>,
}

impl ExperimentData {
    pub fn new(experiments: Experiments, experiment_groups: ExperimentGroups) -> Self {
        Self {
            experiments,
            experiment_groups,
            fetched_at: Utc::now(),
        }
    }
}

/// Trait for abstracting data sources for Superposition configuration and experiments
///
/// This trait allows plugging different data sources (HTTP, File, Redis, etc.)
/// into the Superposition provider system.
#[async_trait]
pub trait SuperpositionDataSource: Send + Sync {
    /// Fetch the latest configuration from the data source
    async fn fetch_config(&self) -> Result<ConfigData>;

    /// Fetch experiment data from the data source
    ///
    /// Returns None if the data source doesn't support experiments
    async fn fetch_experiments(&self) -> Result<Option<ExperimentData>>;

    /// Get a human-readable name for this data source
    fn source_name(&self) -> &str;

    /// Check if this data source supports experiments
    fn supports_experiments(&self) -> bool;

    /// Close and cleanup resources used by this data source
    async fn close(&self) -> Result<()>;
}
