pub mod file;
pub mod http;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde_json::{Map, Value};
use superposition_core::experiment::ExperimentGroups;
use superposition_core::Experiments;
use superposition_types::Config;

use crate::types::Result;

/// Holds a resolved configuration along with the time it was fetched.
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

/// Holds active experiments and experiment groups along with the time they were fetched.
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

/// Trait for fetching configuration and experiment data from a Superposition backend.
///
/// Implementors provide the transport mechanism (e.g. HTTP, file-based) while consumers
/// interact with this unified interface.
#[async_trait]
pub trait SuperpositionDataSource: Send + Sync {
    /// Fetch the full resolved configuration.
    async fn fetch_config(&self) -> Result<ConfigData>;

    /// Fetch a resolved configuration filtered by the given context and key prefixes.
    async fn fetch_filtered_config(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<ConfigData>;

    /// Fetch all active experiments.
    async fn fetch_active_experiments(&self) -> Result<Option<ExperimentData>>;

    /// Fetch active experiments whose conditions are candidates for the given context
    /// and key prefixes.
    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>>;

    /// Fetch active experiments that match the given context and key prefixes.
    async fn fetch_matching_active_experiments(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
    ) -> Result<Option<ExperimentData>>;

    /// Whether this data source supports experiments.
    fn supports_experiments(&self) -> bool;

    /// Clean up any resources held by this data source.
    async fn close(&self) -> Result<()>;
}
