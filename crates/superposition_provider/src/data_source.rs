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

    /// Filter experiments by context using a matcher function (apply or partial_apply),
    /// and optionally by prefix.
    pub fn filter(
        &self,
        context: Option<&Map<String, Value>>,
        prefix_filter: Option<&[String]>,
        matcher: fn(&Map<String, Value>, &Map<String, Value>) -> bool,
    ) -> ExperimentData {
        let mut filtered_experiments = self.experiments.clone();

        // Filter by context using the provided matcher
        if let Some(ctx) = context {
            if !ctx.is_empty() {
                filtered_experiments.retain(|exp| matcher(&exp.context, ctx));
            }
        }

        // Filter by prefix: keep experiments where any variant has an override key
        // starting with any of the prefixes
        if let Some(prefixes) = prefix_filter {
            if !prefixes.is_empty() {
                filtered_experiments.retain(|exp| {
                    exp.variants.iter().any(|variant| {
                        let overrides = variant.overrides.clone().into_inner();
                        overrides.keys().any(|key| {
                            prefixes.iter().any(|prefix| key.starts_with(prefix))
                        })
                    })
                });
            }
        }

        // Keep all groups (they reference experiments by ID)
        ExperimentData::new(filtered_experiments, self.experiment_groups.clone())
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

    /// Set up a file watcher and return a stream of change notifications.
    ///
    /// Returns `Ok(None)` if this data source does not support watching.
    fn watch(&self) -> Result<Option<crate::types::WatchStream>> {
        Ok(None)
    }

    /// Clean up any resources held by this data source.
    async fn close(&self) -> Result<()>;
}
