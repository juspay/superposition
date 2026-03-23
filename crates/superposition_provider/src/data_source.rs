pub mod file;
pub mod http;

use std::fmt::Display;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde_json::{Map, Value};
use superposition_core::experiment::ExperimentGroups;
use superposition_core::Experiments;
use superposition_types::Config;

use crate::types::Result;

pub enum FetchResponse<T> {
    NotModified,
    Data(T),
}

impl<T> FetchResponse<T> {
    pub fn is_not_modified(&self) -> bool {
        matches!(self, FetchResponse::NotModified)
    }

    pub fn data(&self) -> Option<&T> {
        match self {
            FetchResponse::Data(data) => Some(data),
            FetchResponse::NotModified => None,
        }
    }

    pub fn into_data(self) -> Option<T> {
        match self {
            FetchResponse::Data(data) => Some(data),
            FetchResponse::NotModified => None,
        }
    }

    pub fn map_data<U, F: FnOnce(T) -> U>(self, f: F) -> FetchResponse<U> {
        match self {
            FetchResponse::Data(data) => FetchResponse::Data(f(data)),
            FetchResponse::NotModified => FetchResponse::NotModified,
        }
    }
}

impl<T: Display> Display for FetchResponse<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FetchResponse::NotModified => write!(f, "NotModified"),
            FetchResponse::Data(data) => write!(f, "Data({})", data),
        }
    }
}

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

impl Display for ConfigData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ConfigData(fetched_at: {}, config.contexts: {}, config.overrides: {}, config.default_configs: {}, config.dimensions: {})",
            self.fetched_at,
            self.config.contexts.len(),
            self.config.overrides.len(),
            self.config.default_configs.len(),
            self.config.dimensions.len()
        )
    }
}

pub struct ExperimentResponse {
    pub experiments: FetchResponse<Experiments>,
    pub experiment_groups: FetchResponse<ExperimentGroups>,
    pub fetched_at: DateTime<Utc>,
}

impl Display for ExperimentResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ExperimentResponse(data: {} experiments, {} experiment groups, fetched_at: {})",
            self.experiments.data().map(|e| e.len()).unwrap_or_default(),
            self.experiment_groups.data().map(|e| e.len()).unwrap_or_default(),
            self.fetched_at
        )
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
    pub fn update_with(mut self, new_value: ExperimentResponse) -> Self {
        if let Some(exps) = new_value.experiments.into_data() {
            self.experiments = exps
        }

        if let Some(grps) = new_value.experiment_groups.into_data() {
            self.experiment_groups = grps
        }

        self.fetched_at = new_value.fetched_at;

        self
    }
}

impl TryFrom<ExperimentResponse> for ExperimentData {
    type Error = String;

    fn try_from(value: ExperimentResponse) -> std::result::Result<Self, Self::Error> {
        match (
            value.experiments.into_data(),
            value.experiment_groups.into_data(),
        ) {
            (Some(experiments), Some(experiment_groups)) => Ok(Self {
                experiments,
                experiment_groups,
                fetched_at: value.fetched_at,
            }),
            (None, None) => Err("ExperimentResponse contains no data".to_string()),
            (None, Some(_)) => {
                Err("ExperimentResponse missing experiments data".to_string())
            }
            (Some(_), None) => {
                Err("ExperimentResponse missing experiment groups data".to_string())
            }
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
    async fn fetch_config(
        &self,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>>;

    /// Fetch a resolved configuration filtered by the given context and key prefixes.
    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>>;

    /// Fetch all active experiments.
    async fn fetch_active_experiments(
        &self,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<ExperimentResponse>;

    /// Fetch active experiments whose conditions are candidates for the given context
    /// and key prefixes.
    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<ExperimentResponse>;

    /// Fetch active experiments that match the given context and key prefixes.
    async fn fetch_matching_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<ExperimentResponse>;

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
