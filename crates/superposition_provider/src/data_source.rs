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

pub struct ExperimentResponse {
    pub data: Experiments,
    pub fetched_at: DateTime<Utc>,
}

impl ExperimentResponse {
    pub fn new(experiments: Experiments) -> Self {
        Self {
            data: experiments,
            fetched_at: Utc::now(),
        }
    }
}

impl Display for ExperimentResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ExperimentResponse(data: {} experiments, fetched_at: {})",
            self.data.len(),
            self.fetched_at
        )
    }
}

pub struct ExperimentGroupResponse {
    pub data: ExperimentGroups,
    pub fetched_at: DateTime<Utc>,
}

impl ExperimentGroupResponse {
    pub fn new(experiment_groups: ExperimentGroups) -> Self {
        Self {
            data: experiment_groups,
            fetched_at: Utc::now(),
        }
    }
}

impl Display for ExperimentGroupResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ExperimentGroupResponse(data: {} experiment groups, fetched_at: {})",
            self.data.len(),
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
    ) -> Result<(
        FetchResponse<ExperimentResponse>,
        FetchResponse<ExperimentGroupResponse>,
    )>;

    /// Fetch active experiments whose conditions are candidates for the given context
    /// and key prefixes.
    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<(
        FetchResponse<ExperimentResponse>,
        FetchResponse<ExperimentGroupResponse>,
    )>;

    /// Fetch active experiments that match the given context and key prefixes.
    async fn fetch_matching_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        last_fetched_at: Option<DateTime<Utc>>,
    ) -> Result<(
        FetchResponse<ExperimentResponse>,
        FetchResponse<ExperimentGroupResponse>,
    )>;

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
