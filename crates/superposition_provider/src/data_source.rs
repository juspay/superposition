pub mod file;
pub mod http;

use std::fmt::Display;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde_json::{Map, Value};
use superposition_core::experiment::ExperimentConfig;
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
    pub data: Config,
    pub fetched_at: DateTime<Utc>,
}

impl Display for ConfigData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ConfigData(fetched_at: {}, data.contexts: {}, data.overrides: {}, data.default_configs: {}, data.dimensions: {})",
            self.fetched_at,
            self.data.contexts.len(),
            self.data.overrides.len(),
            self.data.default_configs.len(),
            self.data.dimensions.len()
        )
    }
}

/// Holds active experiments and experiment groups along with the time they were fetched.
#[derive(Debug, Clone)]
pub struct ExperimentData {
    pub data: ExperimentConfig,
    pub fetched_at: DateTime<Utc>,
}

impl Display for ExperimentData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ExperimentData(data: {} experiments, {} experiment groups, fetched_at: {})",
            self.data.experiments.len(),
            self.data.experiment_groups.len(),
            self.fetched_at
        )
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
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>> {
        self.fetch_filtered_config(None, None, if_modified_since)
            .await
    }

    /// Fetch a resolved configuration filtered by the given context and key prefixes.
    async fn fetch_filtered_config(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ConfigData>>;

    /// Fetch all active experiments.
    async fn fetch_active_experiments(
        &self,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>>;

    /// Fetch active experiments whose conditions are candidates for the given context
    /// and key prefixes.
    async fn fetch_candidate_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>>;

    /// Fetch active experiments that match the given context and key prefixes.
    async fn fetch_matching_active_experiments(
        &self,
        context: Option<Map<String, Value>>,
        prefix_filter: Option<Vec<String>>,
        if_modified_since: Option<DateTime<Utc>>,
    ) -> Result<FetchResponse<ExperimentData>>;

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
