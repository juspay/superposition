use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use crate::types::Result;
use crate::EvaluationContext;

/// Metadata for AllFeatureProvider
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AllFeatureProviderMetadata {
    pub name: String,
    pub version: String,
}

impl AllFeatureProviderMetadata {
    pub fn new(name: String, version: String) -> Self {
        Self { name, version }
    }
}

/// Experiment metadata returned by the provider
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExperimentMeta {
    pub experiment_id: String,
    pub variant_id: String,
    pub experiment_name: Option<String>,
    pub variant_name: Option<String>,
}

/// Trait for bulk configuration resolution
///
/// This trait provides methods to resolve all feature flags at once,
/// which is more efficient than resolving them one by one.
#[async_trait]
pub trait AllFeatureProvider: Send + Sync {
    /// Resolve all features for the given evaluation context
    ///
    /// Returns a map of all feature keys to their resolved values
    async fn resolve_all_features(
        &self,
        context: &EvaluationContext,
    ) -> Result<Map<String, Value>>;

    /// Resolve all features matching the given prefix filters
    ///
    /// If prefix_filter is None, behaves like resolve_all_features
    /// If prefix_filter is Some, only returns features whose keys match any of the prefixes
    async fn resolve_all_features_with_filter(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>>;

    /// Get metadata about this provider
    fn metadata(&self) -> &AllFeatureProviderMetadata;
}

/// Trait for experiment metadata and variant resolution
///
/// This trait provides methods to get information about experiments
/// and which variants are applicable for a given context.
#[async_trait]
pub trait FeatureExperimentMeta: Send + Sync {
    /// Get all applicable variant IDs for the given context
    ///
    /// This returns the list of variant IDs that should be applied
    /// based on the experiments that match the given context.
    async fn get_applicable_variants(
        &self,
        context: &EvaluationContext,
    ) -> Result<Vec<String>>;

    /// Get detailed experiment metadata for the given context
    ///
    /// This returns information about which experiments are active
    /// and which variants have been selected for the given context.
    async fn get_experiment_metadata(
        &self,
        context: &EvaluationContext,
    ) -> Result<Vec<ExperimentMeta>>;

    /// Get the variant for a specific experiment
    ///
    /// Returns None if the experiment is not applicable for the given context
    /// Returns Some(variant_id) if a variant was selected
    async fn get_experiment_variant(
        &self,
        experiment_id: &str,
        context: &EvaluationContext,
    ) -> Result<Option<String>>;
}
