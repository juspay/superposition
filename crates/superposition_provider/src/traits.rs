use async_trait::async_trait;
use open_feature::EvaluationContext;
use serde_json::{Map, Value};

use crate::types::Result;

/// Trait for bulk configuration resolution.
///
/// Implementors provide the ability to resolve all feature flags at once,
/// optionally filtered by key prefixes.
#[async_trait]
pub trait AllFeatureProvider: Send + Sync {
    /// Resolve all features for the given evaluation context.
    async fn resolve_all_features(
        &self,
        context: &EvaluationContext,
    ) -> Result<Map<String, Value>>;

    /// Resolve all features for the given evaluation context, optionally
    /// filtered to only include keys matching the provided prefixes.
    async fn resolve_all_features_with_filter(
        &self,
        context: &EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>>;
}

/// Trait for experiment variant resolution.
///
/// Implementors provide the ability to determine which experiment variants
/// are applicable for a given evaluation context.
#[async_trait]
pub trait FeatureExperimentMeta: Send + Sync {
    /// Get the list of applicable experiment variant IDs for the given context.
    async fn get_applicable_variants(
        &self,
        context: &EvaluationContext,
    ) -> Result<Vec<String>>;
}
