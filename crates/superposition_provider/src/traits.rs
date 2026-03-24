use async_trait::async_trait;
use open_feature::{
    provider::ResolutionDetails, EvaluationContext, EvaluationError, EvaluationErrorCode,
    EvaluationResult, StructValue,
};
use serde_json::{Map, Value};

use crate::{conversions, types::Result};

/// Trait for experiment variant resolution.
///
/// Implementors provide the ability to determine which experiment variants
/// are applicable for a given evaluation context.
#[async_trait]
pub trait FeatureExperimentMeta: Send + Sync {
    /// Get the list of applicable experiment variant IDs for the given context.
    async fn get_applicable_variants(
        &self,
        context: EvaluationContext,
    ) -> Result<Vec<String>>;
}

/// Trait for bulk configuration resolution.
///
/// Implementors provide the ability to resolve all feature flags at once,
/// optionally filtered by key prefixes.
#[async_trait]
pub trait AllFeatureProvider: Send + Sync {
    /// Resolve all features for the given evaluation context.
    async fn resolve_all_features(
        &self,
        context: EvaluationContext,
    ) -> Result<Map<String, Value>>;

    /// Resolve all features for the given evaluation context, optionally
    /// filtered to only include keys matching the provided prefixes.
    async fn resolve_all_features_with_filter(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<&[String]>,
    ) -> Result<Map<String, Value>>;

    async fn resolve_typed<T: Send + Sync>(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
        type_name: &str,
        extractor: impl Fn(Value) -> Option<T> + Send + Sync,
    ) -> EvaluationResult<ResolutionDetails<T>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(mut config) => {
                match config.remove(flag_key) {
                    Some(value) => extractor(value)
                        .map(ResolutionDetails::new)
                        .ok_or_else(|| EvaluationError {
                            code: EvaluationErrorCode::TypeMismatch,
                            message: Some(format!(
                                "Flag '{flag_key}' is not a {type_name}",
                            )),
                        }),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::FlagNotFound,
                        message: Some(format!("Flag '{}' not found", flag_key)),
                    }),
                }
            }
            Err(e) => {
                log::error!("Error evaluating {} flag {}: {}", type_name, flag_key, e);
                Err(EvaluationError {
                    code: EvaluationErrorCode::General(format!(
                        "Error evaluating flag '{}': {}",
                        flag_key, e
                    )),
                    message: Some(format!("Error evaluating flag '{}': {}", flag_key, e)),
                })
            }
        }
    }

    async fn resolve_bool(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        self.resolve_typed(flag_key, evaluation_context, "boolean", |v| v.as_bool())
            .await
    }

    async fn resolve_string(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        self.resolve_typed(flag_key, evaluation_context, "string", |v| match v {
            Value::String(s) => Some(s),
            _ => None,
        })
        .await
    }

    async fn resolve_int(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        self.resolve_typed(flag_key, evaluation_context, "integer", |v| v.as_i64())
            .await
    }

    async fn resolve_float(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        self.resolve_typed(flag_key, evaluation_context, "float", |v| v.as_f64())
            .await
    }

    async fn resolve_struct(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        self.resolve_typed(flag_key, evaluation_context, "struct", |v| {
            conversions::value_to_struct(v).ok()
        })
        .await
    }
}
