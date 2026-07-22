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
        prefix_filter: Option<Vec<String>>,
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
    ) -> Result<Map<String, Value>> {
        self.resolve_all_features_with_filter(context, None).await
    }

    /// Resolve all features for the given evaluation context, optionally
    /// filtered to only include keys matching the provided prefixes.
    async fn resolve_all_features_with_filter(
        &self,
        context: EvaluationContext,
        prefix_filter: Option<Vec<String>>,
    ) -> Result<Map<String, Value>>;

    /// Resolve a flag and extract it as `T`.
    ///
    /// Error reasons need no handling here: the failure paths return `EvaluationError`, which has
    /// no `reason` field, and the SDK stamps `EvaluationReason::Error` on the details it builds
    /// from it. The Java and Python providers return the details object themselves, so they set it
    /// explicitly — same outcome, different seam.
    ///
    /// TODO: successful resolutions leave `reason` unset. Reporting it accurately (STATIC for a
    /// default-config value, TARGETING_MATCH for a context override, SPLIT for an experiment
    /// variant) needs `eval_config` in `superposition_core` to say, per key, where the value came
    /// from. Until it does, guessing would be worse than saying nothing — a flag no experiment
    /// touched would still be labelled SPLIT. The same TODO applies to the Java and Python clients.
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

    /// Resolve a flag whose value is a JSON array.
    ///
    /// `resolve_struct` cannot return one: OpenFeature models an object flag as a
    /// `StructValue`, which has no array form, so a top-level array is a TypeMismatch there.
    /// (An array *nested inside* an object flag is fine and needs no special handling.)
    /// This is the typed way to read one; the alternative is `resolve_all_features`, which
    /// hands back the raw `serde_json::Value`.
    ///
    /// The Java and Python clients return top-level arrays from their object accessor
    /// directly, because their SDKs' object type admits one. This method exists to close
    /// that gap, not to add a capability the other clients lack.
    async fn resolve_array(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<Vec<open_feature::Value>>> {
        self.resolve_typed(flag_key, evaluation_context, "array", |v| match v {
            Value::Array(items) => items
                .into_iter()
                .map(|item| conversions::value_to_openfeature_value(item).ok())
                .collect(),
            _ => None,
        })
        .await
    }
}
