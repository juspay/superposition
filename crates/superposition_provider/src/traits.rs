use async_trait::async_trait;
use open_feature::{
    provider::ResolutionDetails, EvaluationContext, EvaluationError, EvaluationErrorCode,
    EvaluationResult, StructValue,
};
use serde_json::{Map, Value};

use crate::{types::Result, utils::ConversionUtils};

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

    async fn resolve_bool(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<bool>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_bool() {
                    Some(bool_val) => Ok(ResolutionDetails::new(bool_val)),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not a boolean", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                log::error!("Error evaluating boolean flag {}: {}", flag_key, e);
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

    async fn resolve_string(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<String>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_str() {
                    Some(str_val) => Ok(ResolutionDetails::new(str_val.to_owned())),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not a string", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                log::error!("Error evaluating String flag {}: {}", flag_key, e);
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

    async fn resolve_int(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<i64>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_i64() {
                    Some(int_val) => Ok(ResolutionDetails::new(int_val)),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not an integer", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                log::error!("Error evaluating integer flag {}: {}", flag_key, e);
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

    async fn resolve_float(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<f64>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => match value.as_f64() {
                    Some(float_val) => Ok(ResolutionDetails::new(float_val)),
                    None => Err(EvaluationError {
                        code: EvaluationErrorCode::TypeMismatch,
                        message: Some(format!("Flag '{}' is not a float", flag_key)),
                    }),
                },
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                log::error!("Error evaluating float flag {}: {}", flag_key, e);
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

    async fn resolve_struct(
        &self,
        flag_key: &str,
        evaluation_context: EvaluationContext,
    ) -> EvaluationResult<ResolutionDetails<StructValue>> {
        match self.resolve_all_features(evaluation_context).await {
            Ok(config) => match config.get(flag_key) {
                Some(value) => {
                    match ConversionUtils::serde_value_to_struct_value(value) {
                        Ok(struct_value) => Ok(ResolutionDetails::new(struct_value)),
                        Err(e) => {
                            log::error!("Error converting value to StructValue: {}", e);
                            Err(EvaluationError {
                                code: EvaluationErrorCode::TypeMismatch,
                                message: Some(format!(
                                    "Flag '{}' is not a struct: {}",
                                    flag_key, e
                                )),
                            })
                        }
                    }
                }
                None => Err(EvaluationError {
                    code: EvaluationErrorCode::FlagNotFound,
                    message: Some(format!("Flag '{}' not found", flag_key)),
                }),
            },
            Err(e) => {
                log::error!("Error evaluating Object flag {}: {}", flag_key, e);
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
}
