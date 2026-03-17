//! Unified configuration format support
//!
//! This module provides parsers and serializers for multiple configuration
//! formats (TOML and JSON) with shared validation logic.

pub mod error;
pub mod json;
#[cfg(test)]
pub mod tests;
pub mod toml;

use std::collections::HashMap;

use serde_json::Value;
use superposition_types::{
    database::models::cac::DimensionType, Condition, Config, DefaultConfigsWithSchema,
    DetailedConfig, DimensionInfo, Overrides,
};

use crate::{
    helpers::{build_context, create_connections_with_dependents},
    validations,
};
pub use error::FormatError;

/// Trait for configuration format parsers/serializers
pub trait ConfigFormat {
    /// Parse a string into DetailedConfig
    fn parse_into_detailed(input: &str) -> Result<DetailedConfig, FormatError>;

    fn parse_config(input: &str) -> Result<Config, FormatError> {
        Ok(Config::from(Self::parse_into_detailed(input)?))
    }

    /// Serialize DetailedConfig to string
    fn serialize(detailed_config: DetailedConfig) -> Result<String, FormatError>;

    /// Get the format name for error messages
    fn format_name() -> &'static str;

    /// Helper to create syntax error for a specific format
    fn syntax_error(message: impl Into<String>) -> FormatError {
        FormatError::SyntaxError {
            format: Self::format_name().to_string(),
            message: message.into(),
        }
    }

    /// Helper to create conversion error for a specific format
    fn conversion_error(message: impl Into<String>) -> FormatError {
        FormatError::ConversionError {
            format: Self::format_name().to_string(),
            message: message.into(),
        }
    }

    /// Helper to create serialization error for a specific format
    fn serialization_error(message: impl Into<String>) -> FormatError {
        FormatError::SerializationError {
            format: Self::format_name().to_string(),
            message: message.into(),
        }
    }

    fn try_into_detailed<T, F>(
        default_configs: DefaultConfigsWithSchema,
        mut dimensions: HashMap<String, DimensionInfo>,
        overrides: Vec<T>,
        split_overrides: F,
    ) -> Result<DetailedConfig, FormatError>
    where
        F: Fn(T) -> Result<(Condition, Overrides), FormatError>,
    {
        let mut modified_overrides = HashMap::new();
        let mut contexts = Vec::new();

        // Default configs validation
        for (k, v) in default_configs.iter() {
            validations::validate_config_value(k, &v.value, &v.schema).map_err(
                |errors| {
                    let error = &errors[0];
                    FormatError::ValidationError {
                        key: format!("default-configs.{}", error.key()),
                        errors: error
                            .errors()
                            .map(validations::format_validation_errors)
                            .unwrap_or_default(),
                    }
                },
            )?;
        }

        // Dimensions validation and dependency graph construction
        let mut position_to_dimensions: HashMap<i32, Vec<String>> = HashMap::new();
        for (dim, dim_info) in dimensions.clone().into_iter() {
            position_to_dimensions
                .entry(dim_info.position)
                .or_default()
                .push(dim.clone());

            match dim_info.dimension_type {
                DimensionType::LocalCohort(ref cohort_dim) => {
                    if !dimensions.contains_key(cohort_dim) {
                        return Err(FormatError::InvalidDimension(cohort_dim.clone()));
                    }

                    validations::validate_cohort_schema_structure(&Value::from(
                        &dim_info.schema,
                    ))
                    .map_err(|errors| {
                        FormatError::ValidationError {
                            key: format!("{}.schema", dim),
                            errors: validations::format_validation_errors(&errors),
                        }
                    })?;

                    let cohort_dimension_info =
                        dimensions.get(cohort_dim).ok_or_else(|| {
                            FormatError::InvalidDimension(cohort_dim.clone())
                        })?;

                    validations::validate_cohort_dimension_position(
                        cohort_dimension_info,
                        &dim_info,
                    )
                    .map_err(|_| {
                        FormatError::InvalidCohortDimensionPosition {
                            dimension: dim.clone(),
                            dimension_position: dim_info.position,
                            cohort_dimension: cohort_dim.to_string(),
                            cohort_dimension_position: cohort_dimension_info.position,
                        }
                    })?;

                    create_connections_with_dependents(cohort_dim, &dim, &mut dimensions);
                }
                DimensionType::RemoteCohort(ref cohort_dim) => {
                    if !dimensions.contains_key(cohort_dim) {
                        return Err(FormatError::InvalidDimension(cohort_dim.clone()));
                    }

                    validations::validate_schema(&Value::from(&dim_info.schema))
                        .map_err(|errors| FormatError::ValidationError {
                            key: format!("{}.schema", dim),
                            errors: validations::format_validation_errors(&errors),
                        })?;

                    let cohort_dimension_info =
                        dimensions.get(cohort_dim).ok_or_else(|| {
                            FormatError::InvalidDimension(cohort_dim.clone())
                        })?;

                    validations::validate_cohort_dimension_position(
                        cohort_dimension_info,
                        &dim_info,
                    )
                    .map_err(|_| {
                        FormatError::InvalidCohortDimensionPosition {
                            dimension: dim.clone(),
                            dimension_position: dim_info.position,
                            cohort_dimension: cohort_dim.to_string(),
                            cohort_dimension_position: cohort_dimension_info.position,
                        }
                    })?;

                    create_connections_with_dependents(cohort_dim, &dim, &mut dimensions);
                }
                DimensionType::Regular {} => {
                    validations::validate_schema(&Value::from(&dim_info.schema))
                        .map_err(|errors| FormatError::ValidationError {
                            key: format!("{}.schema", dim),
                            errors: validations::format_validation_errors(&errors),
                        })?;
                }
            }
        }

        // Check for duplicate positions
        for (position, dimensions) in position_to_dimensions {
            if dimensions.len() > 1 {
                return Err(FormatError::DuplicatePosition {
                    position,
                    dimensions,
                });
            }
        }

        // Context and override generation with validation
        for (index, ctx) in overrides.into_iter().enumerate() {
            let (condition, override_vals) = split_overrides(ctx)?;

            validations::validate_context(&condition, &dimensions).map_err(|errors| {
                let first_error = &errors[0];
                match first_error {
                    validations::ContextValidationError::UndeclaredDimension {
                        dimension,
                    } => FormatError::UndeclaredDimension {
                        dimension: dimension.clone(),
                        context: format!("[{}]", index),
                    },
                    validations::ContextValidationError::ValidationError {
                        key,
                        errors,
                    } => FormatError::ValidationError {
                        key: format!("context[{}]._context_.{}", index, key),
                        errors: validations::format_validation_errors(errors),
                    },
                    _ => FormatError::ValidationError {
                        key: format!("context[{}]._context_", index),
                        errors: format!("{} validation errors", errors.len()),
                    },
                }
            })?;
            validations::validate_overrides(&override_vals, &default_configs).map_err(
                |errors| {
                    let first_error = &errors[0];
                    match first_error {
                        validations::ContextValidationError::InvalidOverrideKey {
                            key,
                        } => FormatError::InvalidOverrideKey {
                            key: key.clone(),
                            context: format!("[{}]", index),
                        },
                        validations::ContextValidationError::ValidationError {
                            key,
                            errors,
                        } => FormatError::ValidationError {
                            key: format!("context[{}].{}", index, key),
                            errors: validations::format_validation_errors(errors),
                        },
                        _ => FormatError::ValidationError {
                            key: format!("context[{}]", index),
                            errors: format!("{} validation errors", errors.len()),
                        },
                    }
                },
            )?;

            let (context, override_hash, override_vals) =
                build_context(condition, override_vals, &dimensions).map_err(|e| {
                    Self::conversion_error(format!("Failed to build context: {}", e))
                })?;

            modified_overrides.insert(override_hash, override_vals);
            contexts.push(context);
        }

        // Sort contexts by priority (weight) - higher weight means higher priority
        contexts.sort_by(|a, b| a.priority.cmp(&b.priority));

        // Set correct values for weight and priority after sorting
        contexts.iter_mut().enumerate().for_each(|(index, ctx)| {
            ctx.weight = index as i32;
            ctx.priority = index as i32;
        });

        Ok(DetailedConfig {
            default_configs,
            dimensions,
            contexts,
            overrides: modified_overrides,
        })
    }
}
