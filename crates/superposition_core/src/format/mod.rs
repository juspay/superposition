pub mod error;
pub mod json_impl;
pub mod toml_impl;

use std::collections::HashMap;
use superposition_types::{
    database::models::cac::DimensionType, DetailedConfig, DimensionInfo,
};

pub use error::{conversion_error, serialization_error, syntax_error, FormatError};

/// Trait for configuration format parsers/serializers
pub trait ConfigFormat {
    /// Parse a string into DetailedConfig
    fn parse(input: &str) -> Result<DetailedConfig, FormatError>;

    /// Serialize DetailedConfig to string
    fn serialize(detailed_config: DetailedConfig) -> Result<String, FormatError>;

    /// Get the format name for error messages
    fn format_name() -> &'static str;
}

/// Shared validation logic for all formats
///
/// This function validates:
/// - Default configs against their schemas
/// - Dimension schemas (including cohort dimensions)
/// - Duplicate dimension positions
/// - Contexts against declared dimensions
/// - Overrides against default configs
///
/// After validation, it:
/// - Sorts contexts by priority (based on dimension positions)
/// - Assigns correct weights
/// - Builds dependency graphs for cohort dimensions
pub fn validate_detailed_config(
    detailed: &mut DetailedConfig,
) -> Result<(), FormatError> {
    use crate::helpers::create_connections_with_dependents;
    use crate::validations;
    use serde_json::Value;

    let default_configs = &detailed.default_configs;
    let dimensions = &mut detailed.dimensions;

    // Validate default configs against their schemas
    for (k, v) in default_configs.iter() {
        validations::validate_config_value(k, &v.value, &v.schema).map_err(|errors| {
            let error = &errors[0];
            FormatError::ValidationError {
                key: format!("default-configs.{}", error.key()),
                errors: error
                    .errors()
                    .map(validations::format_validation_errors)
                    .unwrap_or_default(),
            }
        })?;
    }

    // Validate dimensions and build dependency graphs
    let mut position_to_dimensions: HashMap<i32, Vec<String>> = HashMap::new();

    for (dim, dim_info) in dimensions.clone().iter() {
        position_to_dimensions
            .entry(dim_info.position)
            .or_default()
            .push(dim.clone());

        match &dim_info.dimension_type {
            DimensionType::LocalCohort(cohort_dim) => {
                if !dimensions.contains_key(cohort_dim) {
                    return Err(FormatError::InvalidDimension(cohort_dim.clone()));
                }

                validations::validate_cohort_schema_structure(&Value::from(
                    &dim_info.schema,
                ))
                .map_err(|errors| FormatError::ValidationError {
                    key: format!("{}.schema", dim),
                    errors: validations::format_validation_errors(&errors),
                })?;

                let cohort_dimension_info = dimensions
                    .get(cohort_dim)
                    .ok_or_else(|| FormatError::InvalidDimension(cohort_dim.clone()))?;

                validations::validate_cohort_dimension_position(
                    cohort_dimension_info,
                    dim_info,
                )
                .map_err(|_| {
                    FormatError::InvalidCohortDimensionPosition {
                        dimension: dim.clone(),
                        dimension_position: dim_info.position,
                        cohort_dimension: cohort_dim.clone(),
                        cohort_dimension_position: cohort_dimension_info.position,
                    }
                })?;

                create_connections_with_dependents(cohort_dim, dim, dimensions);
            }
            DimensionType::RemoteCohort(cohort_dim) => {
                if !dimensions.contains_key(cohort_dim) {
                    return Err(FormatError::InvalidDimension(cohort_dim.clone()));
                }

                validations::validate_schema(&Value::from(&dim_info.schema)).map_err(
                    |errors| FormatError::ValidationError {
                        key: format!("{}.schema", dim),
                        errors: validations::format_validation_errors(&errors),
                    },
                )?;

                let cohort_dimension_info = dimensions
                    .get(cohort_dim)
                    .ok_or_else(|| FormatError::InvalidDimension(cohort_dim.clone()))?;

                validations::validate_cohort_dimension_position(
                    cohort_dimension_info,
                    dim_info,
                )
                .map_err(|_| {
                    FormatError::InvalidCohortDimensionPosition {
                        dimension: dim.clone(),
                        dimension_position: dim_info.position,
                        cohort_dimension: cohort_dim.clone(),
                        cohort_dimension_position: cohort_dimension_info.position,
                    }
                })?;

                create_connections_with_dependents(cohort_dim, dim, dimensions);
            }
            DimensionType::Regular {} => {
                validations::validate_schema(&Value::from(&dim_info.schema)).map_err(
                    |errors| FormatError::ValidationError {
                        key: format!("{}.schema", dim),
                        errors: validations::format_validation_errors(&errors),
                    },
                )?;
            }
        }
    }

    // Check for duplicate positions
    for (position, dims) in position_to_dimensions {
        if dims.len() > 1 {
            return Err(FormatError::DuplicatePosition {
                position,
                dimensions: dims,
            });
        }
    }

    // Validate contexts and overrides
    for (index, context) in detailed.contexts.iter().enumerate() {
        let condition = &context.condition;

        validations::validate_context(condition, dimensions).map_err(|errors| {
            let first_error = &errors[0];
            match first_error {
                validations::ContextValidationError::UndeclaredDimension {
                    dimension,
                } => FormatError::UndeclaredDimension {
                    dimension: dimension.clone(),
                    context: format!("[{}]", index),
                },
                validations::ContextValidationError::ValidationError { key, errors } => {
                    FormatError::ValidationError {
                        key: format!("context[{}]._context_.{}", index, key),
                        errors: validations::format_validation_errors(errors),
                    }
                }
                _ => FormatError::ValidationError {
                    key: format!("context[{}]._context_", index),
                    errors: format!("{} validation errors", errors.len()),
                },
            }
        })?;
    }

    for (index, context) in detailed.contexts.iter().enumerate() {
        let override_key = context.override_with_keys.get_key();
        if let Some(override_vals) = detailed.overrides.get(override_key) {
            validations::validate_overrides(override_vals, default_configs).map_err(
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
        }
    }

    // Sort contexts by priority (weight) - higher weight means higher priority
    // Weight is calculated based on dimension positions: sum of 2^position for each dimension in context
    detailed
        .contexts
        .sort_by(|a, b| b.priority.cmp(&a.priority));

    // Set correct values for weight and priority after sorting
    detailed
        .contexts
        .iter_mut()
        .enumerate()
        .for_each(|(index, ctx)| {
            ctx.weight = index as i32;
            ctx.priority = index as i32;
        });

    Ok(())
}
