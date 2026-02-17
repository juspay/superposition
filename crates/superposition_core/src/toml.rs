mod helpers;
#[cfg(test)]
mod test;

use std::{
    collections::{BTreeMap, HashMap},
    fmt,
    ops::Deref,
    str::FromStr,
};

use serde::{Deserialize, Serialize};
use serde_json::Value;
use superposition_types::database::models::cac::{DependencyGraph, DimensionType};
use superposition_types::{
    Config, Context, DefaultConfigsWithSchema, DetailedConfig, DimensionInfo,
    ExtendedMap, Overrides,
};
use toml::Value as TomlValue;

use crate::{
    helpers::{build_context, create_connections_with_dependents},
    toml::helpers::{
        format_key, format_toml_value, toml_to_json, try_condition_from_toml,
        try_overrides_from_toml,
    },
    validations,
};

/// Detailed error type for TOML parsing and serialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TomlError {
    TomlSyntaxError(String),
    InvalidDimension(String),
    InvalidCohortDimensionPosition {
        dimension: String,
        dimension_position: i32,
        cohort_dimension: String,
        cohort_dimension_position: i32,
    },
    UndeclaredDimension {
        dimension: String,
        context: String,
    },
    InvalidOverrideKey {
        key: String,
        context: String,
    },
    DuplicatePosition {
        position: i32,
        dimensions: Vec<String>,
    },
    ConversionError(String),
    SerializationError(String),
    ValidationError {
        key: String,
        errors: String,
    },
}

impl fmt::Display for TomlError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidCohortDimensionPosition {
                dimension,
                dimension_position,
                cohort_dimension,
                cohort_dimension_position,
            }  => { write!(
                f,
                "TOML validation error: Dimension {} position {} should be greater than cohort dimension {} position {}",
                dimension, dimension_position, cohort_dimension, cohort_dimension_position
            )},
            Self::UndeclaredDimension {
                dimension,
                context,
            } => write!(
                f,
                "TOML parsing error: Undeclared dimension '{}' used in context '{}'",
                dimension, context
            ),
            Self::InvalidOverrideKey { key, context } => write!(
                f,
                "TOML parsing error: Override key '{}' not found in default-config (context: '{}')",
                key, context
            ),
            Self::DuplicatePosition {
                position,
                dimensions,
            } => write!(
                f,
                "TOML parsing error: Duplicate position '{}' found in dimensions: {}",
                position,
                dimensions.join(", ")
            ),
            Self::TomlSyntaxError(e) => write!(f, "TOML syntax error: {}", e),
            Self::ConversionError(e) => write!(f, "TOML conversion error: {}", e),
            Self::SerializationError(msg) => write!(f, "TOML serialization error: {}", msg),
            Self::InvalidDimension(d) => write!(f, "Dimension does not exist: {}", d),
            Self::ValidationError { key, errors } => {
                write!(f, "Schema validation failed for key '{}': {}", key, errors)
            }
        }
    }
}

impl std::error::Error for TomlError {}

#[derive(Serialize, Deserialize)]
pub struct DimensionInfoToml {
    pub position: i32,
    pub schema: toml::Table,
    #[serde(rename = "type", default = "dim_type_default")]
    pub dimension_type: String,
}

fn dim_type_default() -> String {
    DimensionType::default().to_string()
}

impl TryFrom<DimensionInfo> for DimensionInfoToml {
    type Error = TomlError;
    fn try_from(d: DimensionInfo) -> Result<Self, Self::Error> {
        let schema = toml::Table::try_from(d.schema.into_inner()).map_err(|e| {
            TomlError::ConversionError(format!(
                "Schema contains values incompatible with TOML: {}",
                e
            ))
        })?;
        Ok(Self {
            position: d.position,
            schema,
            dimension_type: d.dimension_type.to_string(),
        })
    }
}

impl TryFrom<DimensionInfoToml> for DimensionInfo {
    type Error = TomlError;
    fn try_from(d: DimensionInfoToml) -> Result<Self, Self::Error> {
        let schema_json = toml_to_json(TomlValue::Table(d.schema));
        let schema_map = match schema_json {
            Value::Object(map) => map,
            _ => {
                return Err(TomlError::ConversionError(
                    "Schema must be an object".to_string(),
                ))
            }
        };
        Ok(Self {
            position: d.position,
            schema: ExtendedMap::from(schema_map),
            dimension_type: DimensionType::from_str(&d.dimension_type)
                .map_err(TomlError::ConversionError)?,
            dependency_graph: DependencyGraph(HashMap::new()),
            value_compute_function_name: None,
        })
    }
}

#[derive(Serialize, Deserialize)]
struct ContextToml {
    #[serde(rename = "_context_")]
    context: toml::Table,
    #[serde(flatten)]
    overrides: toml::Table,
}

impl TryFrom<(Context, &HashMap<String, Overrides>)> for ContextToml {
    type Error = TomlError;
    fn try_from(
        (context, overrides): (Context, &HashMap<String, Overrides>),
    ) -> Result<Self, Self::Error> {
        let context_toml: toml::Table =
            toml::Table::try_from(context.condition.deref().clone())
                .map_err(|e| TomlError::ConversionError(e.to_string()))?;
        let overrides_toml: toml::Table =
            toml::Table::try_from(overrides.get(context.override_with_keys.get_key()))
                .map_err(|e| TomlError::ConversionError(e.to_string()))?;

        Ok(Self {
            context: context_toml,
            overrides: overrides_toml,
        })
    }
}

#[derive(Serialize, Deserialize)]
struct DetailedConfigToml {
    #[serde(rename = "default-configs")]
    default_configs: DefaultConfigsWithSchema,
    dimensions: BTreeMap<String, DimensionInfoToml>,
    overrides: Vec<ContextToml>,
}

impl DetailedConfigToml {
    fn emit_default_configs(
        default_configs: DefaultConfigsWithSchema,
    ) -> Result<String, TomlError> {
        let mut out = String::new();
        out.push_str("[default-configs]\n");

        for (k, v) in default_configs.into_inner() {
            let v_toml = TomlValue::try_from(v).map_err(|e| {
                TomlError::SerializationError(format!(
                    "Failed to serialize default-config '{}': {}",
                    k, e
                ))
            })?;

            let v_str = format_toml_value(&v_toml);
            out.push_str(&format!("{} = {}\n", format_key(&k), v_str));
        }

        out.push('\n');
        Ok(out)
    }

    fn emit_dimensions(
        dimensions: BTreeMap<String, DimensionInfoToml>,
    ) -> Result<String, TomlError> {
        let mut out = String::new();
        out.push_str("[dimensions]\n");

        for (k, v) in dimensions {
            let v_toml = TomlValue::try_from(v).map_err(|e| {
                TomlError::SerializationError(format!(
                    "Failed to serialize dimension '{}': {}",
                    k, e
                ))
            })?;
            let v_str = format_toml_value(&v_toml);
            out.push_str(&format!("{} = {}\n", format_key(&k), v_str));
        }

        out.push('\n');
        Ok(out)
    }

    fn emit_overrides(ctx: ContextToml) -> Result<String, TomlError> {
        let mut out = String::new();
        out.push_str("[[overrides]]\n");

        // Serialize the _context_ field as an inline table
        let context_str = format_toml_value(&TomlValue::Table(ctx.context));
        out.push_str(&format!("_context_ = {}\n", context_str));

        // Serialize overrides
        for (k, v) in ctx.overrides {
            let v_str = format_toml_value(&v);
            out.push_str(&format!("{} = {}\n", format_key(&k), v_str));
        }

        out.push('\n');
        Ok(out)
    }

    pub fn serialize_to_toml(self) -> Result<String, TomlError> {
        let mut out = String::new();

        out.push_str(&Self::emit_default_configs(self.default_configs)?);
        out.push('\n');

        out.push_str(&Self::emit_dimensions(self.dimensions)?);
        out.push('\n');

        for ctx in self.overrides {
            out.push_str(&Self::emit_overrides(ctx)?);
        }

        out.push('\n');
        Ok(out)
    }
}

impl TryFrom<DetailedConfig> for DetailedConfigToml {
    type Error = TomlError;
    fn try_from(d: DetailedConfig) -> Result<Self, Self::Error> {
        Ok(Self {
            default_configs: d.default_configs,
            dimensions: d
                .dimensions
                .into_iter()
                .map(|(k, v)| DimensionInfoToml::try_from(v).map(|dim| (k, dim)))
                .collect::<Result<BTreeMap<_, _>, _>>()?,
            overrides: d
                .contexts
                .into_iter()
                .map(|c| ContextToml::try_from((c, &d.overrides)))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

impl TryFrom<DetailedConfigToml> for DetailedConfig {
    type Error = TomlError;
    fn try_from(d: DetailedConfigToml) -> Result<Self, Self::Error> {
        let default_configs = d.default_configs;
        let mut overrides = HashMap::new();
        let mut contexts = Vec::new();
        let mut dimensions = d
            .dimensions
            .into_iter()
            .map(|(k, v)| v.try_into().map(|dim_info| (k, dim_info)))
            .collect::<Result<HashMap<_, DimensionInfo>, TomlError>>()?;

        // Default configs validation
        for (k, v) in default_configs.iter() {
            validations::validate_config_value(k, &v.value, &v.schema).map_err(
                |errors| {
                    let error = &errors[0];
                    TomlError::ValidationError {
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
                        return Err(TomlError::InvalidDimension(cohort_dim.clone()));
                    }

                    validations::validate_cohort_schema_structure(&Value::from(
                        &dim_info.schema,
                    ))
                    .map_err(|errors| {
                        TomlError::ValidationError {
                            key: format!("{}.schema", dim),
                            errors: validations::format_validation_errors(&errors),
                        }
                    })?;

                    let cohort_dimension_info = dimensions
                        .get(cohort_dim)
                        .ok_or_else(|| TomlError::InvalidDimension(cohort_dim.clone()))?;

                    validations::validate_cohort_dimension_position(
                        cohort_dimension_info,
                        &dim_info,
                    )
                    .map_err(|_| {
                        TomlError::InvalidCohortDimensionPosition {
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
                        return Err(TomlError::InvalidDimension(cohort_dim.clone()));
                    }

                    validations::validate_schema(&Value::from(&dim_info.schema))
                        .map_err(|errors| TomlError::ValidationError {
                            key: format!("{}.schema", dim),
                            errors: validations::format_validation_errors(&errors),
                        })?;

                    let cohort_dimension_info = dimensions
                        .get(cohort_dim)
                        .ok_or_else(|| TomlError::InvalidDimension(cohort_dim.clone()))?;

                    validations::validate_cohort_dimension_position(
                        cohort_dimension_info,
                        &dim_info,
                    )
                    .map_err(|_| {
                        TomlError::InvalidCohortDimensionPosition {
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
                        .map_err(|errors| TomlError::ValidationError {
                            key: format!("{}.schema", dim),
                            errors: validations::format_validation_errors(&errors),
                        })?;
                }
            }
        }

        // Check for duplicate positions
        for (position, dimensions) in position_to_dimensions {
            if dimensions.len() > 1 {
                return Err(TomlError::DuplicatePosition {
                    position,
                    dimensions,
                });
            }
        }

        // Context and override generation with validation
        for (index, ctx) in d.overrides.into_iter().enumerate() {
            let condition = try_condition_from_toml(ctx.context)?;
            let override_vals = try_overrides_from_toml(ctx.overrides)?;

            validations::validate_context(&condition, &dimensions).map_err(|errors| {
                let first_error = &errors[0];
                match first_error {
                    validations::ContextValidationError::UndeclaredDimension {
                        dimension,
                    } => TomlError::UndeclaredDimension {
                        dimension: dimension.clone(),
                        context: format!("[{}]", index),
                    },
                    validations::ContextValidationError::ValidationError {
                        key,
                        errors,
                    } => TomlError::ValidationError {
                        key: format!("context[{}]._context_.{}", index, key),
                        errors: validations::format_validation_errors(errors),
                    },
                    _ => TomlError::ValidationError {
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
                        } => TomlError::InvalidOverrideKey {
                            key: key.clone(),
                            context: format!("[{}]", index),
                        },
                        validations::ContextValidationError::ValidationError {
                            key,
                            errors,
                        } => TomlError::ValidationError {
                            key: format!("context[{}].{}", index, key),
                            errors: validations::format_validation_errors(errors),
                        },
                        _ => TomlError::ValidationError {
                            key: format!("context[{}]", index),
                            errors: format!("{} validation errors", errors.len()),
                        },
                    }
                },
            )?;

            let (context, override_hash, override_vals) =
                build_context(condition, override_vals, &dimensions)
                    .map_err(TomlError::ConversionError)?;

            overrides.insert(override_hash, override_vals);
            contexts.push(context);
        }

        // Sort contexts by priority (weight) - higher weight means higher priority
        contexts.sort_by(|a, b| b.priority.cmp(&a.priority));

        // Set correct values for weight and priority after sorting
        contexts.iter_mut().enumerate().for_each(|(index, ctx)| {
            ctx.weight = index as i32;
            ctx.priority = index as i32;
        });

        Ok(Self {
            default_configs,
            dimensions,
            contexts,
            overrides,
        })
    }
}

/// Parse TOML configuration string into structured components
///
/// This function parses a TOML string containing default-config, dimensions, and context sections,
/// and returns the parsed structures that can be used with other superposition_core functions.
///
/// # Arguments
/// * `toml_content` - TOML string containing default-config, dimensions, and context sections
///
/// # Returns
/// * `Ok(Config)` - Successfully parsed configuration with:
///   - `default_config`: Map of configuration keys to values
///   - `contexts`: Vector of context conditions
///   - `overrides`: HashMap of override configurations
///   - `dimensions`: HashMap of dimension information
/// * `Err(TomlError)` - Detailed error about what went wrong
///
/// # Example TOML Format
/// ```toml
/// [default_configs]
/// timeout = { value = 30, schema = { type = "integer" } }
/// enabled = { value = true, schema = { type = "boolean" } }
///
/// [dimensions]
/// os = { schema = { type = "string" } }
/// region = { schema = { type = "string" } }
///
/// [context]
/// "os=linux" = { timeout = 60 }
/// "os=linux;region=us-east" = { timeout = 90, enabled = false }
/// ```
///
/// # Example Usage
/// ```rust,no_run
/// use superposition_core::parse_toml_config;
///
/// let toml_content = r#"
///     [default_configs]
///     timeout = { value = 30, schema = { type = "integer" } }
///
///     [dimensions]
///     os = { schema = { type = "string" } }
///
///     [context]
///     "os=linux" = { timeout = 60 }
/// "#;
///
/// let parsed = parse_toml_config(toml_content)?;
/// println!("Parsed {} contexts", parsed.contexts.len());
/// # Ok::<(), superposition_core::TomlError>(())
/// ```
pub fn parse_toml_config(toml_str: &str) -> Result<Config, TomlError> {
    let detailed_toml_config = toml::from_str::<DetailedConfigToml>(toml_str)
        .map_err(|e| TomlError::TomlSyntaxError(e.to_string()))?;
    let detailed_config = DetailedConfig::try_from(detailed_toml_config)?;
    let config = Config::from(detailed_config);

    Ok(config)
}

/// Serialize DetailedConfig structure to TOML format
///
/// Converts a DetailedConfig object back to TOML string format matching the input specification.
/// The output can be parsed by `parse_toml_config()` to recreate an equivalent Config.
///
/// # Arguments
/// * `config` - The DetailedConfig structure to serialize
///
/// # Returns
/// * `Ok(String)` - TOML formatted string
/// * `Err(TomlError)` - Serialization error
pub fn serialize_to_toml(detailed_config: DetailedConfig) -> Result<String, TomlError> {
    let toml_config = DetailedConfigToml::try_from(detailed_config)?;

    toml_config.serialize_to_toml()
}
