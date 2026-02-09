mod helpers;
#[cfg(test)]
mod test;

use std::{
    collections::{BTreeMap, HashMap},
    fmt,
    ops::Deref,
    str::FromStr,
};

use bigdecimal::ToPrimitive;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::database::models::cac::{DependencyGraph, DimensionType};
use superposition_types::{
    Cac, Condition, Config, Context, DefaultConfigsWithSchema, DetailedConfig,
    DimensionInfo, ExtendedMap, OverrideWithKeys, Overrides,
};

use crate::{
    helpers::{calculate_context_weight, hash},
    toml::helpers::{
        create_connections_with_dependents, inline_table, to_toml_string,
        validate_config_key, validate_context, validate_overrides,
    },
    validations,
};

/// Detailed error type for TOML parsing and serialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TomlError {
    TomlSyntaxError(String),
    InvalidDimension(String),
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
    NullValueInConfig(String),
    ValidationError {
        key: String,
        errors: String,
    },
}

impl fmt::Display for TomlError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
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
            Self::NullValueInConfig(e) => write!(f, "TOML cannot handle NULL values for key: {}", e),
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

#[derive(Serialize, Deserialize, Clone)]
pub struct DimensionInfoToml {
    pub position: i32,
    pub schema: Map<String, Value>,
    #[serde(rename = "type", default = "dim_type_default")]
    pub dimension_type: String,
}

fn dim_type_default() -> String {
    DimensionType::default().to_string()
}

impl From<DimensionInfo> for DimensionInfoToml {
    fn from(d: DimensionInfo) -> Self {
        Self {
            position: d.position,
            schema: d.schema.into_inner(),
            dimension_type: d.dimension_type.to_string(),
        }
    }
}

impl TryFrom<DimensionInfoToml> for DimensionInfo {
    type Error = TomlError;
    fn try_from(d: DimensionInfoToml) -> Result<Self, Self::Error> {
        Ok(Self {
            position: d.position,
            schema: ExtendedMap::from(d.schema),
            dimension_type: DimensionType::from_str(&d.dimension_type)
                .map_err(|e| TomlError::ConversionError(e))?,
            dependency_graph: DependencyGraph(HashMap::new()),
            value_compute_function_name: None,
        })
    }
}

#[derive(Serialize, Deserialize)]
struct ContextToml {
    #[serde(rename = "_context_")]
    context: BTreeMap<String, Value>,
    #[serde(flatten)]
    overrides: BTreeMap<String, Value>,
}

impl From<(Context, &HashMap<String, Overrides>)> for ContextToml {
    fn from((context, overrides): (Context, &HashMap<String, Overrides>)) -> Self {
        Self {
            context: context.condition.deref().clone().into_iter().collect(),
            overrides: overrides
                .get(context.override_with_keys.get_key())
                .map(|ov| ov.clone().into_iter().collect())
                .unwrap_or_default(),
        }
    }
}

impl From<DetailedConfig> for DetailedConfigToml {
    fn from(d: DetailedConfig) -> Self {
        Self {
            default_configs: d.default_configs,
            dimensions: d
                .dimensions
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            contexts: d
                .contexts
                .into_iter()
                .map(|c| ContextToml::from((c, &d.overrides)))
                .collect(),
        }
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
            validate_config_key(k, &v.value, &v.schema, 0)?;
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
        for (index, ctx) in d.contexts.into_iter().enumerate() {
            let override_map: Map<_, _> = ctx.overrides.into_iter().collect();
            let over_val = Value::Object(override_map);
            let override_hash = hash(&over_val);
            let override_ = match over_val {
                Value::Object(override_map) => Cac::<Overrides>::try_from(override_map)
                    .ok()
                    .map(|cac| cac.into_inner()),
                _ => None,
            };

            let condition_map: Map<_, _> = ctx.context.into_iter().collect();
            let cond_val = Value::Object(condition_map);
            let condition_hash = hash(&cond_val);
            let condition = match cond_val {
                Value::Object(condition_map) => Cac::<Condition>::try_from(condition_map)
                    .ok()
                    .map(|cac| cac.into_inner()),
                _ => None,
            };

            match (override_, condition) {
                (Some(o), Some(c)) => {
                    validate_context(&c, &dimensions, index)?;
                    validate_overrides(&o, &default_configs, index)?;

                    let priority = calculate_context_weight(&c, &dimensions)
                        .map_err(|e| TomlError::ConversionError(e.to_string()))?
                        .to_i32()
                        .ok_or_else(|| {
                            TomlError::ConversionError(
                                "Failed to convert context weight to i32".to_string(),
                            )
                        })?;

                    overrides.insert(override_hash.clone(), o);
                    contexts.push(Context {
                        condition: c,
                        id: condition_hash,
                        priority,
                        override_with_keys: OverrideWithKeys::new(override_hash),
                        weight: 0,
                    });
                }
                _ => {}
            }
        }

        // Sort contexts by priority (weight) - higher weight means higher priority
        contexts.sort_by(|a, b| b.priority.cmp(&a.priority));

        // Set correct values for weight and priority after sorting
        contexts.iter_mut().enumerate().for_each(|(index, ctx)| {
            ctx.weight = index as i32;
            ctx.priority = index as i32;
        });

        let detailed_config = Self {
            default_configs,
            dimensions,
            contexts,
            overrides,
        };

        Ok(detailed_config)
    }
}

#[derive(Serialize, Deserialize)]
struct DetailedConfigToml {
    #[serde(rename = "default-configs")]
    default_configs: DefaultConfigsWithSchema,
    dimensions: BTreeMap<String, DimensionInfoToml>,
    contexts: Vec<ContextToml>,
}

impl DetailedConfigToml {
    fn emit_default_configs(
        default_configs: DefaultConfigsWithSchema,
    ) -> Result<String, TomlError> {
        let mut out = String::new();
        out.push_str("[default-configs]\n");

        for (k, v) in default_configs.into_inner() {
            let val = serde_json::to_value(v).map_err(|e| {
                TomlError::SerializationError(format!(
                    "Failed to serialize dimension '{}': {}",
                    k, e
                ))
            })?;
            let v_str = to_toml_string(val, &k)?;
            out.push_str(&format!("{k} = {v_str}\n"));
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
            let val = serde_json::to_value(v).map_err(|e| {
                TomlError::SerializationError(format!(
                    "Failed to serialize dimension '{}': {}",
                    k, e
                ))
            })?;
            let v_str = to_toml_string(val, &k)?;
            out.push_str(&format!("{k} = {v_str}\n"));
        }

        out.push('\n');
        Ok(out)
    }

    fn emit_context(ctx: ContextToml) -> Result<String, TomlError> {
        let mut out = String::new();
        out.push_str("[[overrides]]\n");
        out.push_str(&format!(
            "_context_ = {}\n",
            inline_table(ctx.context, "_context_")?
        ));

        for (k, v) in ctx.overrides {
            let v_str = to_toml_string(v, &k)?;
            out.push_str(&format!("{k} = {v_str}\n"));
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

        for ctx in self.contexts {
            out.push_str(&Self::emit_context(ctx)?);
        }

        out.push('\n');
        Ok(out)
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
    let toml_config = DetailedConfigToml::from(detailed_config);

    toml_config.serialize_to_toml()
}
