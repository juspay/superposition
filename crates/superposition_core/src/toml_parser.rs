use std::collections::HashMap;
use std::fmt;

use bigdecimal::ToPrimitive;
use itertools::Itertools;
use percent_encoding::{percent_decode_str, utf8_percent_encode, AsciiSet, CONTROLS};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::database::models::cac::{DependencyGraph, DimensionType};
use superposition_types::ExtendedMap;
use superposition_types::{Cac, Condition, Config, Context, DimensionInfo, Overrides};

/// Character set for URL-encoding dimension keys and values.
/// Encodes: '=', ';', and all control characters.
const CONTEXT_ENCODE_SET: &AsciiSet = &CONTROLS.add(b'=').add(b';');

/// Check if a string needs quoting in TOML.
/// Strings containing special characters like '=', ';', whitespace, or quotes need quoting.
fn needs_quoting(s: &str) -> bool {
    s.chars()
        .any(|c| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
}

/// Detailed error type for TOML parsing and serialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TomlError {
    TomlSyntaxError(String),
    MissingSection(String),
    InvalidDimension(String),
    MissingField {
        section: String,
        key: String,
        field: String,
    },
    InvalidContextExpression {
        expression: String,
        reason: String,
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
    InvalidContextCondition(String),
    NullValueInConfig(String),
    ValidationError {
        key: String,
        errors: String,
    },
}

impl fmt::Display for TomlError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::MissingSection(s) => {
                write!(f, "TOML parsing error: Missing required section '{}'", s)
            }
            Self::MissingField {
                section,
                key,
                field,
            } => write!(
                f,
                "TOML parsing error: Missing field '{}' in section '{}' for key '{}'",
                field, section, key
            ),
            Self::InvalidContextExpression {
                expression,
                reason,
            } => write!(
                f,
                "TOML parsing error: Invalid context expression '{}': {}",
                expression, reason
            ),
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
            Self::InvalidContextCondition(cond) => write!(f, "Cannot serialize context condition: {}", cond),
            Self::InvalidDimension(d) => write!(f, "Dimension does not exist: {}", d),
            Self::ValidationError { key, errors } => {
                write!(f, "Schema validation failed for key '{}': {}", key, errors)
            }
        }
    }
}

impl std::error::Error for TomlError {}

type DefaultConfigValueMap = Map<String, Value>;
type DefaultConfigSchemaMap = Map<String, Value>;

pub struct DefaultConfigInfo {
    value: Value,
    schema: Value,
}

pub struct DefaultConfigWithSchema(Map<String, DefaultConfigInfo>);

/// Convert TOML value to serde_json Value
fn toml_value_to_serde_value(toml_value: toml::Value) -> Value {
    match toml_value {
        toml::Value::String(s) => Value::String(s),
        toml::Value::Integer(i) => Value::Number(i.into()),
        toml::Value::Float(f) => {
            // Handle NaN and Infinity
            if f.is_finite() {
                serde_json::Number::from_f64(f)
                    .map(Value::Number)
                    .unwrap_or(Value::Null)
            } else {
                Value::Null
            }
        }
        toml::Value::Boolean(b) => Value::Bool(b),
        toml::Value::Datetime(dt) => Value::String(dt.to_string()),
        toml::Value::Array(arr) => {
            let values: Vec<Value> =
                arr.into_iter().map(toml_value_to_serde_value).collect();
            Value::Array(values)
        }
        toml::Value::Table(table) => {
            let mut map = Map::new();
            for (k, v) in table {
                map.insert(k, toml_value_to_serde_value(v));
            }
            Value::Object(map)
        }
    }
}

/// Convert serde_json::Value to toml::Value - return None for NULL
pub fn serde_value_to_toml_value(json: Value) -> Option<toml::Value> {
    match json {
        // TOML has no null, so we return None to signal it should be skipped
        Value::Null => None,

        Value::Bool(b) => Some(toml::Value::Boolean(b)),

        Value::Number(n) => {
            // TOML differentiates between Integer and Float.
            // JSON just has "Number". We try to parse as Integer first.
            if let Some(i) = n.as_i64() {
                Some(toml::Value::Integer(i))
            } else if let Some(f) = n.as_f64() {
                Some(toml::Value::Float(f))
            } else {
                // Edge case: u64 numbers larger than i64::MAX
                // TOML only supports i64 officially.
                // We fallback to string to prevent data loss, or you could panic.
                Some(toml::Value::String(n.to_string()))
            }
        }

        Value::String(s) => Some(toml::Value::String(s)),

        Value::Array(arr) => arr
            .into_iter()
            .map(serde_value_to_toml_value)
            .collect::<Option<Vec<_>>>()
            .map(toml::Value::Array),

        Value::Object(obj) => {
            let mut table = toml::map::Map::new();
            for (k, v) in obj {
                let toml_v = serde_value_to_toml_value(v)?;
                table.insert(k, toml_v);
            }
            Some(toml::Value::Table(table))
        }
    }
}

/// Convert JSON to deterministic sorted string for consistent hashing
fn json_to_sorted_string(v: &Value) -> String {
    match v {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Number(n) => n.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::Array(arr) => {
            let items: Vec<String> = arr.iter().map(json_to_sorted_string).collect();
            format!("[{}]", items.join(","))
        }
        Value::Object(map) => {
            let items: Vec<String> = map
                .iter()
                .sorted_by_key(|(k, _)| *k)
                .map(|(k, v)| format!("\"{}\":{}", k, json_to_sorted_string(v)))
                .collect();
            format!("{{{}}}", items.join(","))
        }
    }
}

/// Hash a serde_json Value using BLAKE3
fn hash(val: &Value) -> String {
    let sorted = json_to_sorted_string(val);
    blake3::hash(sorted.as_bytes()).to_string()
}

/// Parse context expression string (e.g., "os=linux;region=us-east")
/// Keys and values are URL-encoded to handle special characters like '=' and ';'.
fn parse_context_expression(
    input: &str,
    dimensions: &HashMap<String, DimensionInfo>,
) -> Result<Map<String, Value>, TomlError> {
    let mut result = Map::new();

    for pair in input.split(';') {
        let pair = pair.trim();
        if pair.is_empty() {
            continue;
        }

        let (k, v) =
            pair.split_once('=')
                .ok_or_else(|| TomlError::InvalidContextExpression {
                    expression: input.to_string(),
                    reason: format!("Invalid key=value pair: '{}'", pair),
                })?;

        // URL-decode the key
        let key_encoded = k.trim();
        let key = percent_decode_str(key_encoded)
            .decode_utf8()
            .map_err(|e| TomlError::InvalidContextExpression {
                expression: input.to_string(),
                reason: format!("Invalid UTF-8 in encoded key '{}': {}", key_encoded, e),
            })?
            .to_string();

        // URL-decode the value
        let value_encoded = v.trim();
        if value_encoded.is_empty() {
            return Err(TomlError::InvalidContextExpression {
                expression: input.to_string(),
                reason: format!("Empty value after equals in: '{}'", pair),
            });
        }

        let value_str = percent_decode_str(value_encoded)
            .decode_utf8()
            .map_err(|e| TomlError::InvalidContextExpression {
                expression: input.to_string(),
                reason: format!(
                    "Invalid UTF-8 in encoded value '{}': {}",
                    value_encoded, e
                ),
            })?
            .to_string();

        // Type conversion: try to parse as different types
        let value = if let Ok(i) = value_str.parse::<i64>() {
            Value::Number(i.into())
        } else if let Ok(f) = value_str.parse::<f64>() {
            serde_json::Number::from_f64(f)
                .map(Value::Number)
                .unwrap_or_else(|| Value::String(value_str.to_string()))
        } else if let Ok(b) = value_str.parse::<bool>() {
            Value::Bool(b)
        } else {
            Value::String(value_str.to_string())
        };

        // Validate value against dimension schema
        let Some(dimension_info) = dimensions.get(&key) else {
            return Err(TomlError::UndeclaredDimension {
                dimension: key.clone(),
                context: input.to_string(),
            });
        };

        let schema_json = serde_json::to_value(&dimension_info.schema).map_err(|e| {
            TomlError::ConversionError(format!(
                "Invalid schema for dimension '{}': {}",
                key, e
            ))
        })?;

        crate::validations::validate_against_schema(&value, &schema_json).map_err(
            |errors: Vec<String>| TomlError::ValidationError {
                key: format!("{}.{}", input, key),
                errors: crate::validations::format_validation_errors(&errors),
            },
        )?;

        result.insert(key, value);
    }

    Ok(result)
}

/// Parse the default-config section
/// Returns (values, schemas) where schemas are stored for validating overrides
fn parse_default_config(
    table: &toml::Table,
) -> Result<(DefaultConfigValueMap, DefaultConfigSchemaMap), TomlError> {
    let section = table
        .get("default-config")
        .ok_or_else(|| TomlError::MissingSection("default-config".into()))?
        .as_table()
        .ok_or_else(|| {
            TomlError::ConversionError("default-config must be a table".into())
        })?;

    let mut values = Map::new();
    let mut schemas = Map::new();
    for (key, value) in section {
        let table = value.as_table().ok_or_else(|| {
            TomlError::ConversionError(format!(
                "default-config.{} must be a table with 'value' and 'schema'",
                key
            ))
        })?;

        // Validate required fields
        if !table.contains_key("value") {
            return Err(TomlError::MissingField {
                section: "default-config".into(),
                key: key.clone(),
                field: "value".into(),
            });
        }
        if !table.contains_key("schema") {
            return Err(TomlError::MissingField {
                section: "default-config".into(),
                key: key.clone(),
                field: "schema".into(),
            });
        }

        let schema = toml_value_to_serde_value(table["schema"].clone());

        let serde_value = toml_value_to_serde_value(table["value"].clone());

        // Validate value against schema
        crate::validations::validate_against_schema(&serde_value, &schema).map_err(
            |errors: Vec<String>| TomlError::ValidationError {
                key: key.clone(),
                errors: crate::validations::format_validation_errors(&errors),
            },
        )?;

        values.insert(key.clone(), serde_value);
        schemas.insert(key.clone(), schema);
    }

    Ok((values, schemas))
}

/// Parse the dimensions section
fn parse_dimensions(
    table: &toml::Table,
) -> Result<HashMap<String, DimensionInfo>, TomlError> {
    let section = table
        .get("dimensions")
        .ok_or_else(|| TomlError::MissingSection("dimensions".into()))?
        .as_table()
        .ok_or_else(|| TomlError::ConversionError("dimensions must be a table".into()))?;

    let mut result = HashMap::new();
    let mut position_to_dimensions: HashMap<i32, Vec<String>> = HashMap::new();

    // First pass: collect all dimensions without schema validation
    for (key, value) in section {
        let table = value.as_table().ok_or_else(|| {
            TomlError::ConversionError(format!(
                "dimensions.{} must be a table with 'schema' and 'position'",
                key
            ))
        })?;

        if !table.contains_key("schema") {
            return Err(TomlError::MissingField {
                section: "dimensions".into(),
                key: key.clone(),
                field: "schema".into(),
            });
        }

        if !table.contains_key("position") {
            return Err(TomlError::MissingField {
                section: "dimensions".into(),
                key: key.clone(),
                field: "position".into(),
            });
        }

        let position = table["position"].as_integer().ok_or_else(|| {
            TomlError::ConversionError(format!(
                "dimensions.{}.position must be an integer",
                key
            ))
        })? as i32;

        // Track position usage for duplicate detection
        position_to_dimensions
            .entry(position)
            .or_default()
            .push(key.clone());

        let schema = toml_value_to_serde_value(table["schema"].clone());

        let schema_map = ExtendedMap::try_from(schema).map_err(|e| {
            TomlError::ConversionError(format!(
                "Invalid schema for dimension '{}': {}",
                key, e
            ))
        })?;

        let dimension_info = DimensionInfo {
            position,
            schema: schema_map,
            dimension_type: DimensionType::Regular {},
            dependency_graph: DependencyGraph(HashMap::new()),
            value_compute_function_name: None,
        };

        result.insert(key.clone(), dimension_info);
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

    // Second pass: parse dimension types and validate schemas based on type
    for (key, value) in section {
        let table = value.as_table().ok_or_else(|| {
            TomlError::ConversionError(format!("Invalid data for dimension: {}", key))
        })?;

        // Parse dimension type (optional, defaults to "regular")
        let dimension_type = if let Some(type_value) = table.get("type") {
            let type_str = type_value.as_str().ok_or_else(|| {
                TomlError::ConversionError(format!(
                    "dimensions.{}.type must be a string",
                    key
                ))
            })?;

            if type_str == "regular" {
                // Validate regular dimension schema
                let schema = toml_value_to_serde_value(table["schema"].clone());
                crate::validations::validate_schema(&schema).map_err(|errors| {
                    TomlError::ValidationError {
                        key: format!("{}.schema", key),
                        errors: crate::validations::format_validation_errors(&errors),
                    }
                })?;
                DimensionType::Regular {}
            } else if type_str.starts_with("local_cohort:") {
                // Parse format: local_cohort:<dimension_name>
                let parts: Vec<&str> = type_str.splitn(2, ':').collect();
                if parts.len() != 2 {
                    return Err(TomlError::ConversionError(format!(
                        "dimensions.{}.type must be 'regular', 'local_cohort:<dimension_name>', or 'remote_cohort:<dimension_name>', got '{}'",
                        key, type_str
                    )));
                }
                let cohort_dimension = parts[1];
                if cohort_dimension.is_empty() {
                    return Err(TomlError::ConversionError(format!(
                        "dimensions.{}.type: cohort dimension name cannot be empty",
                        key
                    )));
                }

                // Validate that the referenced dimension exists
                if !result.contains_key(cohort_dimension) {
                    return Err(TomlError::ConversionError(format!(
                        "dimensions.{}.type: referenced dimension '{}' does not exist in dimensions table",
                        key, cohort_dimension
                    )));
                }

                // Validate that the schema has the cohort structure (type, enum, definitions)
                let schema = toml_value_to_serde_value(table["schema"].clone());
                crate::validations::validate_cohort_schema_structure(&schema).map_err(
                    |errors| TomlError::ValidationError {
                        key: format!("{}.schema", key),
                        errors: crate::validations::format_validation_errors(&errors),
                    },
                )?;

                DimensionType::LocalCohort(cohort_dimension.to_string())
            } else if type_str.starts_with("remote_cohort:") {
                // Parse format: remote_cohort:<dimension_name>
                let parts: Vec<&str> = type_str.splitn(2, ':').collect();
                if parts.len() != 2 {
                    return Err(TomlError::ConversionError(format!(
                        "dimensions.{}.type must be 'regular', 'local_cohort:<dimension_name>', or 'remote_cohort:<dimension_name>', got '{}'",
                        key, type_str
                    )));
                }
                let cohort_dimension = parts[1];
                if cohort_dimension.is_empty() {
                    return Err(TomlError::ConversionError(format!(
                        "dimensions.{}.type: cohort dimension name cannot be empty",
                        key
                    )));
                }

                // Validate that the referenced dimension exists
                if !result.contains_key(cohort_dimension) {
                    return Err(TomlError::ConversionError(format!(
                        "dimensions.{}.type: referenced dimension '{}' does not exist in dimensions table",
                        key, cohort_dimension
                    )));
                }

                // For remote cohorts, use normal schema validation (no definitions required)
                let schema = toml_value_to_serde_value(table["schema"].clone());
                crate::validations::validate_schema(&schema).map_err(|errors| {
                    TomlError::ValidationError {
                        key: format!("{}.schema", key),
                        errors: crate::validations::format_validation_errors(&errors),
                    }
                })?;

                DimensionType::RemoteCohort(cohort_dimension.to_string())
            } else {
                return Err(TomlError::ConversionError(format!(
                    "dimensions.{}.type must be 'regular', 'local_cohort:<dimension_name>', or 'remote_cohort:<dimension_name>', got '{}'",
                    key, type_str
                )));
            }
        } else {
            // Default to regular, validate schema
            let schema = toml_value_to_serde_value(table["schema"].clone());
            crate::validations::validate_schema(&schema).map_err(|errors| {
                TomlError::ValidationError {
                    key: format!("{}.schema", key),
                    errors: crate::validations::format_validation_errors(&errors),
                }
            })?;
            DimensionType::Regular {}
        };

        let Some(dimension_info) = result.get_mut(key) else {
            return Err(TomlError::InvalidDimension(format!(
                "Dimension {} not available in second pass",
                key.clone()
            )));
        };

        // Update the dimension info with the parsed type
        dimension_info.dimension_type = dimension_type;
    }

    Ok(result)
}

/// Parse the context section
fn parse_contexts(
    table: &toml::Table,
    default_config: &Map<String, Value>,
    schemas: &Map<String, Value>,
    dimensions: &HashMap<String, DimensionInfo>,
) -> Result<(Vec<Context>, HashMap<String, Overrides>), TomlError> {
    let section = table
        .get("context")
        .ok_or_else(|| TomlError::MissingSection("context".into()))?
        .as_table()
        .ok_or_else(|| TomlError::ConversionError("context must be a table".into()))?;

    let mut contexts = Vec::new();
    let mut overrides_map = HashMap::new();

    for (context_expr, override_values) in section {
        // Parse context expression
        let context_map = parse_context_expression(context_expr, dimensions)?;

        // Parse override values
        let override_table = override_values.as_table().ok_or_else(|| {
            TomlError::ConversionError(format!(
                "context.{} must be a table",
                context_expr
            ))
        })?;

        let mut override_config = Map::new();
        for (key, value) in override_table {
            // Validate key exists in default_config
            if !default_config.contains_key(key) {
                return Err(TomlError::InvalidOverrideKey {
                    key: key.clone(),
                    context: context_expr.clone(),
                });
            }

            let serde_value = toml_value_to_serde_value(value.clone());

            // Validate override value against schema
            if let Some(schema) = schemas.get(key) {
                crate::validations::validate_against_schema(&serde_value, schema)
                    .map_err(|errors: Vec<String>| TomlError::ValidationError {
                        key: format!("{}.{}", context_expr, key),
                        errors: crate::validations::format_validation_errors(&errors),
                    })?;
            }

            override_config.insert(key.clone(), serde_value);
        }

        // Compute priority and hash
        let priority = context_map
            .keys()
            .filter_map(|key| dimensions.get(key))
            .map(|dim_info| {
                crate::helpers::calculate_weight_from_index(dim_info.position as u32)
                    .ok()
                    .and_then(|w| w.to_i32())
                    .unwrap_or(0)
            })
            .sum();
        // TODO:: we can possibly operate on the Map instead of converting into a Value::Object
        let override_hash = hash(&Value::Object(override_config.clone()));

        // Create Context
        let condition = Cac::<Condition>::try_from(context_map).map_err(|e| {
            TomlError::ConversionError(format!(
                "Invalid condition for context '{}': {}",
                context_expr, e
            ))
        })?;

        let context = Context {
            condition: condition.into_inner(),
            id: override_hash.clone(),
            priority,
            override_with_keys: superposition_types::OverrideWithKeys::new(
                override_hash.clone(),
            ),
            weight: 1,
        };

        // Create Overrides
        let overrides = Cac::<Overrides>::try_from(override_config)
            .map_err(|e| {
                TomlError::ConversionError(format!(
                    "Invalid overrides for context '{}': {}",
                    context_expr, e
                ))
            })?
            .into_inner();

        contexts.push(context);
        overrides_map.insert(override_hash, overrides);
    }

    Ok((contexts, overrides_map))
}

/// Parse TOML configuration string into structured components
///
/// # Arguments
/// * `toml_content` - TOML string containing default-config, dimensions, and context sections
///
/// # Returns
/// * `Ok(Config)` - Successfully parsed configuration
/// * `Err(TomlError)` - Detailed error about what went wrong
///
/// # Example TOML Format
/// ```toml
/// [default-config]
/// timeout = { value = 30, schema = { type = "integer" } }
///
/// [dimensions]
/// os = { schema = { type = "string" } }
///
/// [context]
/// "os=linux" = { timeout = 60 }
/// ```
pub fn parse(toml_content: &str) -> Result<Config, TomlError> {
    // 1. Parse TOML string
    let toml_table: toml::Table = toml::from_str(toml_content)
        .map_err(|e| TomlError::TomlSyntaxError(e.to_string()))?;

    // 2. Extract and validate "default-config" section
    let (default_config, schemas) = parse_default_config(&toml_table)?;

    // 3. Extract and validate "dimensions" section
    let dimensions = parse_dimensions(&toml_table)?;

    // 4. Extract and parse "context" section
    let (contexts, overrides) =
        parse_contexts(&toml_table, &default_config, &schemas, &dimensions)?;

    Ok(Config {
        default_configs: default_config.into(),
        contexts,
        overrides,
        dimensions,
    })
}

/// Convert Condition to context expression string (e.g., "city=Bangalore; vehicle_type=cab")
/// Keys and values are URL-encoded to handle special characters like '=' and ';'.
fn condition_to_string(condition: &Cac<Condition>) -> Result<String, TomlError> {
    // Clone the condition to get the inner Map
    let condition_inner = condition.clone().into_inner();

    let mut pairs: Vec<String> = condition_inner
        .iter()
        .map(|(key, value)| {
            let key_encoded = utf8_percent_encode(key, CONTEXT_ENCODE_SET).to_string();
            let value_encoded =
                utf8_percent_encode(&value_to_string_simple(value), CONTEXT_ENCODE_SET)
                    .to_string();
            format!("{}={}", key_encoded, value_encoded)
        })
        .collect();

    // Sort for deterministic output
    pairs.sort();

    Ok(pairs.join("; "))
}

/// Simple value to string for context expressions (no quotes for strings)
fn value_to_string_simple(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        _ => value.to_string(),
    }
}

/// Serialize Config structure to TOML format
///
/// Converts a Config object back to TOML string format matching the input specification.
/// The output can be parsed by `parse()` to recreate an equivalent Config.
///
/// # Arguments
/// * `config` - The Config structure to serialize
///
/// # Returns
/// * `Ok(String)` - TOML formatted string
/// * `Err(TomlError)` - Serialization error
pub fn serialize_to_toml(config: &Config) -> Result<String, TomlError> {
    let mut output = String::new();

    // 1. Serialize [default-config] section
    output.push_str("[default-config]\n");
    for (key, value) in config.default_configs.iter() {
        // Quote key if it contains special characters
        let quoted_key = if needs_quoting(key) {
            format!(r#""{}""#, key.replace('"', r#"\""#))
        } else {
            key.clone()
        };

        // Infer a basic schema type based on the value
        let schema = match value {
            Value::String(_) => r#"{ type = "string" }"#,
            Value::Number(n) => {
                if n.is_i64() {
                    r#"{ type = "integer" }"#
                } else {
                    r#"{ type = "number" }"#
                }
            }
            Value::Bool(_) => r#"{ type = "boolean" }"#,
            Value::Array(_) => r#"{ type = "array" }"#,
            Value::Object(_) => r#"{ type = "object" }"#,
            Value::Null => r#"{ type = "null" }"#,
        };

        let toml_value = serde_value_to_toml_value(value.clone()).ok_or_else(|| {
            TomlError::NullValueInConfig(format!("Null value present for key: {}", key))
        })?;

        let toml_entry = format!(
            "{} = {{ value = {}, schema = {} }}\n",
            quoted_key, toml_value, schema
        );
        output.push_str(&toml_entry);
    }
    output.push('\n');

    // 2. Serialize [dimensions] section
    output.push_str("[dimensions]\n");
    let mut sorted_dims: Vec<_> = config.dimensions.iter().collect();
    sorted_dims.sort_by_key(|(_, info)| info.position);

    for (name, info) in sorted_dims {
        let schema_json = serde_json::to_value(&info.schema).map_err(|e| {
            TomlError::SerializationError(format!("{}: for dimension: {}", e, name))
        })?;

        // Serialize dimension type
        let type_field = match &info.dimension_type {
            DimensionType::Regular {} => r#"type = "regular""#.to_string(),
            DimensionType::LocalCohort(cohort_name) => {
                format!(r#"type = "local_cohort:{}""#, cohort_name)
            }
            DimensionType::RemoteCohort(cohort_name) => {
                format!(r#"type = "remote_cohort:{}""#, cohort_name)
            }
        };

        // Quote dimension name if it contains special characters
        let quoted_name = if needs_quoting(name) {
            format!(r#""{}""#, name.replace('"', r#"\""#))
        } else {
            name.clone()
        };

        let Some(schema_toml) = serde_value_to_toml_value(schema_json) else {
            return Err(TomlError::NullValueInConfig(format!(
                "schema for dimensions: {} contains null values",
                name
            )));
        };

        let toml_entry = format!(
            "{} = {{ position = {}, schema = {}, {} }}\n",
            quoted_name, info.position, schema_toml, type_field
        );
        output.push_str(&toml_entry);
    }
    output.push('\n');

    // 3. Serialize [context.*] sections
    for context in &config.contexts {
        // Wrap Condition in Cac for condition_to_string
        let condition_cac = Cac::<Condition>::try_from(context.condition.clone())
            .map_err(|e| {
                TomlError::InvalidContextCondition(format!(
                    "{}: for context: {}",
                    e, context.id
                ))
            })?;
        let condition_str = condition_to_string(&condition_cac)?;

        output.push_str(&format!("[context.\"{}\"]\n", condition_str));

        // DIAGNOSTIC: Print what we're looking for vs what's available
        let override_key = context.override_with_keys.get_key();
        if let Some(overrides) = config.overrides.get(override_key) {
            for (key, value) in overrides.clone() {
                // Quote key if it contains special characters
                let quoted_key = if needs_quoting(&key) {
                    format!(r#""{}""#, key.replace('"', r#"\""#))
                } else {
                    key.clone()
                };
                let toml_value =
                    serde_value_to_toml_value(value.clone()).ok_or_else(|| {
                        TomlError::NullValueInConfig(format!(
                            "Null value for key: {} in context: {}",
                            key, context.id
                        ))
                    })?;

                output.push_str(&format!("{} = {}\n", quoted_key, toml_value));
            }
        }
        output.push('\n');
    }

    Ok(output)
}

#[cfg(test)]
mod serialization_tests {
    use super::*;

    #[test]
    fn test_condition_to_string_simple() {
        let mut condition_map = Map::new();
        condition_map.insert("city".to_string(), Value::String("Bangalore".to_string()));
        let condition = Cac::<Condition>::try_from(condition_map).unwrap();

        let result = condition_to_string(&condition).unwrap();
        assert_eq!(result, "city=Bangalore");
    }

    #[test]
    fn test_condition_to_string_multiple() {
        let mut condition_map = Map::new();
        condition_map.insert("city".to_string(), Value::String("Bangalore".to_string()));
        condition_map
            .insert("vehicle_type".to_string(), Value::String("cab".to_string()));
        let condition = Cac::<Condition>::try_from(condition_map).unwrap();

        let result = condition_to_string(&condition).unwrap();
        // Order may vary, check both parts present
        assert!(result.contains("city=Bangalore"));
        assert!(result.contains("vehicle_type=cab"));
        assert!(result.contains("; "));
    }

    #[test]
    fn test_toml_round_trip_simple() {
        let original_toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { "type" = "string" } }

[context."os=linux"]
timeout = 60
"#;

        // Parse TOML -> Config
        let config = parse(original_toml).unwrap();

        // Serialize Config -> TOML
        let serialized = serialize_to_toml(&config).unwrap();

        // Parse again
        let reparsed = parse(&serialized).unwrap();

        // Configs should be functionally equivalent
        assert_eq!(config.default_configs, reparsed.default_configs);
        assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
        assert_eq!(config.contexts.len(), reparsed.contexts.len());
    }

    #[test]
    fn test_toml_round_trip_empty_config() {
        // Note: parse() requires a context section, so we need a minimal valid TOML
        let toml_str = r#"
[default-config]

[dimensions]
os = { position = 1, schema = { type = "string" } }

[context]
"#;

        let config = parse(toml_str).unwrap();
        assert!(config.default_configs.is_empty());
        assert_eq!(config.contexts.len(), 0);
    }

    #[test]
    fn test_dimension_type_regular() {
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" }, type = "regular" }

[context."os=linux"]
timeout = 60
"#;

        let config = parse(toml).unwrap();
        let serialized = serialize_to_toml(&config).unwrap();
        let reparsed = parse(&serialized).unwrap();

        assert!(serialized.contains(r#"type = "regular""#));
        assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
    }

    #[test]
    fn test_dimension_type_local_cohort() {
        // Note: TOML cannot represent jsonlogic rules with operators like "==" as keys
        // So we test parsing with a simplified schema that has the required structure
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
os_cohort = { position = 2, type = "local_cohort:os", schema = { type = "string", enum = ["linux", "windows", "otherwise"], definitions = { linux = "rule_for_linux", windows = "rule_for_windows" } } }

[context."os=linux"]
timeout = 60
"#;

        let config = parse(toml).unwrap();
        let serialized = serialize_to_toml(&config).unwrap();
        let reparsed = parse(&serialized).unwrap();

        assert!(serialized.contains(r#"type = "local_cohort:os""#));
        assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
    }

    #[test]
    fn test_dimension_type_local_cohort_invalid_reference() {
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os_cohort = { position = 1, schema = { type = "string" }, type = "local_cohort:nonexistent" }

[context."os=linux"]
timeout = 60
"#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("does not exist"));
    }

    #[test]
    fn test_dimension_type_local_cohort_empty_name() {
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
os_cohort = { position = 2, schema = { type = "string" }, type = "local_cohort:" }

[context."os=linux"]
timeout = 60
"#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be empty"));
    }

    #[test]
    fn test_dimension_type_remote_cohort() {
        // Remote cohorts use normal schema validation (no definitions required)
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
os_cohort = { position = 2, type = "remote_cohort:os", schema = { type = "string", enum = ["linux", "windows", "macos"] } }

[context."os=linux"]
timeout = 60
"#;

        let config = parse(toml).unwrap();
        let serialized = serialize_to_toml(&config).unwrap();
        let reparsed = parse(&serialized).unwrap();

        assert!(serialized.contains(r#"type = "remote_cohort:os""#));
        assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
    }

    #[test]
    fn test_dimension_type_remote_cohort_invalid_reference() {
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os_cohort = { position = 1, schema = { type = "string" }, type = "remote_cohort:nonexistent" }

[context."os=linux"]
timeout = 60
"#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("does not exist"));
    }

    #[test]
    fn test_dimension_type_remote_cohort_empty_name() {
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
os_cohort = { position = 2, schema = { type = "string" }, type = "remote_cohort:" }

[context."os=linux"]
timeout = 60
"#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be empty"));
    }

    #[test]
    fn test_dimension_type_remote_cohort_invalid_schema() {
        // Remote cohorts with invalid schema should fail validation
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }
os_cohort = { position = 2, type = "remote_cohort:os", schema = { type = "invalid_type" } }

[context."os=linux"]
timeout = 60
"#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Schema validation failed"));
    }

    #[test]
    fn test_dimension_type_default_regular() {
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }

[context."os=linux"]
timeout = 60
"#;

        let config = parse(toml).unwrap();
        let serialized = serialize_to_toml(&config).unwrap();
        let reparsed = parse(&serialized).unwrap();

        // Default should be regular
        assert!(serialized.contains(r#"type = "regular""#));
        assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
    }

    #[test]
    fn test_dimension_type_invalid_format() {
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" }, type = "local_cohort" }

[context."os=linux"]
timeout = 60
"#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("local_cohort:<dimension_name>"));
    }

    // rest of the tests
    #[test]
    fn test_valid_toml_parsing() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }
            enabled = { value = true, schema = { type = "boolean" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context."os=linux"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_ok());
        let parsed = result.unwrap();
        assert_eq!(parsed.default_configs.len(), 2);
        assert_eq!(parsed.dimensions.len(), 1);
        assert_eq!(parsed.contexts.len(), 1);
        assert_eq!(parsed.overrides.len(), 1);
    }

    #[test]
    fn test_missing_section_error() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(result, Err(TomlError::MissingSection(_))));
    }

    #[test]
    fn test_missing_value_field() {
        let toml = r#"
            [default-config]
            timeout = { schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context]
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(result, Err(TomlError::MissingField { .. })));
    }

    #[test]
    fn test_undeclared_dimension() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context."region=us-east"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(result, Err(TomlError::UndeclaredDimension { .. })));
    }

    #[test]
    fn test_invalid_override_key() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context."os=linux"]
            port = 8080
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(result, Err(TomlError::InvalidOverrideKey { .. })));
    }

    #[test]
    fn test_hash_consistency() {
        let val1 = serde_json::json!({"a": 1, "b": 2});
        let val2 = serde_json::json!({"b": 2, "a": 1});
        assert_eq!(hash(&val1), hash(&val2));
    }

    #[test]
    fn test_toml_value_conversion() {
        let toml_str = toml::Value::String("test".to_string());
        let json_val = toml_value_to_serde_value(toml_str);
        assert_eq!(json_val, Value::String("test".to_string()));

        let toml_int = toml::Value::Integer(42);
        let json_val = toml_value_to_serde_value(toml_int);
        assert_eq!(json_val, Value::Number(42.into()));

        let toml_bool = toml::Value::Boolean(true);
        let json_val = toml_value_to_serde_value(toml_bool);
        assert_eq!(json_val, Value::Bool(true));
    }

    #[test]
    fn test_priority_calculation() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }
            region = { position = 2, schema = { type = "string" } }

            [context."os=linux"]
            timeout = 60

            [context."os=linux;region=us-east"]
            timeout = 90
        "#;

        let result = parse(toml);
        assert!(result.is_ok());
        let parsed = result.unwrap();

        // First context has os (position 1): priority = 2^1 = 2
        // Second context has os (position 1) and region (position 2): priority = 2^1 + 2^2 = 6
        assert_eq!(parsed.contexts[0].priority, 2);
        assert_eq!(parsed.contexts[1].priority, 6);
    }

    #[test]
    fn test_missing_position_error() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { schema = { type = "string" } }

            [context."os=linux"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(TomlError::MissingField {
                section,
                field,
                ..
            }) if section == "dimensions" && field == "position"
        ));
    }

    #[test]
    fn test_duplicate_position_error() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }
            region = { position = 1, schema = { type = "string" } }

            [context."os=linux"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(TomlError::DuplicatePosition {
                position,
                dimensions
            }) if position == 1 && dimensions.len() == 2
        ));
    }

    // Validation tests
    #[test]
    fn test_validation_valid_default_config() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }
            enabled = { value = true, schema = { type = "boolean" } }
            name = { value = "test", schema = { type = "string" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context."os=linux"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validation_invalid_default_config_type_mismatch() {
        let toml = r#"
            [default-config]
            timeout = { value = "not_an_integer", schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context."os=linux"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(result, Err(TomlError::ValidationError { .. })));
        let err = result.unwrap_err();
        assert!(err.to_string().contains("timeout"));
    }

    #[test]
    fn test_validation_valid_context_override() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context."os=linux"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validation_invalid_context_override_type_mismatch() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context."os=linux"]
            timeout = "not_an_integer"
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(result, Err(TomlError::ValidationError { .. })));
        let err = result.unwrap_err();
        assert!(err.to_string().contains("os=linux"));
    }

    #[test]
    fn test_validation_valid_dimension_value_in_context() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string", enum = ["linux", "windows", "macos"] } }

            [context."os=linux"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validation_invalid_dimension_value_in_context() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string", enum = ["linux", "windows", "macos"] } }

            [context."os=freebsd"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(result, Err(TomlError::ValidationError { .. })));
        let err = result.unwrap_err();
        assert!(err.to_string().contains("os=freebsd.os"));
    }

    #[test]
    fn test_validation_with_minimum_constraint() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer", minimum = 10 } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context."os=linux"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validation_fails_minimum_constraint() {
        let toml = r#"
            [default-config]
            timeout = { value = 5, schema = { type = "integer", minimum = 10 } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context."os=linux"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(result, Err(TomlError::ValidationError { .. })));
        let err = result.unwrap_err();
        assert!(err.to_string().contains("timeout"));
    }

    #[test]
    fn test_validation_numeric_dimension_value() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            port = { position = 1, schema = { type = "integer", minimum = 1, maximum = 65535 } }

            [context."port=8080"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validation_invalid_numeric_dimension_value() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            port = { position = 1, schema = { type = "integer", minimum = 1, maximum = 65535 } }

            [context."port=70000"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(result, Err(TomlError::ValidationError { .. })));
        let err = result.unwrap_err();
        assert!(err.to_string().contains("port=70000.port"));
    }

    #[test]
    fn test_validation_boolean_dimension_value() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            debug = { position = 1, schema = { type = "boolean" } }

            [context."debug=true"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validation_invalid_boolean_dimension_value() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            debug = { position = 1, schema = { type = "boolean" } }

            [context."debug=yes"]
            timeout = 60
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(result, Err(TomlError::ValidationError { .. })));
        let err = result.unwrap_err();
        assert!(err.to_string().contains("debug=yes.debug"));
    }

    #[test]
    fn test_url_encoding_special_chars() {
        // Test with dimension keys and values containing '=' and ';'
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
"key=with=equals" = { position = 1, schema = { type = "string" } }

[context."key%3Dwith%3Dequals=value%3Bwith%3Bsemicolon"]
timeout = 60
"#;

        let config = parse(toml).unwrap();

        // The parsed condition should have the decoded key and value
        assert_eq!(config.contexts.len(), 1);
        let context = &config.contexts[0];
        assert_eq!(
            context.condition.get("key=with=equals"),
            Some(&Value::String("value;with;semicolon".to_string()))
        );
    }

    #[test]
    fn test_url_encoding_round_trip() {
        // Test that serialization and deserialization work with special chars
        let original_toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
"key=with=equals" = { position = 1, schema = { type = "string" } }
"region" = { position = 0, schema = { type = "string" } }

[context."key%3Dwith%3Dequals=value%3Bwith%3Bsemicolon; region=us-east"]
timeout = 60
"#;

        // Parse TOML -> Config
        let config = parse(original_toml).unwrap();

        // Serialize Config -> TOML
        let serialized = serialize_to_toml(&config).unwrap();

        // The serialized TOML should have URL-encoded keys and values
        assert!(serialized.contains("key%3Dwith%3Dequals"));
        assert!(serialized.contains("value%3Bwith%3Bsemicolon"));

        // Parse again
        let reparsed = parse(&serialized).unwrap();

        // Configs should be functionally equivalent
        assert_eq!(config.default_configs, reparsed.default_configs);
        assert_eq!(config.contexts.len(), reparsed.contexts.len());

        // The condition should have the decoded values
        let context = &reparsed.contexts[0];
        assert_eq!(
            context.condition.get("key=with=equals"),
            Some(&Value::String("value;with;semicolon".to_string()))
        );
        assert_eq!(
            context.condition.get("region"),
            Some(&Value::String("us-east".to_string()))
        );
    }

    #[test]
    fn test_object_value_round_trip() {
        // Test that object values are serialized as triple-quoted JSON and parsed back correctly
        let original_toml = r#"
[default-config]
config = { value = { host = "localhost", port = 8080 } , schema = { type = "object" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }

[context."os=linux"]
config = { host = "prod.example.com", port = 443 }
"#;

        // Parse TOML -> Config
        let config = parse(original_toml).unwrap();

        // Verify default config object was parsed correctly
        let default_config_value = config.default_configs.get("config").unwrap();
        assert_eq!(
            default_config_value.get("host"),
            Some(&Value::String("localhost".to_string()))
        );
        assert_eq!(
            default_config_value.get("port"),
            Some(&Value::Number(serde_json::Number::from(8080)))
        );

        // Serialize Config -> TOML
        let serialized = serialize_to_toml(&config).unwrap();

        // Parse again
        let reparsed = parse(&serialized).unwrap();

        // Configs should be functionally equivalent
        assert_eq!(config.default_configs, reparsed.default_configs);
        assert_eq!(config.contexts.len(), reparsed.contexts.len());

        // Verify override object was parsed correctly
        let override_key = config.contexts[0].override_with_keys.get_key();
        let overrides = config.overrides.get(override_key).unwrap();
        let override_config = overrides.get("config").unwrap();
        assert_eq!(
            override_config.get("host"),
            Some(&Value::String("prod.example.com".to_string()))
        );
        assert_eq!(
            override_config.get("port"),
            Some(&Value::Number(serde_json::Number::from(443)))
        );
    }
}
