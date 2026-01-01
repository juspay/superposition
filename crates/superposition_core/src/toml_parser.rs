use std::collections::HashMap;
use std::fmt;

use itertools::Itertools;
use serde_json::{Map, Value};
use superposition_types::database::models::cac::{DependencyGraph, DimensionType};
use superposition_types::ExtendedMap;
use superposition_types::{Cac, Condition, Config, Context, DimensionInfo, Overrides};

/// Detailed error type for TOML parsing
#[derive(Debug, Clone)]
pub enum TomlParseError {
    FileReadError(String),
    TomlSyntaxError(String),
    MissingSection(String),
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
}

impl fmt::Display for TomlParseError {
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
            Self::TomlSyntaxError(e) => write!(f, "TOML syntax error: {}", e),
            Self::ConversionError(e) => write!(f, "TOML conversion error: {}", e),
            Self::FileReadError(e) => write!(f, "File read error: {}", e),
        }
    }
}

impl std::error::Error for TomlParseError {}

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

/// Compute priority based on dimension positions (bit-shift calculation)
fn compute_priority(
    context_map: &Map<String, Value>,
    dimensions: &HashMap<String, DimensionInfo>,
) -> i32 {
    context_map
        .keys()
        .filter_map(|key| dimensions.get(key))
        .map(|dim_info| 1 << dim_info.position)
        .sum()
}

/// Parse context expression string (e.g., "os=linux;region=us-east")
fn parse_context_expression(
    input: &str,
    dimensions: &HashMap<String, DimensionInfo>,
) -> Result<Map<String, Value>, TomlParseError> {
    let mut result = Map::new();

    for pair in input.split(';') {
        let pair = pair.trim();
        if pair.is_empty() {
            continue;
        }

        let parts: Vec<&str> = pair.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err(TomlParseError::InvalidContextExpression {
                expression: input.to_string(),
                reason: format!("Invalid key=value pair: '{}'", pair),
            });
        }

        let key = parts[0].trim();
        let value_str = parts[1].trim();

        if value_str.is_empty() {
            return Err(TomlParseError::InvalidContextExpression {
                expression: input.to_string(),
                reason: format!("Empty value after equals in: '{}'", pair),
            });
        }

        // Validate dimension exists
        if !dimensions.contains_key(key) {
            return Err(TomlParseError::UndeclaredDimension {
                dimension: key.to_string(),
                context: input.to_string(),
            });
        }

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

        result.insert(key.to_string(), value);
    }

    Ok(result)
}

/// Parse the default-config section
fn parse_default_config(
    table: &toml::Table,
) -> Result<Map<String, Value>, TomlParseError> {
    let section = table
        .get("default-config")
        .ok_or_else(|| TomlParseError::MissingSection("default-config".into()))?
        .as_table()
        .ok_or_else(|| {
            TomlParseError::ConversionError("default-config must be a table".into())
        })?;

    let mut result = Map::new();
    for (key, value) in section {
        let table = value.as_table().ok_or_else(|| {
            TomlParseError::ConversionError(format!(
                "default-config.{} must be a table with 'value' and 'schema'",
                key
            ))
        })?;

        // Validate required fields
        if !table.contains_key("value") {
            return Err(TomlParseError::MissingField {
                section: "default-config".into(),
                key: key.clone(),
                field: "value".into(),
            });
        }
        if !table.contains_key("schema") {
            return Err(TomlParseError::MissingField {
                section: "default-config".into(),
                key: key.clone(),
                field: "schema".into(),
            });
        }

        let value = toml_value_to_serde_value(table["value"].clone());
        result.insert(key.clone(), value);
    }

    Ok(result)
}

/// Parse the dimensions section
fn parse_dimensions(
    table: &toml::Table,
) -> Result<HashMap<String, DimensionInfo>, TomlParseError> {
    let section = table
        .get("dimensions")
        .ok_or_else(|| TomlParseError::MissingSection("dimensions".into()))?
        .as_table()
        .ok_or_else(|| {
            TomlParseError::ConversionError("dimensions must be a table".into())
        })?;

    let mut result = HashMap::new();
    let mut position_to_dimensions: HashMap<i32, Vec<String>> = HashMap::new();

    for (key, value) in section {
        let table = value.as_table().ok_or_else(|| {
            TomlParseError::ConversionError(format!(
                "dimensions.{} must be a table with 'schema' and 'position'",
                key
            ))
        })?;

        if !table.contains_key("schema") {
            return Err(TomlParseError::MissingField {
                section: "dimensions".into(),
                key: key.clone(),
                field: "schema".into(),
            });
        }

        if !table.contains_key("position") {
            return Err(TomlParseError::MissingField {
                section: "dimensions".into(),
                key: key.clone(),
                field: "position".into(),
            });
        }

        let position = table["position"].as_integer().ok_or_else(|| {
            TomlParseError::ConversionError(format!(
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
            TomlParseError::ConversionError(format!(
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
            return Err(TomlParseError::DuplicatePosition {
                position,
                dimensions,
            });
        }
    }

    Ok(result)
}

/// Parse the context section
fn parse_contexts(
    table: &toml::Table,
    default_config: &Map<String, Value>,
    dimensions: &HashMap<String, DimensionInfo>,
) -> Result<(Vec<Context>, HashMap<String, Overrides>), TomlParseError> {
    let section = table
        .get("context")
        .ok_or_else(|| TomlParseError::MissingSection("context".into()))?
        .as_table()
        .ok_or_else(|| {
            TomlParseError::ConversionError("context must be a table".into())
        })?;

    let mut contexts = Vec::new();
    let mut overrides_map = HashMap::new();

    for (context_expr, override_values) in section {
        // Parse context expression
        let context_map = parse_context_expression(context_expr, dimensions)?;

        // Parse override values
        let override_table = override_values.as_table().ok_or_else(|| {
            TomlParseError::ConversionError(format!(
                "context.{} must be a table",
                context_expr
            ))
        })?;

        let mut override_config = Map::new();
        for (key, value) in override_table {
            // Validate key exists in default_config
            if !default_config.contains_key(key) {
                return Err(TomlParseError::InvalidOverrideKey {
                    key: key.clone(),
                    context: context_expr.clone(),
                });
            }

            let serde_value = toml_value_to_serde_value(value.clone());
            override_config.insert(key.clone(), serde_value);
        }

        // Compute priority and hash
        let priority = compute_priority(&context_map, dimensions);
        let override_hash = hash(&serde_json::to_value(&override_config).unwrap());

        // Create Context
        let condition = Cac::<Condition>::try_from(context_map).map_err(|e| {
            TomlParseError::ConversionError(format!(
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
                TomlParseError::ConversionError(format!(
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
/// * `Err(TomlParseError)` - Detailed error about what went wrong
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
pub fn parse(toml_content: &str) -> Result<Config, TomlParseError> {
    // 1. Parse TOML string
    let toml_table: toml::Table = toml::from_str(toml_content)
        .map_err(|e| TomlParseError::TomlSyntaxError(e.to_string()))?;

    // 2. Extract and validate "default-config" section
    let default_config = parse_default_config(&toml_table)?;

    // 3. Extract and validate "dimensions" section
    let dimensions = parse_dimensions(&toml_table)?;

    // 4. Extract and parse "context" section
    let (contexts, overrides) =
        parse_contexts(&toml_table, &default_config, &dimensions)?;

    Ok(Config {
        default_configs: default_config.into(),
        contexts,
        overrides,
        dimensions,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_toml_parsing() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }
            enabled = { value = true, schema = { type = "boolean" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context]
            "os=linux" = { timeout = 60 }
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
        assert!(matches!(result, Err(TomlParseError::MissingSection(_))));
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
        assert!(matches!(result, Err(TomlParseError::MissingField { .. })));
    }

    #[test]
    fn test_undeclared_dimension() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context]
            "region=us-east" = { timeout = 60 }
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(TomlParseError::UndeclaredDimension { .. })
        ));
    }

    #[test]
    fn test_invalid_override_key() {
        let toml = r#"
            [default-config]
            timeout = { value = 30, schema = { type = "integer" } }

            [dimensions]
            os = { position = 1, schema = { type = "string" } }

            [context]
            "os=linux" = { port = 8080 }
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(TomlParseError::InvalidOverrideKey { .. })
        ));
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

            [context]
            "os=linux" = { timeout = 60 }
            "os=linux;region=us-east" = { timeout = 90 }
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

            [context]
            "os=linux" = { timeout = 60 }
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(TomlParseError::MissingField {
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

            [context]
            "os=linux" = { timeout = 60 }
        "#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(TomlParseError::DuplicatePosition {
                position,
                dimensions
            }) if position == 1 && dimensions.len() == 2
        ));
    }
}
