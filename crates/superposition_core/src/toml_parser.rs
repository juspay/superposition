use std::collections::HashMap;
use std::fmt;

use itertools::Itertools;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::database::models::cac::{DependencyGraph, DimensionType};
use superposition_types::ExtendedMap;
use superposition_types::{Cac, Condition, Config, Context, DimensionInfo, Overrides};

/// Detailed error type for TOML parsing and serialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TomlError {
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
    SerializationError(String),
    InvalidContextCondition(String),
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
            Self::TomlSyntaxError(e) => write!(f, "TOML syntax error: {}", e),
            Self::ConversionError(e) => write!(f, "TOML conversion error: {}", e),
            Self::FileReadError(e) => write!(f, "File read error: {}", e),
            Self::SerializationError(msg) => write!(f, "TOML serialization error: {}", msg),
            Self::InvalidContextCondition(cond) => write!(f, "Cannot serialize context condition: {}", cond),
        }
    }
}

impl std::error::Error for TomlError {}

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
) -> Result<Map<String, Value>, TomlError> {
    let mut result = Map::new();

    for pair in input.split(';') {
        let pair = pair.trim();
        if pair.is_empty() {
            continue;
        }

        let parts: Vec<&str> = pair.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err(TomlError::InvalidContextExpression {
                expression: input.to_string(),
                reason: format!("Invalid key=value pair: '{}'", pair),
            });
        }

        let key = parts[0].trim();
        let value_str = parts[1].trim();

        if value_str.is_empty() {
            return Err(TomlError::InvalidContextExpression {
                expression: input.to_string(),
                reason: format!("Empty value after equals in: '{}'", pair),
            });
        }

        // Validate dimension exists
        if !dimensions.contains_key(key) {
            return Err(TomlError::UndeclaredDimension {
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
fn parse_default_config(table: &toml::Table) -> Result<Map<String, Value>, TomlError> {
    let section = table
        .get("default-config")
        .ok_or_else(|| TomlError::MissingSection("default-config".into()))?
        .as_table()
        .ok_or_else(|| {
            TomlError::ConversionError("default-config must be a table".into())
        })?;

    let mut result = Map::new();
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

        let value = toml_value_to_serde_value(table["value"].clone());
        result.insert(key.clone(), value);
    }

    Ok(result)
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

        // Parse dimension type (optional, defaults to "regular")
        let dimension_type = if let Some(type_value) = table.get("type") {
            let type_str = type_value.as_str().ok_or_else(|| {
                TomlError::ConversionError(format!(
                    "dimensions.{}.type must be a string",
                    key
                ))
            })?;
            match type_str {
                "regular" => DimensionType::Regular {},
                "local_cohort" => {
                    // Local cohort requires a cohort field
                    let cohort = table.get("cohort").ok_or_else(|| {
                        TomlError::MissingField {
                            section: "dimensions".into(),
                            key: key.clone(),
                            field: "cohort".into(),
                        }
                    })?;
                    let cohort_name = cohort.as_str().ok_or_else(|| {
                        TomlError::ConversionError(format!(
                            "dimensions.{}.cohort must be a string",
                            key
                        ))
                    })?;
                    DimensionType::LocalCohort(cohort_name.to_string())
                }
                other => {
                    return Err(TomlError::ConversionError(format!(
                        "dimensions.{}.type must be 'regular' or 'local_cohort', got '{}'",
                        key, other
                    )));
                }
            }
        } else {
            DimensionType::Regular {}
        };

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
            dimension_type,
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

    Ok(result)
}

/// Parse the context section
fn parse_contexts(
    table: &toml::Table,
    default_config: &Map<String, Value>,
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
            override_config.insert(key.clone(), serde_value);
        }

        // Compute priority and hash
        let priority = compute_priority(&context_map, dimensions);
        let override_hash = hash(&serde_json::to_value(&override_config).unwrap());

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

/// Convert serde_json::Value to TOML representation string
fn value_to_toml(value: &Value) -> String {
    match value {
        Value::String(s) => {
            format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
        }
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Array(arr) => {
            let items: Vec<String> = arr.iter().map(|v| value_to_toml(v)).collect();
            format!("[{}]", items.join(", "))
        }
        Value::Object(obj) => {
            let items: Vec<String> = obj
                .iter()
                .map(|(k, v)| format!("{} = {}", k, value_to_toml(v)))
                .collect();
            format!("{{ {} }}", items.join(", "))
        }
        Value::Null => "null".to_string(),
    }
}

/// Convert Condition to context expression string (e.g., "city=Bangalore; vehicle_type=cab")
fn condition_to_string(condition: &Cac<Condition>) -> Result<String, TomlError> {
    // Clone the condition to get the inner Map
    let condition_inner = condition.clone().into_inner();

    let mut pairs: Vec<String> = condition_inner
        .iter()
        .map(|(key, value)| format!("{}={}", key, value_to_string_simple(value)))
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
        let toml_entry = format!(
            "{} = {{ value = {}, schema = {} }}\n",
            key,
            value_to_toml(value),
            schema
        );
        output.push_str(&toml_entry);
    }
    output.push('\n');

    // 2. Serialize [dimensions] section
    output.push_str("[dimensions]\n");
    let mut sorted_dims: Vec<_> = config.dimensions.iter().collect();
    sorted_dims.sort_by_key(|(_, info)| info.position);

    for (name, info) in sorted_dims {
        let schema_json = serde_json::to_value(&info.schema)
            .map_err(|e| TomlError::SerializationError(e.to_string()))?;

        // Serialize dimension type
        let type_field = match &info.dimension_type {
            DimensionType::Regular {} => r#"type = "regular""#.to_string(),
            DimensionType::LocalCohort(cohort_name) => {
                format!(r#"type = "local_cohort", cohort = "{}""#, cohort_name)
            }
            DimensionType::RemoteCohort(_) => {
                // Skip remote_cohort types as they're not supported in TOML
                continue;
            }
        };

        let toml_entry = format!(
            "{} = {{ position = {}, schema = {}, {} }}\n",
            name,
            info.position,
            value_to_toml(&schema_json),
            type_field
        );
        output.push_str(&toml_entry);
    }
    output.push('\n');

    // 3. Serialize [context.*] sections
    for context in &config.contexts {
        // Wrap Condition in Cac for condition_to_string
        let condition_cac = Cac::<Condition>::try_from(context.condition.clone())
            .map_err(|e| TomlError::InvalidContextCondition(e.to_string()))?;
        let condition_str = condition_to_string(&condition_cac)?;

        output.push_str(&format!("[context.\"{}\"]\n", condition_str));

        // DIAGNOSTIC: Print what we're looking for vs what's available
        let override_key = context.override_with_keys.get_key();
        if let Some(overrides) = config.overrides.get(override_key) {
            for (key, value) in overrides.clone() {
                output.push_str(&format!("{} = {}\n", key, value_to_toml(&value)));
            }
        }
        output.push('\n');
    }

    Ok(output)
}

#[cfg(test)]
mod serialization_tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_value_to_toml_string() {
        let val = Value::String("hello".to_string());
        assert_eq!(value_to_toml(&val), "\"hello\"");
    }

    #[test]
    fn test_value_to_toml_number() {
        let val = Value::Number(serde_json::Number::from(42));
        assert_eq!(value_to_toml(&val), "42");
    }

    #[test]
    fn test_value_to_toml_bool() {
        assert_eq!(value_to_toml(&Value::Bool(true)), "true");
        assert_eq!(value_to_toml(&Value::Bool(false)), "false");
    }

    #[test]
    fn test_value_to_toml_array() {
        let val = json!(["a", "b", "c"]);
        assert_eq!(value_to_toml(&val), "[\"a\", \"b\", \"c\"]");
    }

    #[test]
    fn test_value_to_toml_object() {
        let val = json!({"type": "string", "enum": ["a", "b"]});
        let result = value_to_toml(&val);
        assert!(result.contains("type = \"string\""));
        assert!(result.contains("enum = [\"a\", \"b\"]"));
    }

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
    fn test_value_to_toml_special_chars() {
        let val = Value::String("hello\"world".to_string());
        assert_eq!(value_to_toml(&val), r#""hello\"world""#);

        let val2 = Value::String("hello\\world".to_string());
        assert_eq!(value_to_toml(&val2), r#""hello\\world""#);
    }

    #[test]
    fn test_toml_round_trip_all_value_types() {
        let toml_str = r#"
[default-config]
string_val = { value = "hello", schema = { type = "string" } }
int_val = { value = 42, schema = { type = "integer" } }
float_val = { value = 3.14, schema = { type = "number" } }
bool_val = { value = true, schema = { type = "boolean" } }

[dimensions]
os = { position = 1, schema = { type = "string" } }

[context."os=linux"]
string_val = "world"
"#;

        let config = parse(toml_str).unwrap();
        let serialized = serialize_to_toml(&config).unwrap();
        let reparsed = parse(&serialized).unwrap();

        assert_eq!(config.default_configs, reparsed.default_configs);
    }

    #[test]
    fn test_value_to_toml_nested() {
        let val = json!({"outer": {"inner": "value"}});
        let result = value_to_toml(&val);
        assert!(result.contains("outer"));
        assert!(result.contains("inner"));
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
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" }, type = "local_cohort", cohort = "test_cohort" }

[context."os=linux"]
timeout = 60
"#;

        let config = parse(toml).unwrap();
        let serialized = serialize_to_toml(&config).unwrap();
        let reparsed = parse(&serialized).unwrap();

        assert!(serialized.contains(r#"type = "local_cohort""#));
        assert!(serialized.contains(r#"cohort = "test_cohort""#));
        assert_eq!(config.dimensions.len(), reparsed.dimensions.len());
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
    fn test_dimension_type_local_cohort_missing_cohort() {
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
        assert!(result.unwrap_err().to_string().contains("cohort"));
    }

    #[test]
    fn test_dimension_type_invalid() {
        let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
os = { position = 1, schema = { type = "string" }, type = "invalid_type" }

[context."os=linux"]
timeout = 60
"#;

        let result = parse(toml);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("regular"));
    }
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

            [context]
            "region=us-east" = { timeout = 60 }
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

            [context]
            "os=linux" = { port = 8080 }
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

            [context]
            "os=linux" = { timeout = 60 }
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
}
