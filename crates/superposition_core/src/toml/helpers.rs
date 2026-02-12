use serde_json::{Map, Value};
use superposition_types::{Cac, Condition, Overrides};
use toml::Value as TomlValue;

use crate::TomlError;

/// Convert toml::Value to serde_json::Value for validation
pub fn toml_to_json(value: TomlValue) -> Value {
    match value {
        TomlValue::String(s) => Value::String(s),
        TomlValue::Integer(i) => Value::Number(i.into()),
        TomlValue::Float(f) => serde_json::Number::from_f64(f)
            .map(Value::Number)
            .unwrap_or(Value::Null),
        TomlValue::Boolean(b) => Value::Bool(b),
        TomlValue::Datetime(dt) => Value::String(dt.to_string()),
        TomlValue::Array(arr) => {
            Value::Array(arr.into_iter().map(toml_to_json).collect())
        }
        TomlValue::Table(table) => {
            let map: Map<String, Value> = table
                .into_iter()
                .map(|(k, v)| (k, toml_to_json(v)))
                .collect();
            Value::Object(map)
        }
    }
}

/// Check if a TOML key needs quoting
pub fn needs_quoting(key: &str) -> bool {
    key.chars()
        .any(|c| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
}

/// Format a TOML key with optional quoting
pub fn format_key(key: &str) -> String {
    if needs_quoting(key) {
        format!("\"{}\"", key.replace('"', r#"\"#))
    } else {
        key.to_string()
    }
}

/// Format a TOML value as a string for inline table usage
pub fn format_toml_value(value: &TomlValue) -> String {
    match value {
        TomlValue::String(s) => format!("\"{}\"", s.replace('"', r#"\"#)),
        TomlValue::Integer(i) => i.to_string(),
        TomlValue::Float(f) => f.to_string(),
        TomlValue::Boolean(b) => b.to_string(),
        TomlValue::Datetime(dt) => format!("\"{}\"", dt),
        TomlValue::Array(arr) => {
            let items: Vec<String> = arr.iter().map(format_toml_value).collect();
            format!("[{}]", items.join(", "))
        }
        TomlValue::Table(table) => {
            let entries: Vec<String> = table
                .iter()
                .map(|(k, v)| format!("{} = {}", format_key(k), format_toml_value(v)))
                .collect();
            format!("{{ {} }}", entries.join(", "))
        }
    }
}

pub fn context_toml_to_condition(ctx: &toml::Table) -> Result<Condition, TomlError> {
    let json = toml_to_json(TomlValue::Table(ctx.clone()));
    let map = match json {
        Value::Object(map) => map,
        _ => {
            return Err(TomlError::ConversionError(
                "Context must be an object".into(),
            ))
        }
    };
    Cac::<Condition>::try_from(map)
        .map(|cac| cac.into_inner())
        .map_err(|e| TomlError::ConversionError(format!("Invalid condition: {}", e)))
}

pub fn overrides_toml_to_map(overrides: &toml::Table) -> Result<Overrides, TomlError> {
    let json = toml_to_json(TomlValue::Table(overrides.clone()));
    let map = match json {
        Value::Object(map) => map,
        _ => {
            return Err(TomlError::ConversionError(
                "Overrides must be an object".into(),
            ))
        }
    };
    Cac::<Overrides>::try_from(map)
        .map(|cac| cac.into_inner())
        .map_err(|e| TomlError::ConversionError(format!("Invalid overrides: {}", e)))
}
