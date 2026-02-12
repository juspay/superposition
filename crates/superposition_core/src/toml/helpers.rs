use std::collections::HashMap;

use bigdecimal::ToPrimitive;
use serde_json::{Map, Value};
use superposition_types::{
    Cac, Condition, Context, DefaultConfigsWithSchema, DimensionInfo, OverrideWithKeys,
    Overrides,
};
use toml::Value as TomlValue;

use crate::helpers::{calculate_context_weight, hash};
use crate::{validations, TomlError};

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

pub fn create_connections_with_dependents(
    cohorted_dimension: &str,
    dimension_name: &str,
    dimensions: &mut HashMap<String, DimensionInfo>,
) {
    for (dim, dim_info) in dimensions.iter_mut() {
        if dim == cohorted_dimension
            && !dim_info.dependency_graph.contains_key(cohorted_dimension)
        {
            dim_info
                .dependency_graph
                .insert(cohorted_dimension.to_string(), vec![]);
        }
        if let Some(current_deps) = dim_info.dependency_graph.get_mut(cohorted_dimension)
        {
            current_deps.push(dimension_name.to_string());
            dim_info
                .dependency_graph
                .insert(dimension_name.to_string(), vec![]);
        }
    }
}

pub fn validate_context_dimension(
    dimension_info: &DimensionInfo,
    key: &str,
    value: &Value,
    index: usize,
) -> Result<(), TomlError> {
    validations::validate_against_schema(value, &Value::from(&dimension_info.schema))
        .map_err(|errors: Vec<String>| TomlError::ValidationError {
            key: format!("context[{}]._context_.{}", index, key),
            errors: validations::format_validation_errors(&errors),
        })?;

    Ok(())
}

pub fn validate_context(
    condition: &Map<String, Value>,
    dimensions: &HashMap<String, DimensionInfo>,
    index: usize,
) -> Result<(), TomlError> {
    for (key, value) in condition {
        let dimension_info =
            dimensions
                .get(key)
                .ok_or_else(|| TomlError::UndeclaredDimension {
                    dimension: key.clone(),
                    context: format!("[{}]", index),
                })?;

        validate_context_dimension(dimension_info, key, value, index)?;
    }

    Ok(())
}

pub fn validate_config_key(
    key: &str,
    value: &Value,
    schema: &Value,
    index: usize,
) -> Result<(), TomlError> {
    validations::validate_against_schema(value, schema).map_err(
        |errors: Vec<String>| TomlError::ValidationError {
            key: format!("context[{}].{}", index, key),
            errors: validations::format_validation_errors(&errors),
        },
    )?;

    Ok(())
}

pub fn validate_overrides(
    overrides: &Map<String, Value>,
    default_configs: &DefaultConfigsWithSchema,
    index: usize,
) -> Result<(), TomlError> {
    for (key, value) in overrides {
        let config_info =
            default_configs
                .get(key)
                .ok_or_else(|| TomlError::InvalidOverrideKey {
                    key: key.clone(),
                    context: format!("[{}]", index),
                })?;

        validate_config_key(key, value, &config_info.schema, index)?;
    }

    Ok(())
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

pub fn build_context(
    condition: Condition,
    overrides: Overrides,
    dimensions: &HashMap<String, DimensionInfo>,
) -> Result<(Context, String, Overrides), TomlError> {
    let override_hash = hash(&Value::Object(
        overrides
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect(),
    ));
    let condition_hash = hash(&Value::Object(
        condition
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect(),
    ));

    let priority = calculate_context_weight(&condition, dimensions)
        .map_err(|e| TomlError::ConversionError(e.to_string()))?
        .to_i32()
        .ok_or_else(|| {
            TomlError::ConversionError("Failed to convert context weight to i32".into())
        })?;

    let context = Context {
        condition,
        id: condition_hash,
        priority,
        override_with_keys: OverrideWithKeys::new(override_hash.clone()),
        weight: 0,
    };

    Ok((context, override_hash, overrides))
}
