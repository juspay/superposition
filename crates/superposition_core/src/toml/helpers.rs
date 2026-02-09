use std::collections::HashMap;

use serde_json::{Map, Value};
use superposition_types::{DefaultConfigsWithSchema, DimensionInfo};

use crate::{validations, TomlError};

pub(super) fn create_connections_with_dependents(
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

fn needs_quoting(s: &str) -> bool {
    s.chars()
        .any(|c| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
}

pub(super) fn inline_table<T: IntoIterator<Item = (String, Value)>>(
    map: T,
    key: &str,
) -> Result<String, TomlError> {
    let parts = map
        .into_iter()
        .map(|(k, v)| {
            let k = if needs_quoting(&k) {
                format!("\"{k}\"") // Quote keys that need it
            } else {
                k
            };
            to_toml_string(v, &format!("{key}.{k}"))
                .map(|v_str| format!("{k} = {}", v_str.trim()))
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(format!("{{ {} }}", parts.join(", ")))
}

/// Convert serde_json::Value to toml::Value - return None for NULL
pub(super) fn to_toml_string(json: Value, key: &str) -> Result<String, TomlError> {
    match json {
        Value::Null => Err(TomlError::NullValueInConfig(key.to_string())),
        Value::Bool(b) => Ok(b.to_string()),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(i.to_string())
            } else if let Some(f) = n.as_f64() {
                Ok(f.to_string())
            } else {
                Ok(n.to_string())
            }
        }
        Value::String(s) => Ok(s.to_string()),
        Value::Array(arr) => {
            let str_arr = arr
                .into_iter()
                .enumerate()
                .map(|(i, v)| to_toml_string(v, &format!("{}[{}]", key, i)))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(format!("[{}]", str_arr.join(", ")))
        }

        Value::Object(obj) => inline_table(obj, key),
    }
}

pub(super) fn validate_context_dimension(
    dimension_info: &DimensionInfo,
    key: &str,
    value: &Value,
    index: usize,
) -> Result<(), TomlError> {
    validations::validate_against_schema(&value, &Value::from(&dimension_info.schema))
        .map_err(|errors: Vec<String>| TomlError::ValidationError {
            key: format!("context[{}]._condition_.{}", index, key),
            errors: validations::format_validation_errors(&errors),
        })?;

    Ok(())
}

pub(super) fn validate_context(
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

pub(super) fn validate_config_key(
    key: &str,
    value: &Value,
    schema: &Value,
    index: usize,
) -> Result<(), TomlError> {
    validations::validate_against_schema(&value, &schema).map_err(
        |errors: Vec<String>| TomlError::ValidationError {
            key: format!("context[{}].{}", index, key),
            errors: validations::format_validation_errors(&errors),
        },
    )?;

    Ok(())
}

pub(super) fn validate_overrides(
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
