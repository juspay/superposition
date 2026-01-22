use std::collections::HashMap;

use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{Map, Value};
use service_utils::{helpers::validation_err_to_str, service::types::SchemaName};
use superposition_macros::{bad_argument, validation_error};
use superposition_types::{DBConnection, DimensionInfo, database::schema, result};

pub fn validate_override_with_default_configs(
    conn: &mut DBConnection,
    override_: &Map<String, Value>,
    schema_name: &SchemaName,
) -> result::Result<()> {
    let keys_array: Vec<&String> = override_.keys().collect();
    let res: Vec<(String, Value)> = schema::default_configs::dsl::default_configs
        .filter(schema::default_configs::dsl::key.eq_any(keys_array))
        .select((
            schema::default_configs::dsl::key,
            schema::default_configs::dsl::schema,
        ))
        .schema_name(schema_name)
        .get_results::<(String, Value)>(conn)?;

    let map = Map::from_iter(res);

    for (key, value) in override_.iter() {
        let schema = map
            .get(key)
            .ok_or(bad_argument!("failed to get schema for config key {}", key))?;
        let instance = value;
        let schema_compile_result = JSONSchema::options()
            .with_draft(Draft::Draft7)
            .compile(schema);
        let jschema = match schema_compile_result {
            Ok(jschema) => jschema,
            Err(e) => {
                log::info!("Failed to compile as a Draft-7 JSON schema: {e}");
                return Err(bad_argument!(
                    "failed to compile ({}) config key schema",
                    key
                ));
            }
        };
        if let Err(e) = jschema.validate(instance) {
            let verrors = e.collect::<Vec<ValidationError>>();
            log::error!("({key}) config key validation error: {:?}", verrors);
            return Err(validation_error!(
                "schema validation failed for {key}: {}",
                validation_err_to_str(verrors)
                    .first()
                    .unwrap_or(&String::new())
            ));
        };
    }

    Ok(())
}

pub fn validate_dimensions(
    cond: &Map<String, Value>,
    dimension_schema_map: &HashMap<String, DimensionInfo>,
) -> result::Result<()> {
    for (dimension, value) in cond.iter() {
        let dimension_data = dimension_schema_map
            .get(dimension)
            .ok_or(bad_argument!("No matching dimension ({}) found", dimension))?;

        let schema_value = Value::from(&dimension_data.schema);
        validate_context_jsonschema(value, &schema_value)?;
    }
    Ok(())
}

pub fn validate_context_jsonschema(
    dimension_value: &Value,
    dimension_schema: &Value,
) -> result::Result<()> {
    let dimension_schema = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(dimension_schema)
        .map_err(|e| {
            log::error!(
                "Failed to compile as a Draft-7 JSON schema: {}",
                e.to_string()
            );
            bad_argument!("Error encountered: invalid jsonschema for dimension.")
        })?;

    dimension_schema.validate(dimension_value).map_err(|e| {
        let verrors = e.collect::<Vec<ValidationError>>();
        log::error!(
            "failed to validate dimension value {}: {:?}",
            dimension_value.to_string(),
            verrors
        );
        validation_error!(
            "failed to validate dimension value {}: {}",
            dimension_value.to_string(),
            validation_err_to_str(verrors)
                .first()
                .unwrap_or(&String::new())
        )
    })
}

// ************ Tests *************

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn test_validate_context_jsonschema() {
        let test_schema = json!({
            "type": "string",
            "pattern": ".*"
        });

        let str_dimension_val = json!("string1".to_owned());
        let ok_str_context =
            validate_context_jsonschema(&str_dimension_val, &test_schema);

        assert!(ok_str_context.is_ok());
    }
}
