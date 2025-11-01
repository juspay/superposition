use std::collections::HashMap;

use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{Map, Value};
use service_utils::{helpers::validation_err_to_str, service::types::SchemaName};
use superposition_macros::{bad_argument, validation_error};
use superposition_types::{database::schema, result, DBConnection, DimensionInfo};

#[cfg(feature = "jsonlogic")]
use super::types::DimensionCondition;

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

#[cfg(feature = "jsonlogic")]
pub fn validate_dimensions(
    object_key: &str,
    cond: &Value,
    dimension_schema_map: &HashMap<String, DimensionInfo>,
) -> result::Result<()> {
    let check_dimension = |key: &String, val: &Value| -> result::Result<()> {
        if key == "var" {
            let dimension_name = val
                .as_str()
                .ok_or(bad_argument!("Dimension name should be of `String` type"))?;
            dimension_schema_map
                .get(dimension_name)
                .map(|_| Ok(()))
                .ok_or(bad_argument!(
                    "No matching dimension ({}) found",
                    dimension_name
                ))?
        } else {
            validate_dimensions(key, val, dimension_schema_map)
        }
    };

    match cond {
        Value::Object(x) => x
            .iter()
            .try_for_each(|(key, val)| check_dimension(key, val)),
        Value::Array(arr) => {
            let mut val: Option<Value> = None;
            let mut condition: Option<DimensionCondition> = None;
            for i in arr {
                if let (None, Ok(x)) = (
                    &condition,
                    serde_json::from_value::<DimensionCondition>(serde_json::json!(i)),
                ) {
                    condition = Some(x);
                } else if val.is_none() {
                    val = Some(i.clone());
                }

                if let (Some(_dimension_value), Some(_dimension_condition)) =
                    (&val, &condition)
                {
                    break;
                }
            }

            if let (Some(dimension_value), Some(dimension_condition)) = (val, condition) {
                let expected_dimension_name = dimension_condition.var;
                let dimension_data = dimension_schema_map
                    .get(&expected_dimension_name)
                    .ok_or(bad_argument!(
                    "No matching `dimension` {} in dimension table",
                    expected_dimension_name
                ))?;

                let schema_value = Value::from(&dimension_data.schema);
                validate_context_jsonschema(object_key, &dimension_value, &schema_value)?;
            }
            arr.iter().try_for_each(|x| {
                validate_dimensions(object_key, x, dimension_schema_map)
            })
        }
        _ => Ok(()),
    }
}

#[cfg(not(feature = "jsonlogic"))]
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
    #[cfg(feature = "jsonlogic")] object_key: &str,
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
    match dimension_value {
        #[cfg(feature = "jsonlogic")]
        Value::Array(val_arr) if object_key == "in" => {
            let mut verrors = Vec::new();
            val_arr.iter().for_each(|x| {
                dimension_schema
                    .validate(x)
                    .map_err(|e| {
                        verrors.append(&mut e.collect::<Vec<ValidationError>>());
                    })
                    .ok();
            });
            if verrors.is_empty() {
                Ok(())
            } else {
                // Check if the array as a whole validates, even with individual errors
                match dimension_schema.validate(dimension_value) {
                    Ok(()) => {
                        log::error!(
                            "Validation errors for individual dimensions, but array as a whole validates: {:?}",
                            verrors
                        );
                        Ok(())
                    }
                    Err(e) => {
                        verrors.append(&mut e.collect::<Vec<ValidationError>>());
                        log::error!(
                            "Validation errors for dimensions in array: {:?}",
                            verrors
                        );
                        Err(validation_error!(
                            "failed to validate dimension value {}: {}",
                            dimension_value.to_string(),
                            validation_err_to_str(verrors)
                                .first()
                                .unwrap_or(&String::new())
                        ))
                    }
                }
            }
        }
        _ => dimension_schema.validate(dimension_value).map_err(|e| {
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
        }),
    }
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
        #[cfg(feature = "jsonlogic")]
        let arr_dimension_val = json!(["string1".to_owned(), "string2".to_owned()]);
        let ok_str_context = validate_context_jsonschema(
            #[cfg(feature = "jsonlogic")]
            "in",
            &str_dimension_val,
            &test_schema,
        );
        #[cfg(feature = "jsonlogic")]
        let ok_arr_context =
            validate_context_jsonschema("in", &arr_dimension_val, &test_schema);
        #[cfg(feature = "jsonlogic")]
        let err_arr_context =
            match validate_context_jsonschema("==", &arr_dimension_val, &test_schema) {
                Err(result::AppError::ValidationError(err)) => {
                    log::info!("{:?}", err);
                    true
                }
                _ => false,
            };

        assert!(ok_str_context.is_ok());
        #[cfg(feature = "jsonlogic")]
        assert!(err_arr_context);
        #[cfg(feature = "jsonlogic")]
        assert!(ok_arr_context.is_ok());
    }
}
