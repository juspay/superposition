use actix_web::http::header::{HeaderMap, HeaderName, HeaderValue};
use itertools::{self, Itertools};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{json, Value};
use service_utils::{
    helpers::validation_err_to_str, result as superposition, validation_error,
};
use std::collections::HashMap;

pub fn get_default_config_validation_schema() -> JSONSchema {
    let my_schema = json!(
    {
        "type": "object",
        "properties": {
          "type": {
            "anyOf": [
              {
                "type": "null"
              },
              {
                "type": "string"
              },
              {
                "type": "object"
              },
              {
                "type": "number"
              },
              {
                "type": "boolean"
              },
              {
                "type": "array"
              }
            ]
          }
        },
        "required": [
          "type"
        ],
        "allOf": [
          {
            "if": {
              "properties": {
                "type": {
                  "const": "string"
                }
              }
            },
            "then": {
                "oneOf": [
                    {
                        "required": ["pattern"],
                        "properties": { "pattern": { "type": "string" } }
                    },
                    {
                        "required": ["enum"],
                        "properties": {
                            "enum": {
                                "type": "array",
                                "contains": { "type": "string" },
                                "minContains": 1
                            },
                        }
                    }
                ]
            }
          }
          // TODO: Add validations for Array types.
        ]
      });

    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&my_schema)
        .expect("THE IMPOSSIBLE HAPPENED, failed to compile the schema for the schema!")
}

pub fn parse_headermap_safe(headermap: &HeaderMap) -> HashMap<String, String> {
    let mut req_headers = HashMap::new();
    let record_header = |(header_name, header_val): (&HeaderName, &HeaderValue)| {
        let header_val = match header_val.to_str() {
            Ok(s) => String::from(s),
            Err(e) => {
                log::error!(
                    "unable to parse value of header {}, error: {e}",
                    header_name
                );
                String::from("Error: non ASCII header value")
            }
        };
        req_headers.insert(header_name.to_string(), header_val);
    };
    headermap.iter().for_each(record_header);
    req_headers
}

pub fn get_meta_schema() -> JSONSchema {
    let my_schema = json!({
        "type": "object",
        "properties": {
            "type": {
                "enum": ["boolean", "number", "string"]
            },
        },
        "required": ["type"],

        // # Add extra validation if needed for other primitive data types
        "if": {
            "properties": { "type": { "const": "string" } }
        }
        , "then": {
            "oneOf": [
                {
                    "required": ["pattern"],
                    "properties": { "pattern": { "type": "string" } }
                },
                {
                    "required": ["enum"],
                    "properties": {
                        "enum": {
                            "type": "array",
                            "contains": { "type": "string" },
                            "minContains": 1
                        },
                    }
                }
            ]
        }
    });

    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&my_schema)
        .expect("Error encountered: Failed to compile 'context_dimension_schema_value'. Ensure it adheres to the correct format and data type.")
}

pub fn validate_context_jsonschema(
    object_key: &str,
    dimension_value: &Value,
    dimension_schema: &JSONSchema,
) -> superposition::Result<()> {
    match dimension_value {
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

/*
  This step is required because an empty object
  is also a valid JSON schema. So added required
  validations for the input.
*/
// TODO: Recursive validation.

pub fn validate_jsonschema(
    validation_schema: &JSONSchema,
    schema: &Value,
) -> superposition::Result<()> {
    let res = match validation_schema.validate(schema) {
        Ok(_) => Ok(()),
        Err(e) => {
            //TODO: Try & render as json.
            let verrors = e.collect::<Vec<ValidationError>>();
            Err(validation_error!(
                "schema validation failed: {}",
                validation_err_to_str(verrors)
                    .first()
                    .unwrap_or(&String::new())
            ))
        }
    };
    res
}

pub fn json_to_sorted_string(v: &Value) -> String {
    match v {
        Value::Object(m) => {
            let mut new_str: String = String::from("");
            for (i, val) in m.iter().sorted_by_key(|item| item.0) {
                let p: String = json_to_sorted_string(val);
                new_str.push_str(i);
                new_str.push_str(&String::from(":"));
                new_str.push_str(&p);
                new_str.push_str(&String::from("$"));
            }
            new_str
        }
        Value::String(m) => m.to_string(),
        Value::Number(m) => m.to_string(),
        Value::Bool(m) => m.to_string(),
        Value::Null => String::from("null"),
        Value::Array(m) => {
            let mut new_vec =
                m.iter().map(json_to_sorted_string).collect::<Vec<String>>();
            new_vec.sort();
            new_vec.join(",")
        }
    }
}

pub fn calculate_context_priority(
    _object_key: &str,
    cond: &Value,
    dimension_schema_map: &HashMap<String, (JSONSchema, i32)>,
) -> Result<i32, String> {
    let get_priority = |key: &str, val: &Value| -> Result<i32, String> {
        if key == "var" {
            let dimension_name =
                val.as_str().ok_or("failed to decode dimension as str")?;
            dimension_schema_map
                .get(dimension_name)
                .map(|(_, priority)| priority)
                .ok_or(String::from(
                    "No matching `dimension` found in dimension table",
                ))
                .copied()
        } else {
            calculate_context_priority(key, val, dimension_schema_map)
        }
    };

    match cond {
        Value::Object(x) => x.iter().try_fold(0, |acc, (key, val)| {
            get_priority(key, val).map(|res| res + acc)
        }),
        Value::Array(arr) => arr.iter().try_fold(0, |acc, item| {
            calculate_context_priority(_object_key, item, dimension_schema_map)
                .map(|res| res + acc)
        }),
        _ => Ok(0),
    }
}

// ************ Tests *************

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_get_meta_schema() {
        let x = get_meta_schema();

        let ok_string_schema = json!({"type": "string", "pattern": ".*"});
        let ok_string_validation = x.validate(&ok_string_schema);
        assert!(ok_string_validation.is_ok());

        let error_string_schema = json!({"type": "string"});
        let error_string_validation = x.validate(&error_string_schema).map_err(|e| {
            let verrors = e.collect::<Vec<ValidationError>>();
            format!(
                "Error While validating string dataType, Bad schema: {:?}",
                verrors.as_slice()
            )
        });
        assert!(error_string_validation.is_err_and(|error| error.contains("Bad schema")));

        let error_object_schema = json!({"type": "object"});
        let error_object_validation = x.validate(&error_object_schema).map_err(|e| {
            let verrors = e.collect::<Vec<ValidationError>>();
            format!(
                "Error While validating object dataType, Bad schema: {:?}",
                verrors.as_slice()
            )
        });
        assert!(error_object_validation.is_err_and(|error| error.contains("Bad schema")));

        let ok_enum_schema = json!({"type": "string", "enum": ["ENUMVAL"]});
        let ok_enum_validation = x.validate(&ok_enum_schema);
        assert!(ok_enum_validation.is_ok());
    }

    #[test]
    fn test_json_to_sorted_string() {
        let first_condition: Value = json!({
            "and": [
                {
                    "==": [
                        {
                            "var": "os"
                        },
                        "android"
                    ]
                },
                {
                    "==": [
                        {
                            "var": "clientId"
                        },
                        "geddit"
                    ]
                }
            ]
        });

        let second_condition: Value = json!({
            "and": [
                {
                    "==": [
                        {
                            "var": "clientId"
                        },
                        "geddit"
                    ]
                },
                {
                    "==": [
                        {
                            "var": "os"
                        },
                        "android"
                    ]
                }
            ]
        });
        let expected_string: String =
            "and:==:android,var:os$$,==:geddit,var:clientId$$$".to_owned();
        assert_eq!(json_to_sorted_string(&first_condition), expected_string);
        assert_eq!(
            json_to_sorted_string(&first_condition),
            json_to_sorted_string(&second_condition)
        );
    }

    #[test]
    fn test_validate_context_jsonschema() {
        let test_schema = json!({
            "type": "string",
            "pattern": ".*"
        });
        let test_jsonschema = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&test_schema)
        .expect("Error encountered: Failed to compile 'context_dimension_schema_value'. Ensure it adheres to the correct format and data type.");

        let str_dimension_val = json!("string1".to_owned());
        let arr_dimension_val = json!(["string1".to_owned(), "string2".to_owned()]);
        let ok_str_context =
            validate_context_jsonschema("in", &str_dimension_val, &test_jsonschema);
        let ok_arr_context =
            validate_context_jsonschema("in", &arr_dimension_val, &test_jsonschema);
        let err_arr_context =
            match validate_context_jsonschema("==", &arr_dimension_val, &test_jsonschema)
            {
                Err(superposition::AppError::ValidationError(err)) => {
                    log::info!("{:?}", err);
                    true
                }
                _ => false,
            };

        assert!(ok_str_context.is_ok());
        assert!(err_arr_context);
        assert!(ok_arr_context.is_ok());
    }
}
