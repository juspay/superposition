use actix_web::http::header::{HeaderMap, HeaderName, HeaderValue};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{json, Value};
use std::collections::HashMap;

pub fn get_default_config_validation_schema() -> JSONSchema {
    let my_schema = json!({
        "type": "object",
        "properties": {
            "type": {
            "enum": ["string", "object", "enum", "number", "boolean", "array"]
            }
        },
        "required": ["type"],
        "allOf": [
    //        {
    //        "if": {
    //            "properties": { "type": { "const": "object" } }
    //        },
    //        "then": {
    //            "properties": { "properties": { "type": "object" } },
    //            "required": ["properties"]
    //        }
    //        },
            {
            "if": {
                "properties": { "type": { "const": "string" } }
            },
            "then": {
                "properties": { "pattern": { "type": "string" } },
                "required": ["pattern"]
            }
            },
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

/*
  This step is required because an empty object
  is also a valid JSON schema. So added required
  validations for the input.
*/
// TODO: Recursive validation.
pub fn validate_jsonschema(
    validation_schema: &JSONSchema,
    schema: &Value,
) -> Result<(), String> {
    let res = match validation_schema.validate(schema) {
        Ok(_) => Ok(()),
        Err(e) => {
            //TODO: Try & render as json.
            let verrors = e.collect::<Vec<ValidationError>>();
            Err(String::from(format!(
                "Bad schema: {:?}",
                verrors.as_slice()
            )))
        }
    };
    res
}

// ************ Tests *************

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_get_meta_schema() {
        let x = get_meta_schema();
        let ok_string_validation = x
            .validate(&json!({"type": "string", "pattern": ".*"}))
            .map_err(|e| {
                let verrors = e.collect::<Vec<ValidationError>>();
                String::from(format!("Bad schema: {:?}", verrors.as_slice()))
            });
        let error_string_validation =
            match x.validate(&json!({"type": "string"})).map_err(|e| {
                let verrors = e.collect::<Vec<ValidationError>>();
                String::from(format!(
                    "Error While validating string dataType, Bad schema: {:?}",
                    verrors.as_slice()
                ))
            }) {
                Ok(()) => false,
                Err(err_str) => err_str.contains("Bad schema"),
            };

        let error_object_validation =
            match x.validate(&json!({"type": "object"})).map_err(|e| {
                let verrors = e.collect::<Vec<ValidationError>>();
                String::from(format!(
                    "Error While validating object dataType, Bad schema: {:?}",
                    verrors.as_slice()
                ))
            }) {
                Ok(()) => false,
                Err(err_str) => err_str.contains("Bad schema"),
            };
        let ok_enum_validation = x
            .validate(&json!({"type": "string", "enum": ["ENUMVAL"]}))
            .map_err(|e| {
                let verrors = e.collect::<Vec<ValidationError>>();
                String::from(format!(
                    "Error While validating enum dataType, Bad schema: {:?}",
                    verrors.as_slice()
                ))
            });
        assert_eq!(ok_enum_validation, Ok(()));
        assert_eq!(error_object_validation, true);
        assert_eq!(ok_string_validation, Ok(()));
        assert_eq!(error_string_validation, true);
    }
}
