//! Shared JSON schema validation utilities
//!
//! This module provides validation functions that can be used across
//! the codebase for validating values against JSON schemas.

use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{json, Value};
use service_utils::helpers::validation_err_to_str;

/// Compile a JSON schema for validation
///
/// # Arguments
/// * `schema` - The JSON schema to compile
///
/// # Returns
/// * `Ok(JSONSchema)` - Compiled schema ready for validation
/// * `Err(String)` - Compilation error message
pub fn compile_schema(schema: &Value) -> Result<JSONSchema, String> {
    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(schema)
        .map_err(|e| e.to_string())
}

/// Validate a value against a pre-compiled JSON schema
///
/// # Arguments
/// * `value` - The value to validate
/// * `compiled_schema` - The pre-compiled JSONSchema
///
/// # Returns
/// * `Ok(())` if validation succeeds
/// * `Err(Vec<String>)` containing validation error messages
pub fn validate_against_compiled_schema(
    value: &Value,
    compiled_schema: &JSONSchema,
) -> Result<(), Vec<String>> {
    compiled_schema
        .validate(value)
        .map_err(|errors| errors.map(|e| e.to_string()).collect())
}

/// Validate a value against a raw JSON schema (compiles and validates)
///
/// This is a convenience function that combines compilation and validation.
/// Use this when you don't need to distinguish between compilation and validation errors.
///
/// # Arguments
/// * `value` - The value to validate
/// * `schema` - The JSON schema to validate against
///
/// # Returns
/// * `Ok(())` if validation succeeds
/// * `Err(Vec<String>)` containing all error messages (compilation + validation)
pub fn validate_against_schema(value: &Value, schema: &Value) -> Result<(), Vec<String>> {
    let compiled_schema = compile_schema(schema).map_err(|e| vec![e])?;
    validate_against_compiled_schema(value, &compiled_schema)
}

/// Validate that a JSON schema is well-formed
///
/// This function checks that a schema can be compiled and passes basic
/// structural validation against a meta-schema.
///
/// # Arguments
/// * `schema` - The JSON schema to validate
///
/// # Returns
/// * `Ok(())` if the schema is valid
/// * `Err(Vec<String>)` containing validation error messages
pub fn validate_schema(schema: &Value) -> Result<(), Vec<String>> {
    // Use the new compile function
    compile_schema(schema).map_err(|e| vec![e])?;

    // Then validate against the meta-schema
    let meta_schema = get_meta_schema();
    meta_schema
        .validate(schema)
        .map_err(|errors| errors.map(|e| e.to_string()).collect())
}

/// Validate the structure of a cohort schema
///
/// This function validates that a cohort schema has the required structure:
/// - `type` field with value "string"
/// - `enum` field with an array of string values
/// - `definitions` field with jsonlogic rules
/// - `enum` must contain "otherwise"
/// - `definitions` keys must match `enum` values (except "otherwise")
/// - `definitions` must not be empty
///
/// Note: This function does NOT compile the schema as JSON Schema because
/// cohort schemas contain jsonlogic rules in the `definitions` field which
/// are not valid JSON Schema syntax.
///
/// # Arguments
/// * `schema` - The cohort schema to validate
///
/// # Returns
/// * `Ok(())` if the schema structure is valid
/// * `Err(Vec<String>)` containing validation error messages
pub fn validate_cohort_schema_structure(schema: &Value) -> Result<(), Vec<String>> {
    // Get the cohort meta-schema
    let cohort_meta_schema = get_cohort_meta_schema();
    cohort_meta_schema.validate(schema).map_err(|e| {
        let verrors = e.collect::<Vec<ValidationError>>();
        vec![format!(
            "schema validation failed: {}",
            validation_err_to_str(verrors)
                .first()
                .unwrap_or(&String::new())
        )]
    })?;

    // Extract enum options
    let enum_options = schema
        .get("enum")
        .and_then(|v| v.as_array())
        .ok_or_else(|| {
            vec!["Cohort schema must have an 'enum' field of type array".to_string()]
        })?
        .iter()
        .filter_map(|v| v.as_str().map(str::to_string))
        .collect::<Vec<String>>();

    // Check that "otherwise" is in the enum
    if !enum_options.contains(&"otherwise".to_string()) {
        return Err(vec![
            "Cohort schema enum must contain 'otherwise' as an option".to_string(),
        ]);
    }

    // Get definitions
    let definitions = schema
        .get("definitions")
        .and_then(|v| v.as_object())
        .ok_or_else(|| {
            vec![
                "Cohort schema must have a 'definitions' field with jsonlogic rules"
                    .to_string(),
            ]
        })?;

    // Check definitions is not empty
    if definitions.is_empty() {
        return Err(vec![
            "Cohort schema definitions must not be empty".to_string()
        ]);
    }

    // Check that each definition key is in the enum (except "otherwise" which should not be in definitions)
    for key in definitions.keys() {
        if !enum_options.contains(key) {
            return Err(vec![format!(
                "Cohort definition '{}' does not have a corresponding enum option",
                key
            )]);
        }
        if key == "otherwise" {
            return Err(vec![
                "Cohort definitions should not contain 'otherwise'".to_string()
            ]);
        }
    }

    // Check that all enum options (except "otherwise") have definitions
    for option in &enum_options {
        if option != "otherwise" && !definitions.contains_key(option) {
            return Err(vec![format!(
                "Cohort enum option '{}' does not have a corresponding definition",
                option
            )]);
        }
    }

    Ok(())
}

/// Get the meta-schema for validating cohort schema definitions
///
/// This schema validates that a cohort schema has the required structure
/// with `type`, `enum`, and `definitions` fields.
///
/// # Returns
/// A compiled JSONSchema for cohort meta-validation
pub fn get_cohort_meta_schema() -> JSONSchema {
    let meta_schema = json!({
        "type": "object",
        "properties": {
            "type": { "type": "string" },
            "enum": {
                "type": "array",
                "items": { "type": "string" },
                "contains": { "const": "otherwise" },
                "minContains": 1,
                "uniqueItems": true
            },
            "definitions": {
                "type": "object",
                "not": {
                    "required": ["otherwise"]
                }
            }
        },
        "required": ["type", "enum", "definitions"]
    });

    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&meta_schema)
        .expect("Failed to compile cohort meta-schema")
}

/// Format validation errors into a human-readable string
///
/// # Arguments
/// * `errors` - Slice of validation error strings
///
/// # Returns
/// A semicolon-separated string of error messages
pub fn format_validation_errors(errors: &[String]) -> String {
    errors.join("; ")
}

/// Get the meta-schema for validating schema definitions
///
/// This schema validates that a schema definition is valid according to
/// the subset of JSON Schema features supported by the system.
///
/// # Returns
/// A compiled JSONSchema for meta-validation
pub fn get_meta_schema() -> JSONSchema {
    let meta_schema = json!({
        "type": "object",
        "properties": {
            "type": {
                "enum": ["boolean", "number", "integer", "string", "array", "null", "object"]
            },
        },
        "required": ["type"],
    });

    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&meta_schema)
        .expect("Failed to compile meta-schema")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_valid_string() {
        let value = json!("hello");
        let schema = json!({ "type": "string" });

        let result = validate_against_schema(&value, &schema);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_invalid_string() {
        let value = json!(42);
        let schema = json!({ "type": "string" });

        let result = validate_against_schema(&value, &schema);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_validate_valid_integer() {
        let value = json!(42);
        let schema = json!({ "type": "integer" });

        let result = validate_against_schema(&value, &schema);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_invalid_integer() {
        let value = json!("42");
        let schema = json!({ "type": "integer" });

        let result = validate_against_schema(&value, &schema);
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_with_enum() {
        let value = json!("linux");
        let schema = json!({
            "type": "string",
            "enum": ["linux", "windows", "macos"]
        });

        let result = validate_against_schema(&value, &schema);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_with_enum_invalid() {
        let value = json!("freebsd");
        let schema = json!({
            "type": "string",
            "enum": ["linux", "windows", "macos"]
        });

        let result = validate_against_schema(&value, &schema);
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_with_minimum() {
        let value = json!(10);
        let schema = json!({
            "type": "integer",
            "minimum": 5
        });

        let result = validate_against_schema(&value, &schema);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_with_minimum_invalid() {
        let value = json!(3);
        let schema = json!({
            "type": "integer",
            "minimum": 5
        });

        let result = validate_against_schema(&value, &schema);
        assert!(result.is_err());
    }

    #[test]
    fn test_format_validation_errors() {
        let schema = json!({ "type": "integer" });
        let value = json!("not an integer");

        let result = validate_against_schema(&value, &schema);
        assert!(result.is_err());

        let errors = result.unwrap_err();
        let formatted = format_validation_errors(&errors);
        assert!(!formatted.is_empty());
    }

    #[test]
    fn test_get_meta_schema() {
        let meta_schema = get_meta_schema();
        let valid_schema = json!({ "type": "string" });

        let result = meta_schema.validate(&valid_schema);
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_meta_schema_invalid() {
        let meta_schema = get_meta_schema();
        let invalid_schema = json!({ "type": "invalid_type" });

        let result = meta_schema.validate(&invalid_schema);
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_schema_valid() {
        let schema = json!({ "type": "string" });
        assert!(validate_schema(&schema).is_ok());
    }

    #[test]
    fn test_validate_schema_with_constraints() {
        let schema = json!({
            "type": "integer",
            "minimum": 0,
            "maximum": 100
        });
        assert!(validate_schema(&schema).is_ok());
    }

    #[test]
    fn test_validate_schema_invalid_type() {
        let schema = json!({ "type": "invalid_type" });
        assert!(validate_schema(&schema).is_err());
    }

    #[test]
    fn test_validate_schema_missing_type() {
        let schema = json!({ "minimum": 0 });
        assert!(validate_schema(&schema).is_err());
    }

    #[test]
    fn test_validate_schema_invalid_syntax() {
        let schema = json!({
            "type": "integer",
            "minimum": "not_a_number"
        });
        assert!(validate_schema(&schema).is_err());
    }

    #[test]
    fn test_validate_cohort_schema_structure_valid() {
        let schema = json!({
            "type": "string",
            "enum": ["cohort1", "cohort2", "otherwise"],
            "definitions": {
                "cohort1": {"==": [{"var": "os"}, "linux"]},
                "cohort2": {"==": [{"var": "os"}, "windows"]}
            }
        });
        assert!(validate_cohort_schema_structure(&schema).is_ok());
    }

    #[test]
    fn test_validate_cohort_schema_structure_missing_enum() {
        let schema = json!({
            "type": "string",
            "definitions": {
                "cohort1": {"==": [{"var": "os"}, "linux"]}
            }
        });
        assert!(validate_cohort_schema_structure(&schema).is_err());
    }

    #[test]
    fn test_validate_cohort_schema_structure_missing_otherwise() {
        let schema = json!({
            "type": "string",
            "enum": ["cohort1", "cohort2"],
            "definitions": {
                "cohort1": {"==": [{"var": "os"}, "linux"]},
                "cohort2": {"==": [{"var": "os"}, "windows"]}
            }
        });
        assert!(validate_cohort_schema_structure(&schema).is_err());
    }

    #[test]
    fn test_validate_cohort_schema_structure_missing_definitions() {
        let schema = json!({
            "type": "string",
            "enum": ["cohort1", "otherwise"]
        });
        assert!(validate_cohort_schema_structure(&schema).is_err());
    }

    #[test]
    fn test_validate_cohort_schema_structure_empty_definitions() {
        let schema = json!({
            "type": "string",
            "enum": ["otherwise"],
            "definitions": {}
        });
        assert!(validate_cohort_schema_structure(&schema).is_err());
    }

    #[test]
    fn test_validate_cohort_schema_structure_definition_not_in_enum() {
        let schema = json!({
            "type": "string",
            "enum": ["cohort1", "otherwise"],
            "definitions": {
                "cohort1": {"==": [{"var": "os"}, "linux"]},
                "cohort2": {"==": [{"var": "os"}, "windows"]}
            }
        });
        assert!(validate_cohort_schema_structure(&schema).is_err());
    }

    #[test]
    fn test_validate_cohort_schema_structure_enum_option_not_in_definitions() {
        let schema = json!({
            "type": "string",
            "enum": ["cohort1", "cohort2", "otherwise"],
            "definitions": {
                "cohort1": {"==": [{"var": "os"}, "linux"]}
            }
        });
        assert!(validate_cohort_schema_structure(&schema).is_err());
    }

    #[test]
    fn test_validate_cohort_schema_structure_otherwise_in_definitions() {
        let schema = json!({
            "type": "string",
            "enum": ["cohort1", "otherwise"],
            "definitions": {
                "cohort1": {"==": [{"var": "os"}, "linux"]},
                "otherwise": {"==": [{"var": "os"}, "macos"]}
            }
        });
        assert!(validate_cohort_schema_structure(&schema).is_err());
    }
}
