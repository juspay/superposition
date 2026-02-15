//! Shared JSON schema validation utilities
//!
//! This module provides validation functions that can be used across
//! the codebase for validating values against JSON schemas.

use std::collections::HashMap;

use jsonschema::{error::ValidationErrorKind, Draft, JSONSchema, ValidationError};
use serde_json::{json, Map, Value};
use superposition_types::{DefaultConfigsWithSchema, DetailedConfig, DimensionInfo};

/// Error type for context and config validation
#[derive(Debug, Clone)]
pub enum ContextValidationError {
    /// Dimension not found in declared dimensions
    UndeclaredDimension { dimension: String },
    /// Config key not found in default configs
    InvalidOverrideKey { key: String },
    /// Schema validation failed
    ValidationError { key: String, errors: Vec<String> },
}

impl ContextValidationError {
    /// Get the key associated with this error
    pub fn key(&self) -> &str {
        match self {
            ContextValidationError::UndeclaredDimension { dimension } => dimension,
            ContextValidationError::InvalidOverrideKey { key } => key,
            ContextValidationError::ValidationError { key, .. } => key,
        }
    }

    /// Get validation error messages if this is a ValidationError
    pub fn errors(&self) -> Option<&[String]> {
        match self {
            ContextValidationError::ValidationError { errors, .. } => Some(errors),
            _ => None,
        }
    }
}

/// Compile a JSON schema for validation
///
/// # Arguments
/// * `schema` - The JSON schema to compile
///
/// # Returns
/// * `Ok(JSONSchema)` - Compiled schema ready for validation
/// * `Err(String)` - Compilation error message
pub fn try_into_jsonschema(schema: &Value) -> Result<JSONSchema, String> {
    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(schema)
        .map_err(|e| e.to_string())
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
    let compiled_schema = try_into_jsonschema(schema).map_err(|e| vec![e])?;
    compiled_schema
        .validate(value)
        .map_err(|errors| errors.map(|e| e.to_string()).collect())
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
    try_into_jsonschema(schema).map_err(|e| vec![e])?;

    // Then validate against the meta-schema
    let meta_schema = get_meta_schema().map_err(|e| vec![e])?;
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
pub fn validate_cohort_schema_structure(
    schema: &Value,
) -> Result<Vec<String>, Vec<String>> {
    // Get the cohort meta-schema
    let cohort_meta_schema = get_cohort_meta_schema().map_err(|e| vec![e])?;
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

    Ok(enum_options)
}

/// Get the meta-schema for validating cohort schema definitions
///
/// This schema validates that a cohort schema has the required structure
/// with `type`, `enum`, and `definitions` fields.
///
/// # Returns
/// * `Ok(JSONSchema)` - Compiled schema ready for validation
/// * `Err(String)` - Compilation error message
pub fn get_cohort_meta_schema() -> Result<JSONSchema, String> {
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

    try_into_jsonschema(&meta_schema)
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
/// * `Ok(JSONSchema)` - Compiled schema ready for validation
/// * `Err(String)` - Compilation error message
pub fn get_meta_schema() -> Result<JSONSchema, String> {
    let meta_schema = json!({
        "type": "object",
        "properties": {
            "type": {
                "enum": ["boolean", "number", "integer", "string", "array", "null", "object"]
            },
        },
        "required": ["type"],
    });

    try_into_jsonschema(&meta_schema)
}

/// Validate a context dimension value against its schema
///
/// # Arguments
/// * `dimension_info` - Information about the dimension including its schema
/// * `key` - The dimension key name
/// * `value` - The value to validate
///
/// # Returns
/// * `Ok(())` if validation succeeds
/// * `Err(Vec<ContextValidationError>)` containing validation errors
pub fn validate_context_dimension(
    dimension_info: &DimensionInfo,
    key: &str,
    value: &Value,
) -> Result<(), Vec<ContextValidationError>> {
    validate_against_schema(value, &Value::from(&dimension_info.schema)).map_err(
        |errors| {
            vec![ContextValidationError::ValidationError {
                key: key.to_string(),
                errors,
            }]
        },
    )
}

/// Validate a context (condition) against dimension schemas
///
/// # Arguments
/// * `condition` - The context condition as a map of dimension names to values
/// * `dimensions` - Map of dimension names to their information
///
/// # Returns
/// * `Ok(())` if validation succeeds
/// * `Err(Vec<ContextValidationError>)` containing validation errors
pub fn validate_context(
    condition: &Map<String, Value>,
    dimensions: &HashMap<String, DimensionInfo>,
) -> Result<(), Vec<ContextValidationError>> {
    let mut all_errors: Vec<ContextValidationError> = Vec::new();

    for (key, value) in condition {
        match dimensions.get(key) {
            Some(dimension_info) => {
                if let Err(errors) =
                    validate_context_dimension(dimension_info, key, value)
                {
                    all_errors.extend(errors);
                }
            }
            None => {
                all_errors.push(ContextValidationError::UndeclaredDimension {
                    dimension: key.clone(),
                });
            }
        }
    }

    if all_errors.is_empty() {
        Ok(())
    } else {
        Err(all_errors)
    }
}

/// Validate a config value against its schema
///
/// # Arguments
/// * `key` - The config key name
/// * `value` - The value to validate
/// * `schema` - The JSON schema to validate against
///
/// # Returns
/// * `Ok(())` if validation succeeds
/// * `Err(Vec<ContextValidationError>)` containing validation errors
pub fn validate_config_value(
    key: &str,
    value: &Value,
    schema: &Value,
) -> Result<(), Vec<ContextValidationError>> {
    validate_against_schema(value, schema).map_err(|errors| {
        vec![ContextValidationError::ValidationError {
            key: key.to_string(),
            errors,
        }]
    })
}

/// Validate that a cohort dimension's position is valid relative to its parent dimension
///
/// Cohort dimensions must have a position that is less than or equal to their
/// parent dimension's position. This ensures proper evaluation order.
///
/// # Arguments
/// * `dimension_info` - Information about the cohort dimension
/// * `cohort_dimension_info` - Information about the parent cohort dimension
///
/// # Returns
/// * `Ok(())` if the position is valid
/// * `Err(String)` containing an error message if the position is invalid
pub fn validate_cohort_dimension_position(
    granular_dimension_info: &DimensionInfo,
    coarse_dimension_info: &DimensionInfo,
) -> Result<(), String> {
    if granular_dimension_info.position < coarse_dimension_info.position {
        return Err(format!(
            "Coarse Dimension position {} should be less than the granular dimension position {}",
            coarse_dimension_info.position, granular_dimension_info.position
        ));
    }
    Ok(())
}

/// Validate overrides against default config schemas
///
/// # Arguments
/// * `overrides` - Map of override keys to values
/// * `default_configs` - Map of default config keys to their info (including schemas)
///
/// # Returns
/// * `Ok(())` if validation succeeds
/// * `Err(Vec<ContextValidationError>)` containing validation errors
pub fn validate_overrides(
    overrides: &Map<String, Value>,
    default_configs: &DefaultConfigsWithSchema,
) -> Result<(), Vec<ContextValidationError>> {
    let mut all_errors: Vec<ContextValidationError> = Vec::new();

    for (key, value) in overrides {
        match default_configs.get(key) {
            Some(config_info) => {
                if let Err(errors) =
                    validate_config_value(key, value, &config_info.schema)
                {
                    all_errors.extend(errors);
                }
            }
            None => {
                all_errors.push(ContextValidationError::InvalidOverrideKey {
                    key: key.clone(),
                });
            }
        }
    }

    if all_errors.is_empty() {
        Ok(())
    } else {
        Err(all_errors)
    }
}

/// Format jsonschema ValidationError instances into human-readable strings
///
/// This function converts jsonschema ValidationError instances into
/// human-readable error messages suitable for API responses and
/// TOML parsing error reporting.
///
/// # Arguments
/// * `errors` - Vector of ValidationError instances
///
/// # Returns
/// A vector of formatted error messages
pub fn validation_err_to_str(errors: Vec<ValidationError>) -> Vec<String> {
    errors.into_iter().map(|error| {
        match error.kind {
            ValidationErrorKind::AdditionalItems { limit } => {
                format!("input array contain more items than expected, limit is {limit}")
            }
            ValidationErrorKind::AdditionalProperties { unexpected } => {
                format!("unexpected properties `{}`", unexpected.join(", "))
            }
            ValidationErrorKind::AnyOf => {
                "not valid under any of the schemas listed in the 'anyOf' keyword".to_string()
            }
            ValidationErrorKind::BacktrackLimitExceeded { error: _ } => {
                "backtrack limit exceeded while matching regex".to_string()
            }
            ValidationErrorKind::Constant { expected_value } => {
                format!("value doesn't match expected constant `{expected_value}`")
            }
            ValidationErrorKind::Contains => {
                "array doesn't contain items conforming to the specified schema".to_string()
            }
            ValidationErrorKind::ContentEncoding { content_encoding } => {
                format!(
                    "value doesn't respect the defined contentEncoding `{content_encoding}`"
                )
            }
            ValidationErrorKind::ContentMediaType { content_media_type } => {
                format!(
                    "value doesn't respect the defined contentMediaType `{content_media_type}`"
                )
            }
            ValidationErrorKind::Enum { options } => {
                format!("value doesn't match any of specified options {}", options)
            }
            ValidationErrorKind::ExclusiveMaximum { limit } => {
                format!("value is too large, limit is {limit}")
            }
            ValidationErrorKind::ExclusiveMinimum { limit } => {
                format!("value is too small, limit is {limit}")
            }
            ValidationErrorKind::FalseSchema => {
                "everything is invalid for `false` schema".to_string()
            }
            ValidationErrorKind::FileNotFound { error: _ } => {
                "referenced file not found".to_string()
            }
            ValidationErrorKind::Format { format } => {
                format!("value doesn't match the specified format `{}`", format)
            }
            ValidationErrorKind::FromUtf8 { error: _ } => {
                "invalid UTF-8 data".to_string()
            }
            ValidationErrorKind::InvalidReference { reference } => {
                format!("`{}` is not a valid reference", reference)
            }
            ValidationErrorKind::InvalidURL { error } => {
                format!("invalid URL: {}", error)
            }
            ValidationErrorKind::JSONParse { error } => {
                format!("error parsing JSON: {}", error)
            }
            ValidationErrorKind::MaxItems { limit } => {
                format!("too many items in array, limit is {}", limit)
            }
            ValidationErrorKind::Maximum { limit } => {
                format!("value is too large, maximum is {}", limit)
            }
            ValidationErrorKind::MaxLength { limit } => {
                format!("string is too long, maximum length is {}", limit)
            }
            ValidationErrorKind::MaxProperties { limit } => {
                format!("too many properties in object, limit is {}", limit)
            }
            ValidationErrorKind::MinItems { limit } => {
                format!("not enough items in array, minimum is {}", limit)
            }
            ValidationErrorKind::Minimum { limit } => {
                format!("value is too small, minimum is {}", limit)
            }
            ValidationErrorKind::MinLength { limit } => {
                format!("string is too short, minimum length is {}", limit)
            }
            ValidationErrorKind::MinProperties { limit } => {
                format!("not enough properties in object, minimum is {}", limit)
            }
            ValidationErrorKind::MultipleOf { multiple_of } => {
                format!("value is not a multiple of {}", multiple_of)
            }
            ValidationErrorKind::Not { schema } => {
                format!("negated schema `{}` failed validation", schema)
            }
            ValidationErrorKind::OneOfMultipleValid => {
                "value is valid under more than one schema listed in the 'oneOf' keyword".to_string()
            }
            ValidationErrorKind::OneOfNotValid => {
                "value is not valid under any of the schemas listed in the 'oneOf' keyword".to_string()
            }
            ValidationErrorKind::Pattern { pattern } => {
                format!("value doesn't match the pattern `{}`", pattern)
            }
            ValidationErrorKind::PropertyNames { error } => {
                format!("object property names are invalid: {}", error)
            }
            ValidationErrorKind::Required { property } => {
                format!("required property `{}` is missing", property)
            }
            ValidationErrorKind::Resolver { url, error } => {
                format!("error resolving reference `{}`: {}", url, error)
            }
            ValidationErrorKind::Schema => {
                "resolved schema failed to compile".to_string()
            }
            ValidationErrorKind::Type { kind } => {
                format!("value doesn't match the required type(s) `{:?}`", kind)
            }
            ValidationErrorKind::UnevaluatedProperties { unexpected } => {
                format!("unevaluated properties `{}`", unexpected.join(", "))
            }
            ValidationErrorKind::UniqueItems => {
                "array contains non-unique elements".to_string()
            }
            ValidationErrorKind::UnknownReferenceScheme { scheme } => {
                format!("unknown reference scheme `{}`", scheme)
            }
            ValidationErrorKind::Utf8 { error } => {
                format!("invalid UTF-8 string: {}", error)
            }
        }
    })
    .collect()
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
        let meta_schema = get_meta_schema().expect("Failed to get meta-schema");
        let valid_schema = json!({ "type": "string" });

        let result = meta_schema.validate(&valid_schema);
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_meta_schema_invalid() {
        let meta_schema = get_meta_schema().expect("Failed to get meta-schema");
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

/// Validates a DetailedConfig and enriches it with dependency graphs
///
/// This function performs comprehensive validation and enrichment:
/// - Validates default configs against their schemas
/// - Validates dimension schemas (including cohort dimensions)
/// - Checks for duplicate dimension positions
/// - Validates contexts against declared dimensions
/// - Validates overrides against default configs
/// - Builds dependency graphs for cohort dimensions
///
/// # Arguments
/// * `detailed` - Mutable reference to DetailedConfig to validate and enrich
///
/// # Returns
/// * `Ok(())` - If validation passes and enrichment is complete
/// * `Err(FormatError)` - If any validation fails
pub fn validate_and_enrich_config(
    detailed: &mut DetailedConfig,
) -> crate::format::error::FormatResult<()> {
    use crate::format::error::{format_validation_errors, FormatError};
    use crate::helpers::create_connections_with_dependents;
    use serde_json::Value;

    let default_configs = &detailed.default_configs;
    let dimensions = &mut detailed.dimensions;

    // Validate default configs against their schemas
    for (k, v) in default_configs.iter() {
        validate_config_value(k, &v.value, &v.schema).map_err(|errors| {
            let error = &errors[0];
            FormatError::ValidationError {
                key: format!("default-configs.{}", error.key()),
                errors: error
                    .errors()
                    .map(format_validation_errors)
                    .unwrap_or_default(),
            }
        })?;
    }

    // Validate dimensions and build dependency graphs
    let mut position_to_dimensions: HashMap<i32, Vec<String>> = HashMap::new();

    for (dim, dim_info) in dimensions.clone().iter() {
        position_to_dimensions
            .entry(dim_info.position)
            .or_default()
            .push(dim.clone());

        match &dim_info.dimension_type {
            superposition_types::database::models::cac::DimensionType::LocalCohort(
                cohort_dim,
            ) => {
                if !dimensions.contains_key(cohort_dim) {
                    return Err(FormatError::InvalidDimension(cohort_dim.clone()));
                }

                validate_cohort_schema_structure(&Value::from(&dim_info.schema))
                    .map_err(|errors| FormatError::ValidationError {
                        key: format!("{}.schema", dim),
                        errors: format_validation_errors(&errors),
                    })?;

                let cohort_dimension_info = dimensions
                    .get(cohort_dim)
                    .ok_or_else(|| FormatError::InvalidDimension(cohort_dim.clone()))?;

                validate_cohort_dimension_position(cohort_dimension_info, dim_info)
                    .map_err(|_| FormatError::InvalidCohortDimensionPosition {
                        dimension: dim.clone(),
                        dimension_position: dim_info.position,
                        cohort_dimension: cohort_dim.clone(),
                        cohort_dimension_position: cohort_dimension_info.position,
                    })?;

                create_connections_with_dependents(&cohort_dim, dim, dimensions);
            }
            superposition_types::database::models::cac::DimensionType::RemoteCohort(
                cohort_dim,
            ) => {
                if !dimensions.contains_key(cohort_dim) {
                    return Err(FormatError::InvalidDimension(cohort_dim.clone()));
                }

                validate_schema(&Value::from(&dim_info.schema)).map_err(|errors| {
                    FormatError::ValidationError {
                        key: format!("{}.schema", dim),
                        errors: format_validation_errors(&errors),
                    }
                })?;

                let cohort_dimension_info = dimensions
                    .get(cohort_dim)
                    .ok_or_else(|| FormatError::InvalidDimension(cohort_dim.clone()))?;

                validate_cohort_dimension_position(cohort_dimension_info, dim_info)
                    .map_err(|_| FormatError::InvalidCohortDimensionPosition {
                        dimension: dim.clone(),
                        dimension_position: dim_info.position,
                        cohort_dimension: cohort_dim.clone(),
                        cohort_dimension_position: cohort_dimension_info.position,
                    })?;

                create_connections_with_dependents(&cohort_dim, dim, dimensions);
            }
            superposition_types::database::models::cac::DimensionType::Regular {} => {
                validate_schema(&Value::from(&dim_info.schema)).map_err(|errors| {
                    FormatError::ValidationError {
                        key: format!("{}.schema", dim),
                        errors: format_validation_errors(&errors),
                    }
                })?;
            }
        }
    }

    // Check for duplicate positions
    for (position, dims) in position_to_dimensions {
        if dims.len() > 1 {
            return Err(FormatError::DuplicatePosition {
                position,
                dimensions: dims,
            });
        }
    }

    // Validate contexts and overrides
    for (index, context) in detailed.contexts.iter().enumerate() {
        let condition = &context.condition;

        validate_context(condition, dimensions).map_err(|errors| {
            let first_error = &errors[0];
            match first_error {
                ContextValidationError::UndeclaredDimension { dimension } => {
                    FormatError::UndeclaredDimension {
                        dimension: dimension.clone(),
                        context: format!("[{}]", index),
                    }
                }
                ContextValidationError::ValidationError { key, errors } => {
                    FormatError::ValidationError {
                        key: format!("context[{}]._context_.{}", index, key),
                        errors: format_validation_errors(errors),
                    }
                }
                _ => FormatError::ValidationError {
                    key: format!("context[{}]._context_", index),
                    errors: format!("{} validation errors", errors.len()),
                },
            }
        })?;
    }

    for (index, context) in detailed.contexts.iter().enumerate() {
        let override_key = context.override_with_keys.get_key();
        if let Some(override_vals) = detailed.overrides.get(override_key) {
            validate_overrides(override_vals, default_configs).map_err(|errors| {
                let first_error = &errors[0];
                match first_error {
                    ContextValidationError::InvalidOverrideKey { key } => {
                        FormatError::InvalidOverrideKey {
                            key: key.clone(),
                            context: format!("[{}]", index),
                        }
                    }
                    ContextValidationError::ValidationError { key, errors } => {
                        FormatError::ValidationError {
                            key: format!("context[{}].{}", index, key),
                            errors: format_validation_errors(errors),
                        }
                    }
                    _ => FormatError::ValidationError {
                        key: format!("context[{}]", index),
                        errors: format!("{} validation errors", errors.len()),
                    },
                }
            })?;
        }
    }

    Ok(())
}
