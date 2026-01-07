use std::collections::HashSet;

use diesel::{ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::Map;
use serde_json::{json, Value};
use service_utils::{helpers::validation_err_to_str, service::types::SchemaName};
use superposition_macros::{unexpected_error, validation_error};
use superposition_types::{
    api::dimension::DimensionName,
    database::{
        models::cac::{Dimension, DimensionType, Position},
        schema::dimensions,
    },
    result as superposition, DBConnection,
};

use crate::api::{
    dimension::fetch_dimensions_info_map,
    functions::{helpers::get_published_function_code, types::FunctionInfo},
};

pub fn validate_dimension_position(
    dimension_name: DimensionName,
    dimension_position: Position,
    max_allowed: i64,
) -> superposition::Result<()> {
    let dimension_name: String = dimension_name.into();
    let dimension_position: i32 = dimension_position.into();
    match (dimension_name.as_str(), dimension_position) {
        ("variantIds", 0) => Ok(()),
        ("variantIds", d_position) => {
            log::error!("invalid position: {d_position} for dimension: variantIds",);
            Err(validation_error!(
                "variantIds' position should be equal to 0"
            ))
        }
        (_, 0) => {
            log::error!("invalid position: 0 for dimension: {dimension_name}",);
            Err(validation_error!("Oth position is reserved for variantIds"))
        }
        (_, d_position) if d_position as i64 > max_allowed => {
            log::error!("position {d_position} value exceeds total number of dimensions {max_allowed}");
            Err(validation_error!(
                "position value exceeds total number of dimensions"
            ))
        }
        _ => Ok(()),
    }
}

pub fn validate_position_wrt_dependency(
    dimension_name: &str,
    position: &Position,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let dimensions_info = fetch_dimensions_info_map(conn, schema_name)?;

    let Some(dimension) = dimensions_info.get(dimension_name) else {
        return Err(unexpected_error!(
            "Dimension {} not found while validating position with respect to dependencies",
            dimension_name
        ));
    };

    let Some(dependent_dimensions) = dimension.dependency_graph.0.get(dimension_name)
    else {
        return Ok(());
    };

    for dep_dimension in dependent_dimensions {
        let Some(dep_dimension_info) = dimensions_info.get(dep_dimension) else {
            return Err(unexpected_error!(
                "Dependent Dimension {} not found while validating position with respect to dependencies",
                dep_dimension
            ));
        };

        if dep_dimension_info.position >= **position {
            return Err(validation_error!(
                    "Position value invalid: position must be greater than the position of dependent dimension {} which is {}",
                    dep_dimension,
                    dep_dimension_info.position,
                ));
        }
    }

    Ok(())
}

pub fn get_cohort_meta_schema() -> JSONSchema {
    let my_schema = json!({
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
) -> superposition::Result<()> {
    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(schema)
        .map_err(|e| {
            validation_error!("Invalid JSON schema (failed to compile): {:?}", e)
        })?;
    validation_schema.validate(schema).map_err(|e| {
        let verrors = e.collect::<Vec<ValidationError>>();
        validation_error!(
            "schema validation failed: {}",
            validation_err_to_str(verrors)
                .first()
                .unwrap_or(&String::new())
        )
    })
}

pub fn allow_primitive_types(schema: &Map<String, Value>) -> superposition::Result<()> {
    match schema.get("type").cloned().unwrap_or_default() {
        Value::String(type_val) if type_val != "array" && type_val != "object" => {
            Ok(())
        }
        Value::Array(arr) if arr.iter().all(|v| v.as_str().is_some_and(|s| s != "array" && s != "object")) => {
            Ok(())
        }
        _ => {
            Err(validation_error!(
                "Invalid schema: expected a primitive type or an array of primitive types, found: {:?}",
                schema
            ))
        }
    }
}

fn validate_cohort_jsonschema(schema: &Value) -> superposition::Result<Vec<String>> {
    let meta_schema = get_cohort_meta_schema();
    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(schema)
        .map_err(|e| {
            validation_error!("Invalid JSON schema (failed to compile): {:?}", e)
        })?;
    meta_schema.validate(schema).map_err(|e| {
        let verrors = e.collect::<Vec<ValidationError>>();
        validation_error!(
            "schema validation failed: {}",
            validation_err_to_str(verrors)
                .first()
                .unwrap_or(&String::new())
        )
    })?;
    let enum_options = schema
        .get("enum")
        .and_then(|v| v.as_array())
        .ok_or_else(|| {
            validation_error!("Cohort schema must have an 'enum' field of type array")
        })?
        .iter()
        .filter_map(|v| v.as_str().map(str::to_string))
        .collect::<Vec<String>>();
    Ok(enum_options)
}

pub fn does_dimension_exist_for_cohorting(
    dim: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<Dimension> {
    if let Some(dim) = dimensions::dsl::dimensions
        .filter(dimensions::dsl::dimension.eq(dim))
        .schema_name(schema_name)
        .get_result::<Dimension>(conn)
        .optional()?
    {
        match dim.dimension_type {
            DimensionType::LocalCohort(_) => Err(validation_error!(
                "Dimension {} is a local cohort and cannot be used in cohorting",
                &dim.dimension
            )),
            _ => Ok(dim),
        }
    } else {
        Err(validation_error!(
            "Dimension {} used in cohort schema has not been created or does not exist. Please create the dimension first before using it in cohort schema.",
            dim
        ))
    }
}

pub fn validate_cohort_position(
    position: &Position,
    based_on_dimension: &Dimension,
    create: bool,
) -> superposition::Result<()> {
    if create && *position > based_on_dimension.position {
        return Err(validation_error!(
                "While creating dimension, Cohort dimension position {} must be less than or equal to the position {} of the dimension it is based on",
                **position,
                *based_on_dimension.position
            ));
    } else if !create && *position >= based_on_dimension.position {
        return Err(validation_error!(
                "While updating dimension, Cohort dimension position {} must be less than the position {} of the dimension it is based on",
                **position,
                *based_on_dimension.position
            ));
    }
    Ok(())
}

fn check_fn_published(
    fn_name: &str,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let FunctionInfo { published_code, .. } =
        get_published_function_code(conn, fn_name, schema_name)?;
    if published_code.is_some() {
        Ok(())
    } else {
        Err(validation_error!(
            "Function {} doesn't exist / function code not published yet.",
            fn_name
        ))
    }
}

pub fn validate_value_compute_function(
    dimension_type: &DimensionType,
    function: &Option<String>,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    match dimension_type {
        DimensionType::LocalCohort(_) if function.is_some() => Err(validation_error!(
            "Value Compute function should not be provided for local cohort dimensions"
        )),
        DimensionType::RemoteCohort(_) => {
            if let Some(func_name) = function {
                check_fn_published(func_name, conn, schema_name)
            } else {
                Err(validation_error!(
                    "Value Compute function must be provided for remote cohort dimensions"
                ))
            }
        }
        _ => {
            if let Some(func_name) = function {
                check_fn_published(func_name, conn, schema_name)
            } else {
                Ok(())
            }
        }
    }
}
pub fn validate_validation_function(
    function: &Option<String>,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    if let Some(func_name) = function {
        check_fn_published(func_name, conn, schema_name)
    } else {
        Ok(())
    }
}

pub fn validate_cohort_schema(
    cohort_schema: &Value,
    cohort_based_on: &String,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<Dimension> {
    if cohort_based_on.is_empty() {
        return Err(validation_error!(
            "Please specify a valid dimension that this cohort can derive from. Refer our API docs for examples",
        ));
    }

    let enum_options = validate_cohort_jsonschema(cohort_schema)?;

    let cohort_schema = cohort_schema.get("definitions").ok_or(validation_error!(
        "Local cohorts require the jsonlogic rules to be written in the `definitions` field. Refer our API docs for examples",
    ))?;

    let logic = match cohort_schema {
        Value::Object(logic) if logic.is_empty() => {
            return Err(validation_error!(
                "Empty object is not allowed as a schema, mention at least one cohort"
            ));
        }
        Value::Object(logic) => {
            let cohort_options = logic.keys();
            if cohort_options.len() != enum_options.len() - 1 {
                return Err(validation_error!(
                    "The definition of the cohort and the enum options do not match. Some enum options do not have a definition, found {} cohorts and {} enum options (not including otherwise)",
                    cohort_options.len(),
                    enum_options.len() - 1
                ));
            }
            for cohort in cohort_options {
                if !enum_options.contains(cohort) {
                    return Err(validation_error!(
                        "Cohort {} does not have a corresponding enum option",
                        cohort
                    ));
                }
            }
            logic
        }
        _ => {
            return Err(validation_error!(
                "Invalid JSON Logic schema: expected an object, found: {}",
                cohort_schema
            ));
        }
    };

    // check if only one dimension is used across all cohort enums
    let mut dimensions_used = HashSet::new();

    for (cohort_option, expression) in logic.iter() {
        let ast =
            jsonlogic::expression::Expression::from_json(expression).map_err(|e| {
                validation_error!(
                    "Invalid JSON Logic schema for cohort {}, found: {}",
                    cohort_option,
                    e
                )
            })?;

        let dims = ast.get_variable_names().map_err(|e| {
            validation_error!(
                "Invalid JSON Logic in cohort {}, error while parsing variable names: {}",
                cohort_option,
                e
            )
        })?;
        dimensions_used.extend(dims);
    }

    let dimensions_used = dimensions_used.into_iter().collect::<Vec<_>>();

    match dimensions_used[..] {
        [] => {
            // no dimensions? not allowed
            Err(validation_error!(
                "No dimensions used in cohort schema, one dimension is required"
            ))
        }
        [ref dim] => {
            // check if the single dimension used exists in the dimensions table
            let based_on_dimension =
                does_dimension_exist_for_cohorting(dim, schema_name, conn)?;
            if dim != cohort_based_on {
                return Err(validation_error!(
                    "Dimension used in cohort schema ({}) does not match the dimension specified in cohort_based_on ({})",
                    dim,
                    cohort_based_on
                ));
            }
            Ok(based_on_dimension)
        }
        _ => {
            // more than one dimension? not allowed
            Err(validation_error!(
                "Multiple dimensions used in cohort schema and that is not allowed: {:?}",
                dimensions_used
            ))
        }
    }
}

// ************ Tests *************

#[cfg(test)]
mod tests {
    use crate::helpers::get_meta_schema;

    use super::*;

    #[test]
    fn test_get_meta_schema() {
        let x = get_meta_schema();

        let ok_string_schema = json!({"type": "string", "pattern": ".*"});
        let ok_string_validation = x.validate(&ok_string_schema);
        assert!(ok_string_validation.is_ok());

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
}
