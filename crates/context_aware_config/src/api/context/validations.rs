use std::collections::HashMap;

use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use jsonschema::ValidationError;
use serde_json::{Map, Value};
use service_utils::service::types::SchemaName;
use superposition_core::validations::{try_into_jsonschema, validation_err_to_str};
use superposition_macros::{bad_argument, validation_error};
use superposition_types::{
    DBConnection, DimensionInfo,
    database::{models::cac::DimensionType, schema},
    logic::validate_user_cohort_definitions,
    result,
};

pub fn validate_override_with_default_configs(
    conn: &mut DBConnection,
    override_: &Map<String, Value>,
    schema_name: &SchemaName,
) -> result::Result<()> {
    let keys_array: Vec<&String> = override_.keys().collect();
    let res: Vec<(String, Value)> = schema::default_configs::dsl::default_configs
        .filter(schema::default_configs::dsl::key.eq_any(&keys_array))
        .select((
            schema::default_configs::dsl::key,
            schema::default_configs::dsl::schema,
        ))
        .schema_name(schema_name)
        .get_results::<(String, Value)>(conn)?;

    let map = Map::from_iter(res);
    let user_cohort_dimensions = schema::dimensions::dsl::dimensions
        .filter(schema::dimensions::dsl::dimension.eq_any(&keys_array))
        .select((
            schema::dimensions::dsl::dimension,
            schema::dimensions::dsl::schema,
            schema::dimensions::dsl::dimension_type,
        ))
        .schema_name(schema_name)
        .load::<(String, Value, DimensionType)>(conn)?
        .into_iter()
        .filter_map(|(dimension, schema, dimension_type)| match dimension_type {
            DimensionType::UserCohort(based_on) => Some((dimension, (schema, based_on))),
            _ => None,
        })
        .collect::<HashMap<_, _>>();

    for (key, value) in override_.iter() {
        let schema = map
            .get(key)
            .ok_or(bad_argument!("failed to get schema for config key {}", key))?;

        let jschema = try_into_jsonschema(schema).map_err(|e| {
            log::error!("({key}) schema compilation error: {}", e);
            bad_argument!("Invalid JSON schema")
        })?;

        jschema.validate(value).map_err(|e| {
            let verrors = e.collect::<Vec<jsonschema::ValidationError>>();
            log::error!("({key}) config key validation error: {:?}", verrors);
            validation_error!(
                "schema validation failed for {key}: {}",
                &validation_err_to_str(verrors)
                    .first()
                    .unwrap_or(&String::new())
            )
        })?;

        if let Some((dimension_schema, based_on)) = user_cohort_dimensions.get(key) {
            let dimension_schema = dimension_schema.as_object().ok_or_else(|| {
                validation_error!(
                    "Stored schema for user cohort {} is not an object",
                    key
                )
            })?;
            validate_user_cohort_definitions(value, dimension_schema, based_on).map_err(
                |error| {
                    validation_error!(
                        "Invalid definition override for user cohort {}: {}",
                        key,
                        error
                    )
                },
            )?;
        }
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
    let dimension_schema = try_into_jsonschema(dimension_schema).map_err(|e| {
        log::error!("Failed to compile as a Draft-7 JSON schema: {}", e);
        bad_argument!("Error encountered: invalid jsonschema for dimension.")
    })?;

    dimension_schema.validate(dimension_value).map_err(|e| {
        let verrors = e.collect::<Vec<ValidationError>>();
        log::error!(
            "failed to validate dimension value {}: {:?}",
            dimension_value,
            verrors
        );
        validation_error!(
            "failed to validate dimension value {}: {}",
            dimension_value,
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
