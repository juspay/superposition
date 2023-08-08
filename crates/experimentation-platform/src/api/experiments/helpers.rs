use super::types::{ExperimentCreateRequest, Variant, VariantType};
use crate::db::models::{Experiment, ExperimentStatusType};
use diesel::pg::PgConnection;
use diesel::{BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::{Map, Value};
use service_utils::service::types::ExperimentationFlags;
use std::collections::HashSet;

pub fn check_variant_types(variants: &Vec<Variant>) -> Result<(), &'static str> {
    let mut experimental_variant_cnt = 0;
    let mut control_variant_cnt = 0;

    for variant in variants {
        match variant.variant_type {
            VariantType::CONTROL => {
                control_variant_cnt += 1;
            }
            VariantType::EXPERIMENTAL => {
                experimental_variant_cnt += 1;
            }
        }
    }

    if control_variant_cnt > 1 || control_variant_cnt == 0 {
        return Err("experiment should have exactly 1 control variant.");
    } else if experimental_variant_cnt < 1 {
        return Err("experiment should have atlease 1 experimental variant");
    }

    Ok(())
}

pub fn extract_dimensions(
    context_json: &Value,
) -> Result<Map<String, Value>, &'static str> {
    // Assuming max 2-level nesting in context json logic
    let context = context_json
        .as_object()
        .ok_or("extract_dimensions: context not an object")?;

    let conditions = match context.get("and") {
        Some(conditions_json) => conditions_json
            .as_array()
            .ok_or("extract_dimension: failed parsing conditions as an array")?
            .clone(),
        None => vec![context_json.clone()],
    };

    let mut dimension_tuples = Vec::new();
    for condition in &conditions {
        let condition_obj = condition
            .as_object()
            .ok_or("extract_dimensions: failed to parse condition as an object")?;
        let operators = condition_obj.keys();

        for operator in operators {
            let operands = condition_obj[operator]
                .as_array()
                .ok_or("extract_dimension: failed to parse operands as an arrays")?;

            let variable_name = operands
                .get(0) // getting first element which should contain an object with property `var` with string value
                .ok_or(
                    "extract_dimension: failed to get variable name from operands list",
                )?
                .as_object() // parsing json value as an object/map
                .ok_or("extract_dimension: failed to parse variable as an object")?
                .get("var") // accessing `var` from object/map which contains variable name
                .ok_or("extract_dimension: var property not present in variable object")?
                .as_str() // parsing json value as raw string
                .ok_or("extract_dimension: var propery value is not a string")?;

            let variable_value = operands
                .get(1) // getting second element which should be the value of the variable
                .ok_or(
                    "extract_dimension: failed to get variable value from operands list",
                )?;

            dimension_tuples.push((String::from(variable_name), variable_value.clone()));
        }
    }

    Ok(Map::from_iter(dimension_tuples))
}

pub fn are_overlapping_contexts(
    context_a: &Value,
    context_b: &Value,
) -> Result<bool, &'static str> {
    let dimensions_a = extract_dimensions(context_a)?;
    let dimensions_b = extract_dimensions(context_b)?;

    let dim_a_keys = dimensions_a.keys();
    let dim_b_keys = dimensions_b.keys();

    let ref_keys = if dim_a_keys.len() > dim_b_keys.len() {
        dim_b_keys
    } else {
        dim_a_keys
    };

    let mut is_overlapping = true;
    for key in ref_keys {
        let test = (dimensions_a.contains_key(key) && dimensions_b.contains_key(key))
            && (dimensions_a[key] == dimensions_b[key]);
        is_overlapping = is_overlapping && test;

        if !test {
            break;
        }
    }

    Ok(is_overlapping)
}

pub fn check_variants_override_coverage(
    variants: &Vec<Variant>,
    override_keys: &Vec<String>,
) -> bool {
    let mut has_complete_coverage = true;

    for variant in variants {
        let overrides = &variant.overrides;
        let mut is_valid_variant = true;

        for override_key in override_keys {
            let has_override_key = match overrides[override_key] {
                Value::Null => false,
                _ => true,
            };
            is_valid_variant = is_valid_variant && has_override_key;
        }

        has_complete_coverage = has_complete_coverage && is_valid_variant;
        if !has_complete_coverage {
            break;
        }
    }

    has_complete_coverage
}

pub fn validate_experiment(
    experiment: &ExperimentCreateRequest,
    flags: &ExperimentationFlags,
    conn: &mut PgConnection,
) -> Result<bool, &'static str> {
    use crate::db::schema::cac_v1::experiments::dsl::*;

    let created_perdicate = status.eq(ExperimentStatusType::CREATED);
    let inprogress_predicate = status.eq(ExperimentStatusType::INPROGRESS);
    let active_experiments_filter =
        experiments.filter(created_perdicate.or(inprogress_predicate));

    let active_experiments: Vec<Experiment> =
        active_experiments_filter.load(conn).map_err(|e| {
            log::info!("validate_experiment: {e}");
            "Failed to fetch active experiments"
        })?;

    let mut valid_experiment = true;
    if !flags.allow_same_keys_overlapping_ctx
        || !flags.allow_diff_keys_overlapping_ctx
        || !flags.allow_same_keys_non_overlapping_ctx
    {
        let override_keys_set: HashSet<_> = experiment.override_keys.iter().collect();
        for active_experiment in active_experiments.iter() {
            let are_overlapping =
                are_overlapping_contexts(&experiment.context, &active_experiment.context)
                    .map_err(|e| {
                        log::info!("validate_experiment: {e}");
                        "Failed to validate for overlapping context"
                    })?;

            let have_intersecting_key_set = active_experiment
                .override_keys
                .iter()
                .any(|key| override_keys_set.contains(key));

            if !flags.allow_diff_keys_overlapping_ctx {
                valid_experiment =
                    valid_experiment && !(are_overlapping && !have_intersecting_key_set);
            }
            if !flags.allow_same_keys_overlapping_ctx {
                valid_experiment =
                    valid_experiment && !(are_overlapping && have_intersecting_key_set);
            }
            if !flags.allow_same_keys_non_overlapping_ctx {
                valid_experiment =
                    valid_experiment && !(!are_overlapping && have_intersecting_key_set);
            }

            if !valid_experiment {
                break;
            }
        }
    }

    Ok(valid_experiment)
}

pub fn add_variant_dimension_to_ctx(
    context_json: &Value,
    variant: String,
) -> Result<Value, &'static str> {
    let context = context_json
        .as_object()
        .ok_or("extract_dimensions: context not an object")?;

    let mut conditions = match context.get("and") {
        Some(conditions_json) => conditions_json
            .as_array()
            .ok_or("extract_dimension: failed parsing conditions as an array")?
            .clone(),
        None => vec![context_json.clone()],
    };

    let variant_condition = serde_json::json!({
        "in" : [
            variant,
            { "var": "variantIds" }
        ]
    });
    conditions.push(variant_condition);

    let mut updated_ctx = Map::new();
    updated_ctx.insert(String::from("and"), serde_json::Value::Array(conditions));

    match serde_json::to_value(updated_ctx) {
        Ok(value) => Ok(value),
        Err(e) => {
            log::info!("add_variant_dimension_to_ctx: Failed to convert context to serde_json::Value {e}");
            Err("add_variant_dimension_to_ctx: Failed to convert context to serde_json::Value")
        }
    }
}
