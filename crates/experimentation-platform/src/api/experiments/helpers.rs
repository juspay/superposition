use super::types::{Variant, VariantType};
use crate::db::models::{Experiment, ExperimentStatusType};
use diesel::pg::PgConnection;
use diesel::{BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::{Map, Value};
use service_utils::errors::types::ErrorResponse;
use service_utils::service::types::ExperimentationFlags;
use service_utils::{errors::types::Error as err, types as app};
use std::collections::HashSet;

pub fn check_variant_types(variants: &Vec<Variant>) -> app::Result<()> {
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
        return Err(err::BadArgument(ErrorResponse {
            message: "experiment should have exactly 1 control variant".to_string(),
            possible_fix: "ensure only one control variant is present".to_string(),
        }));
    } else if experimental_variant_cnt < 1 {
        return Err(err::BadArgument(ErrorResponse {
            message: "experiment should have at least 1 experimental variant".to_string(),
            possible_fix: "ensure only one control variant is present".to_string(),
        }));
    }

    Ok(())
}

pub fn validate_override_keys(override_keys: &Vec<String>) -> app::Result<()> {
    let mut key_set: HashSet<&str> = HashSet::new();
    for key in override_keys {
        if !key_set.insert(key) {
            return Err(err::BadArgument(ErrorResponse {
                message: "override_keys are not unique".to_string(),
                possible_fix: "remove duplicate entries in override_keys".to_string(),
            }));
        }
    }

    Ok(())
}

pub fn get_variable_name_and_value(operands: &Vec<Value>) -> app::Result<(&str, &Value)> {
    let (obj_pos, variable_obj) = operands
        .iter()
        .enumerate()
        .find(|(_, operand)| {
            operand.is_object() && operand.as_object().unwrap().get("var").is_some()
        })
        .ok_or(err::BadArgument(ErrorResponse {
            message: " failed to get variable name from operands list".to_string(),
            possible_fix: "ensure the context provided obeys the rules of JSON logic"
                .to_string(),
        }))?;

    let variable_name = variable_obj
        .as_object()
        .map_or(None, |obj| obj.get("var"))
        .map_or(None, |value| value.as_str())
        .ok_or(err::BadArgument(ErrorResponse {
            message: " failed to get variable name from operands list".to_string(),
            possible_fix: "ensure the context provided obeys the rules of JSON logic"
                .to_string(),
        }))?;

    let value_pos = (obj_pos + 1) % 2;
    let variable_value =
        operands
            .get(value_pos)
            .ok_or(err::BadArgument(ErrorResponse {
                message: " failed to get variable value from operands list".to_string(),
                possible_fix: "ensure the context provided obeys the rules of JSON logic"
                    .to_string(),
            }))?;

    Ok((variable_name, variable_value))
}

pub fn extract_dimensions(context_json: &Value) -> app::Result<Map<String, Value>> {
    // Assuming max 2-level nesting in context json logic
    let context = context_json
        .as_object()
        .ok_or(err::BadArgument(ErrorResponse { message: "An error occurred while extracting dimensions: context not a valid JSON object".to_string(), possible_fix: "send a valid JSON context".to_string() }))?;

    let conditions = match context.get("and") {
        Some(conditions_json) => conditions_json
            .as_array()
            .ok_or(err::BadArgument(ErrorResponse { message: "An error occurred while extracting dimensions: failed parsing conditions as an array".to_string(), possible_fix: "ensure the context provided obeys the rules of JSON logic".to_string() }))?
            .clone(),
        None => vec![context_json.clone()],
    };

    let mut dimension_tuples = Vec::new();
    for condition in &conditions {
        let condition_obj =
            condition
                .as_object()
                .ok_or(err::BadArgument(ErrorResponse {
                    message: " failed to parse condition as an object".to_string(),
                    possible_fix:
                        "ensure the context provided obeys the rules of JSON logic"
                            .to_string(),
                }))?;
        let operators = condition_obj.keys();

        for operator in operators {
            let operands = condition_obj[operator].as_array().ok_or(err::BadArgument(
                ErrorResponse {
                    message: " failed to parse operands as an arrays".to_string(),
                    possible_fix:
                        "ensure the context provided obeys the rules of JSON logic"
                            .to_string(),
                },
            ))?;

            let (variable_name, variable_value) = get_variable_name_and_value(operands)?;

            dimension_tuples.push((String::from(variable_name), variable_value.clone()));
        }
    }

    Ok(Map::from_iter(dimension_tuples))
}

pub fn are_overlapping_contexts(
    context_a: &Value,
    context_b: &Value,
) -> app::Result<bool> {
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

pub fn check_variant_override_coverage(
    variant_override: &Map<String, Value>,
    override_keys: &Vec<String>,
) -> bool {
    if variant_override.keys().len() != override_keys.len() {
        return false;
    }

    for override_key in override_keys {
        if variant_override.get(override_key).is_none() {
            return false;
        }
    }
    return true;
}

pub fn check_variants_override_coverage(
    variant_overrides: &Vec<&Map<String, Value>>,
    override_keys: &Vec<String>,
) -> bool {
    for variant_override in variant_overrides {
        if !check_variant_override_coverage(variant_override, override_keys) {
            return false;
        }
    }

    return true;
}

pub fn is_valid_experiment(
    context: &Value,
    override_keys: &Vec<String>,
    flags: &ExperimentationFlags,
    active_experiments: &Vec<Experiment>,
) -> app::Result<(bool, String)> {
    let mut valid_experiment = true;
    let mut invalid_reason = String::new();
    if !flags.allow_same_keys_overlapping_ctx
        || !flags.allow_diff_keys_overlapping_ctx
        || !flags.allow_same_keys_non_overlapping_ctx
    {
        let override_keys_set: HashSet<_> = override_keys.iter().collect();
        for active_experiment in active_experiments.iter() {
            let are_overlapping =
                are_overlapping_contexts(context, &active_experiment.context)
                    .map_err(|e| {
                        log::info!("validate_experiment: {e}");
                        err::BadArgument(ErrorResponse {
                            message: "Failed to validate for overlapping context. One of the current running experiments already has this context or overlaps with it".into(),
                            possible_fix: "Overlapping contexts are not allowed currently as per your configuration of CAC".into(),
                        })
                    })?;

            let have_intersecting_key_set = active_experiment
                .override_keys
                .iter()
                .any(|key| override_keys_set.contains(key));

            let same_key_set = active_experiment
                .override_keys
                .iter()
                .all(|key| override_keys_set.contains(key));

            if !flags.allow_diff_keys_overlapping_ctx {
                valid_experiment =
                    valid_experiment && !(are_overlapping && !same_key_set);
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
                invalid_reason.push_str("This current context overlaps with an existing experiment or the keys in the context are overlapping");
                break;
            }
        }
    }

    Ok((valid_experiment, invalid_reason))
}

pub fn validate_experiment(
    context: &Value,
    override_keys: &Vec<String>,
    experiment_id: Option<i64>,
    flags: &ExperimentationFlags,
    conn: &mut PgConnection,
) -> app::Result<(bool, String)> {
    use crate::db::schema::experiments::dsl as experiments_dsl;

    let active_experiments: Vec<Experiment> = experiments_dsl::experiments
        .filter(
            diesel::dsl::not(experiments_dsl::id.eq(experiment_id.unwrap_or_default()))
                .and(
                    experiments_dsl::status
                        .eq(ExperimentStatusType::CREATED)
                        .or(experiments_dsl::status.eq(ExperimentStatusType::INPROGRESS)),
                ),
        )
        .load(conn)?;

    is_valid_experiment(context, override_keys, flags, &active_experiments)
}

pub fn add_variant_dimension_to_ctx(
    context_json: &Value,
    variant: String,
) -> app::Result<Value> {
    let context = context_json
        .as_object()
        .ok_or(err::BadArgument(ErrorResponse {
            message: "context not an object".to_string(),
            possible_fix: "ensure the context provided obeys the rules of JSON logic"
                .to_string(),
        }))?;

    let mut conditions = match context.get("and") {
        Some(conditions_json) => conditions_json
            .as_array()
            .ok_or(err::BadArgument(ErrorResponse {
                message: " failed parsing conditions as an array".to_string(),
                possible_fix: "ensure the context provided obeys the rules of JSON logic"
                    .to_string(),
            }))?
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
        Err(_) => Err(err::BadArgument(ErrorResponse {
            message: "Failed to convert context to a valid JSON object".to_string(),
            possible_fix: "Check the request sent for correctness".to_string(),
        })),
    }
}

pub fn extract_override_keys(overrides: &Map<String, Value>) -> HashSet<String> {
    overrides.keys().map(String::from).collect()
}
