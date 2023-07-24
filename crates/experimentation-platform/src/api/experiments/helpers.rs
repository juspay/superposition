use super::types::{ExperimentCreateRequest, Variant, VariantType};
use crate::db::models::{Experiment, ExperimentStatusType};
use diesel::pg::PgConnection;
use diesel::{BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::Value;
use service_utils::service::types::ExperimentationFlags;
use std::collections::{HashMap, HashSet};

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

pub fn are_overlapping_contexts(
    context_a_json: &Value,
    context_b_json: &Value,
) -> Result<bool, &'static str> {
    let context_a = context_a_json
        .as_object()
        .ok_or("context_a not an object")?;
    let context_b = context_b_json
        .as_object()
        .ok_or("context_b not an object")?;

    let context_a_keys = context_a.keys();
    let context_b_keys = context_b.keys();

    let ref_keys = if context_a_keys.len() > context_b_keys.len() {
        context_b_keys
    } else {
        context_a_keys
    };

    let mut is_overlapping = true;
    for key in ref_keys {
        let test = (context_a.contains_key(key) && context_b.contains_key(key))
            && (context_a[key] == context_b[key]);
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
    use crate::db::schema::experiments::dsl::*;

    let created_perdicate = status.eq(ExperimentStatusType::CREATED);
    let inprogress_predicate = status.eq(ExperimentStatusType::INPROGRESS);
    let active_experiments_filter =
        experiments.filter(created_perdicate.or(inprogress_predicate));

    let active_experiments: Vec<Experiment> =
        active_experiments_filter.load(conn).map_err(|e| {
            println!("validate_experiment: {e}");
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
                        println!("validate_experiment: {e}");
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

pub fn add_fields_to_json(json: &Value, fields: &HashMap<String, String>) -> Value {
    match json {
        Value::Object(m) => {
            let mut m = m.clone();
            for (k, v) in fields {
                m.insert(k.clone(), Value::String(v.clone()));
            }
            Value::Object(m)
        }

        v => v.clone(),
    }
}
