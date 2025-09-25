use std::collections::HashMap;

use serde_json::{Map, Value};

use crate::{
    database::models::cac::{DependencyGraph, DimensionType},
    DimensionInfo,
};

fn apply_logic(
    condition: &Map<String, Value>,
    context: &Map<String, Value>,
    partial: bool,
) -> bool {
    for (dimension, value) in condition {
        if let Some(context_value) = context.get(dimension) {
            if dimension == "variantIds" {
                if let Value::Array(ref context_values) = context_value {
                    if !context_values.contains(value) {
                        return false;
                    }
                } else {
                    return false;
                }
            } else if *context_value != *value {
                return false;
            }
        } else if partial {
            continue;
        } else {
            return false;
        }
    }
    true
}

pub fn apply(condition: &Map<String, Value>, context: &Map<String, Value>) -> bool {
    apply_logic(condition, context, false)
}

pub fn partial_apply(
    condition: &Map<String, Value>,
    context: &Map<String, Value>,
) -> bool {
    apply_logic(condition, context, true)
}

fn _evaluate_local_cohort_dimension(
    cohort_based_on: &str,
    cohort_based_on_value: &Value,
    schema: &Value,
) -> Option<String> {
    let cohort_schema = schema.as_object()?;
    let definitions_object = cohort_schema.get("definitions")?.as_object()?;

    // Get the array of cohort names from the "enum" field and remove "otherwise"
    let cohort_enums = cohort_schema
        .get("enum")?
        .as_array()?
        .iter()
        .filter_map(|v| v.as_str())
        .filter(|s| *s != "otherwise")
        .collect::<Vec<_>>();

    for cohort_option in cohort_enums {
        let jsonlogic = definitions_object.get(cohort_option)?;
        // Find the first matching cohort definition
        let evaluation_data = serde_json::json!({cohort_based_on: cohort_based_on_value});
        if jsonlogic::apply(jsonlogic, &evaluation_data) == Ok(Value::Bool(true)) {
            return Some(cohort_option.to_string());
        }
    }

    None
}

fn evaluate_local_cohort_dimension(
    cohort_based_on: &str,
    cohort_based_on_value: &Value,
    schema: &Value,
) -> String {
    _evaluate_local_cohort_dimension(cohort_based_on, cohort_based_on_value, schema)
        .unwrap_or_else(|| "otherwise".to_string())
}

fn evaluate_cohorts_dependency(
    dimension: &str,
    value: &Value,
    dependency_graph: &DependencyGraph,
    dimensions: &HashMap<String, DimensionInfo>,
    modified_context: &mut Map<String, Value>,
) {
    let immediate_dependants =
        dependency_graph.get(dimension).cloned().unwrap_or_default();
    for cohort_dimension in immediate_dependants {
        if let Some(dimension_info) = dimensions.get(&cohort_dimension) {
            if matches!(
                dimension_info.dimension_type,
                DimensionType::RemoteCohort(_)
            ) {
                continue;
            }
            let cohort_value = Value::String(evaluate_local_cohort_dimension(
                dimension,
                value,
                &dimension_info.schema,
            ));

            evaluate_cohorts_dependency(
                &cohort_dimension,
                &cohort_value,
                &dimension_info.dependency_graph,
                dimensions,
                modified_context,
            );
            modified_context.insert(cohort_dimension, cohort_value);
        }
    }
}

pub fn evaluate_cohort(
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
) -> Map<String, Value> {
    let mut modified_context = Map::new();

    for (dimension_key, value) in query_data {
        if let Some(dimension_info) = dimensions.get(dimension_key) {
            if matches!(dimension_info.dimension_type, DimensionType::Regular {}) {
                evaluate_cohorts_dependency(
                    dimension_key,
                    value,
                    &dimension_info.dependency_graph,
                    dimensions,
                    &mut modified_context,
                )
            }
        }
        modified_context.insert(dimension_key.to_string(), value.clone());
    }

    for (dimension_key, value) in query_data {
        if let Some(dimension_info) = dimensions.get(dimension_key) {
            if matches!(
                dimension_info.dimension_type,
                DimensionType::RemoteCohort(_)
            ) {
                evaluate_cohorts_dependency(
                    dimension_key,
                    value,
                    &dimension_info.dependency_graph,
                    dimensions,
                    &mut modified_context,
                )
            }
        }
        modified_context.insert(dimension_key.to_string(), value.clone());
    }

    for (dimension_key, value) in query_data {
        if let Some(dimension_info) = dimensions.get(dimension_key) {
            if matches!(dimension_info.dimension_type, DimensionType::LocalCohort(_))
                && !modified_context.contains_key(dimension_key)
            {
                modified_context.insert(dimension_key.to_string(), value.clone());
            }
        }
    }

    for dimension_key in dimensions.keys() {
        if let Some(dimension_info) = dimensions.get(dimension_key) {
            if matches!(dimension_info.dimension_type, DimensionType::LocalCohort(_))
                && !modified_context.contains_key(dimension_key)
            {
                modified_context.insert(
                    dimension_key.to_string(),
                    Value::String("otherwise".to_string()),
                );
            }
        }
    }

    modified_context
}
