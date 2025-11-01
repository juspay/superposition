#[cfg(test)]
mod tests;

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
    schema: &Map<String, Value>,
) -> Option<String> {
    let definitions_object = schema.get("definitions")?.as_object()?;

    // Get the array of cohort names from the "enum" field and remove "otherwise"
    let cohort_enums = schema
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
    schema: &Map<String, Value>,
) -> String {
    _evaluate_local_cohort_dimension(cohort_based_on, cohort_based_on_value, schema)
        .unwrap_or_else(|| "otherwise".to_string())
}

/// Evaluates local cohort dependencies in a depth-first manner
fn evaluate_local_cohorts_dependency(
    dimension: &str,
    value: &Value,
    dependency_graph: &DependencyGraph,
    dimensions: &HashMap<String, DimensionInfo>,
    modified_context: &mut Map<String, Value>,
    query_data: &Map<String, Value>,
) {
    let mut stack = dependency_graph
        .get(dimension)
        .cloned()
        .unwrap_or_default()
        .into_iter()
        .map(|d| (d, dimension.to_string(), value.clone()))
        .collect::<Vec<_>>();

    // Depth-first traversal of dependencies
    while let Some((cohort_dimension, based_on, based_on_val)) = stack.pop() {
        if let Some(dimension_info) = dimensions.get(&cohort_dimension) {
            let mut cohort_val = None;
            match &dimension_info.dimension_type {
                DimensionType::LocalCohort(_) => {
                    let cohort_value = Value::String(evaluate_local_cohort_dimension(
                        &based_on,
                        &based_on_val,
                        &dimension_info.schema,
                    ));
                    modified_context
                        .insert(cohort_dimension.clone(), cohort_value.clone());
                    cohort_val = Some(cohort_value);
                }
                _ => {
                    if let Some(value) = query_data.get(&cohort_dimension) {
                        modified_context.insert(cohort_dimension.clone(), value.clone());
                        cohort_val = Some(value.clone());
                    }
                }
            }

            if let Some(cohort_val) = cohort_val {
                stack.extend(
                    dimension_info
                        .dependency_graph
                        .get(&cohort_dimension)
                        .cloned()
                        .unwrap_or_default()
                        .into_iter()
                        .map(|d| (d, cohort_dimension.clone(), cohort_val.clone()))
                        .collect::<Vec<_>>(),
                );
            }
        }
    }
}

/// Evaluates all local cohort dimensions based on the provided query data and dimension definitions
/// First all local cohorts which are computable from the query data are evaluated, then any remaining local cohorts are set to "otherwise"
/// Computation starts from such a point, such that dependencies can be resolved in a depth-first manner
///
/// Values of regular and remote cohort dimensions in query_data are retained as is.
/// Returned value, might have a different value for local cohort dimensions based on its based on dimensions,
/// if the value provided for the local cohort was incorrect in the query data.
pub fn evaluate_local_cohorts(
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
) -> Map<String, Value> {
    if dimensions.is_empty() {
        return query_data.clone();
    }

    let mut modified_context = Map::new();

    // Start from dimensions that are closest to root in each tree
    for dimension_key in dimensions_to_start_from(dimensions, query_data) {
        if let Some(value) = query_data.get(&dimension_key) {
            if let Some(dimension_info) = dimensions.get(&dimension_key) {
                modified_context.insert(dimension_key.to_string(), value.clone());
                evaluate_local_cohorts_dependency(
                    &dimension_key,
                    value,
                    &dimension_info.dependency_graph,
                    dimensions,
                    &mut modified_context,
                    query_data,
                );
            }
        }
    }

    // For any local cohort dimension not yet set, set it to "otherwise"
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

/// Identifies starting dimensions for evaluation based on query data and dimension definitions
/// For each tree in the dependency graph, picks the node closest to root from query_data for each branch of the tree.
/// If nothing is found and a local cohort is encountered, picks that local cohort as start point from that branch.
pub fn dimensions_to_start_from(
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
) -> Vec<String> {
    let mut start_dimensions = Vec::new();

    let regular_dimensions = dimensions
        .iter()
        .filter(|(_, data)| matches!(data.dimension_type, DimensionType::Regular {}))
        .map(|(dim_name, _)| dim_name.clone())
        .collect::<Vec<String>>();

    for root_dimension in regular_dimensions {
        let dependency_graph = &dimensions
            .get(&root_dimension)
            .map(|data| data.dependency_graph.clone())
            .unwrap_or_default();

        let mut stack = vec![root_dimension];

        while let Some(current_dimension) = stack.pop() {
            if query_data.contains_key(&current_dimension) {
                start_dimensions.push(current_dimension);
                continue;
            }

            if let Some(data) = dimensions.get(&current_dimension) {
                if matches!(data.dimension_type, DimensionType::LocalCohort(_)) {
                    start_dimensions.push(current_dimension);
                    continue;
                }
            }

            stack.extend(
                dependency_graph
                    .get(&current_dimension)
                    .cloned()
                    .unwrap_or_default(),
            );
        }
    }

    start_dimensions
}
