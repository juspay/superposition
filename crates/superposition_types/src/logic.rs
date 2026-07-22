#[cfg(test)]
mod tests;

use std::collections::{HashMap, HashSet};

use serde_json::{Map, Value};

use crate::{database::models::cac::DimensionType, DimensionInfo, ExtendedMap};

#[inline]
fn apply_logic(
    condition: &Map<String, Value>,
    context: &Map<String, Value>,
    partial: bool,
) -> bool {
    condition.iter().all(|(dimension, expected_value)| {
        let Some(actual_value) = context.get(dimension) else {
            return partial;
        };

        match (dimension.as_str(), actual_value) {
            ("variantIds", Value::Array(values)) => values.contains(expected_value),
            ("variantIds", _) => false,
            (_, actual_value) => actual_value == expected_value,
        }
    })
}

/// Core context application logic - checks if all dimensions in condition are satisfied by context
/// Only exact matches are considered valid, except for "variantIds" dimension where containment is checked
/// Returns true if condition is satisfied by context, false otherwise
pub fn apply(condition: &Map<String, Value>, context: &Map<String, Value>) -> bool {
    apply_logic(condition, context, false)
}

/// Filtering logic that allows partial matching of context
/// For dimensions present in context, performs the same checks as `apply`
/// For dimensions absent in context, skips the check (allows partial matching)
/// This is useful for matching contexts that may not have all dimensions defined
/// For array context values, checks for containment - added behaviour over `apply`
/// Returns false if there is a mismatch, true otherwise
pub fn partial_apply(
    condition: &Map<String, Value>,
    context: &Map<String, Value>,
) -> bool {
    for (dimension, value) in condition {
        let Some(context_value) = context.get(dimension) else {
            continue; // Skip dimensions not in context (partial matching)
        };

        // For array context values, check containment
        if let Value::Array(context_values) = context_value {
            if !context_values.contains(value) {
                return false;
            }
        } else if dimension == "variantIds" {
            // variantIds must always be an array - fail if scalar
            return false;
        } else if *context_value != *value {
            // For non-array values, check equality
            return false;
        }
    }
    true
}

mod cohort_evaluation {
    use jsonlogic::expression::Expression;
    use serde_json::json;

    use crate::database::models::cac::DependencyGraph;

    use super::*;

    const COHORT_OTHERWISE: &str = "otherwise";

    fn cohort_options(schema: &Map<String, Value>) -> Result<Vec<String>, String> {
        schema
            .get("enum")
            .and_then(Value::as_array)
            .ok_or_else(|| "Cohort schema must contain an enum array".to_string())?
            .iter()
            .map(|value| {
                value
                    .as_str()
                    .map(str::to_string)
                    .ok_or_else(|| "Cohort enum values must be strings".to_string())
            })
            .filter(|option| option.as_deref() != Ok(COHORT_OTHERWISE))
            .collect()
    }

    fn select_matching_cohort_option(
        definitions: &Map<String, Value>,
        dimension_schema: &Map<String, Value>,
        based_on: &str,
        based_on_value: &Value,
    ) -> Result<Value, String> {
        let evaluation_data = json!({ based_on: based_on_value });

        for option in cohort_options(dimension_schema)? {
            let definition = definitions.get(&option).ok_or_else(|| {
                format!("Definition for cohort option {option} is missing")
            })?;
            match jsonlogic::apply(definition, &evaluation_data) {
                Ok(Value::Bool(true)) => return Ok(Value::String(option)),
                Ok(Value::Bool(false)) => {}
                Ok(value) => {
                    return Err(format!(
                        "Cohort definition {option} returned {value} instead of a boolean"
                    ));
                }
                Err(error) => {
                    return Err(format!(
                        "Failed to evaluate cohort definition {option}: {error}"
                    ));
                }
            }
        }

        Ok(Value::String(COHORT_OTHERWISE.to_string()))
    }

    fn select_local_cohort_option(
        cohort_based_on: &str,
        cohort_based_on_value: &Value,
        schema: &Map<String, Value>,
    ) -> String {
        let Some(definitions) = schema.get("definitions").and_then(Value::as_object)
        else {
            return COHORT_OTHERWISE.to_string();
        };

        select_matching_cohort_option(
            definitions,
            schema,
            cohort_based_on,
            cohort_based_on_value,
        )
        .ok()
        .and_then(|value| value.as_str().map(str::to_string))
        .unwrap_or_else(|| COHORT_OTHERWISE.to_string())
    }

    fn resolve_dimension_value(
        dimension_name: &str,
        dimension_info: &DimensionInfo,
        based_on_value: &Value,
        query_data: &Map<String, Value>,
        resolved_user_definitions: Option<&Map<String, Value>>,
    ) -> Result<Option<Value>, String> {
        match &dimension_info.dimension_type {
            DimensionType::LocalCohort(based_on) => {
                Ok(Some(Value::String(select_local_cohort_option(
                    based_on,
                    based_on_value,
                    &dimension_info.schema,
                ))))
            }
            DimensionType::UserCohort(based_on) => {
                let Some(resolved_user_definitions) = resolved_user_definitions else {
                    return Ok(None);
                };
                let definitions = resolved_user_definitions
                    .get(dimension_name)
                    .and_then(Value::as_object)
                    .ok_or_else(|| {
                        format!(
                            "Generated definition key {dimension_name} is missing from resolved config"
                        )
                    })?;
                select_matching_cohort_option(
                    definitions,
                    dimension_info.schema.inner(),
                    based_on,
                    based_on_value,
                )
                .map(Some)
            }
            DimensionType::Regular {} | DimensionType::RemoteCohort(_) => {
                Ok(query_data.get(dimension_name).cloned())
            }
        }
    }

    /// Resolves cohort dependencies in a depth-first manner.
    fn resolve_dependent_cohorts(
        dimension: &str,
        value: &Value,
        dependency_graph: &DependencyGraph,
        dimensions: &HashMap<String, DimensionInfo>,
        modified_context: &mut Map<String, Value>,
        query_data: &Map<String, Value>,
        resolved_user_definitions: Option<&Map<String, Value>>,
    ) -> Result<(), String> {
        let mut stack = dependency_graph
            .get(dimension)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(|dependent| (dependent, value.clone()))
            .collect::<Vec<_>>();

        // Depth-first traversal of dependencies.
        while let Some((dependent_dimension, based_on_value)) = stack.pop() {
            let Some(dimension_info) = dimensions.get(&dependent_dimension) else {
                continue;
            };
            let Some(dependent_value) = resolve_dimension_value(
                &dependent_dimension,
                dimension_info,
                &based_on_value,
                query_data,
                resolved_user_definitions,
            )?
            else {
                continue;
            };

            modified_context.insert(dependent_dimension.clone(), dependent_value.clone());
            stack.extend(
                dimension_info
                    .dependency_graph
                    .get(&dependent_dimension)
                    .cloned()
                    .unwrap_or_default()
                    .into_iter()
                    .map(|dependent| (dependent, dependent_value.clone())),
            );
        }

        Ok(())
    }

    pub(super) fn evaluate_context(
        dimensions: &HashMap<String, DimensionInfo>,
        query_data: &Map<String, Value>,
        resolved_user_definitions: Option<&Map<String, Value>>,
        skip_unresolved: bool,
    ) -> Result<Map<String, Value>, String> {
        if dimensions.is_empty() {
            return Ok(query_data.clone());
        }

        let mut modified_context = Map::new();

        // Start from dimensions that are closest to the root in each tree.
        for dimension_key in find_evaluation_start_dimensions(dimensions, query_data) {
            if let Some(value) = query_data.get(&dimension_key) {
                if let Some(dimension_info) = dimensions.get(&dimension_key) {
                    if matches!(
                        dimension_info.dimension_type,
                        DimensionType::UserCohort(_)
                    ) {
                        continue;
                    }
                    modified_context.insert(dimension_key.to_string(), value.clone());
                    resolve_dependent_cohorts(
                        &dimension_key,
                        value,
                        &dimension_info.dependency_graph,
                        dimensions,
                        &mut modified_context,
                        query_data,
                        resolved_user_definitions,
                    )?;
                }
            }
        }

        if skip_unresolved {
            return Ok(modified_context);
        }

        for (dimension_key, dimension_info) in dimensions {
            let should_default =
                matches!(dimension_info.dimension_type, DimensionType::LocalCohort(_))
                    || (resolved_user_definitions.is_some()
                        && matches!(
                            dimension_info.dimension_type,
                            DimensionType::UserCohort(_)
                        ));
            if should_default && !modified_context.contains_key(dimension_key) {
                modified_context.insert(
                    dimension_key.clone(),
                    Value::String(COHORT_OTHERWISE.to_string()),
                );
            }
        }

        Ok(modified_context)
    }

    pub(super) fn build_definition_schema(
        dimension_schema: &Map<String, Value>,
    ) -> Result<ExtendedMap, String> {
        let options = cohort_options(dimension_schema)?;
        let properties = options
            .iter()
            .cloned()
            .map(|option| (option, json!({})))
            .collect::<Map<String, Value>>();
        let required = options.into_iter().map(Value::String).collect::<Vec<_>>();

        ExtendedMap::try_from(json!({
            "type": "object",
            "properties": properties,
            "required": required,
            "additionalProperties": false
        }))
    }

    pub(super) fn validate_definitions(
        definitions: &Value,
        dimension_schema: &Map<String, Value>,
        based_on: &str,
    ) -> Result<(), String> {
        let definitions = definitions.as_object().ok_or_else(|| {
            "User cohort definition value must be a JSON object".to_string()
        })?;
        let expected = cohort_options(dimension_schema)?
            .into_iter()
            .collect::<HashSet<_>>();
        let actual = definitions.keys().cloned().collect::<HashSet<_>>();

        if actual != expected {
            let mut missing = expected.difference(&actual).cloned().collect::<Vec<_>>();
            let mut unexpected =
                actual.difference(&expected).cloned().collect::<Vec<_>>();
            missing.sort();
            unexpected.sort();
            return Err(format!(
                "User cohort definitions do not match the dimension enum; missing: {:?}, unexpected: {:?}",
                missing, unexpected
            ));
        }

        let mut referenced_dimensions = HashSet::new();
        for (cohort, definition) in definitions {
            let expression = Expression::from_json(definition).map_err(|error| {
                format!("Invalid JSON Logic for user cohort {cohort}: {error}")
            })?;
            let variables = expression.get_variable_names().map_err(|error| {
                format!(
                    "Unable to inspect JSON Logic variables for user cohort {cohort}: {error}"
                )
            })?;
            referenced_dimensions.extend(variables);
        }

        if referenced_dimensions.len() != 1 || !referenced_dimensions.contains(based_on) {
            let mut referenced_dimensions =
                referenced_dimensions.into_iter().collect::<Vec<_>>();
            referenced_dimensions.sort();
            return Err(format!(
                "User cohort definitions must reference only the declared based_on dimension {based_on}; found: {:?}",
                referenced_dimensions
            ));
        }

        Ok(())
    }

    #[cfg(test)]
    pub(super) fn resolve_local_cohort_option_for_test(
        cohort_based_on: &str,
        cohort_based_on_value: &Value,
        schema: &Map<String, Value>,
    ) -> String {
        select_local_cohort_option(cohort_based_on, cohort_based_on_value, schema)
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
    cohort_evaluation::evaluate_context(dimensions, query_data, None, false)
        .unwrap_or_else(|_| query_data.clone())
}

/// Same as evaluate_local_cohorts but does not set unresolved local cohorts to "otherwise"
pub fn evaluate_local_cohorts_skip_unresolved(
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
) -> Map<String, Value> {
    cohort_evaluation::evaluate_context(dimensions, query_data, None, true)
        .unwrap_or_else(|_| query_data.clone())
}

pub fn extract_user_cohort_definitions(
    schema: &Map<String, Value>,
) -> Result<Map<String, Value>, String> {
    schema
        .get("definitions")
        .and_then(Value::as_object)
        .cloned()
        .ok_or_else(|| {
            "User cohort schema must contain an object-valued definitions field"
                .to_string()
        })
}

pub fn build_user_cohort_definition_schema(
    dimension_schema: &Map<String, Value>,
) -> Result<ExtendedMap, String> {
    cohort_evaluation::build_definition_schema(dimension_schema)
}

pub fn validate_user_cohort_definitions(
    definitions: &Value,
    dimension_schema: &Map<String, Value>,
    based_on: &str,
) -> Result<(), String> {
    cohort_evaluation::validate_definitions(definitions, dimension_schema, based_on)
}

pub fn user_cohort_dimension_names(
    dimensions: &HashMap<String, DimensionInfo>,
) -> HashSet<String> {
    dimensions
        .iter()
        .filter_map(|(name, info)| {
            matches!(info.dimension_type, DimensionType::UserCohort(_))
                .then_some(name.clone())
        })
        .collect()
}

pub fn evaluate_cohorts_with_resolved_definitions(
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    resolved_definitions: &Map<String, Value>,
) -> Result<Map<String, Value>, String> {
    for (name, info) in dimensions {
        let DimensionType::UserCohort(based_on) = &info.dimension_type else {
            continue;
        };
        let definitions = resolved_definitions.get(name).ok_or_else(|| {
            format!("Generated definition key {name} is missing from resolved config")
        })?;
        validate_user_cohort_definitions(definitions, info.schema.inner(), based_on)?;
    }

    cohort_evaluation::evaluate_context(
        dimensions,
        query_data,
        Some(resolved_definitions),
        false,
    )
}

/// Identifies starting dimensions for evaluation based on query data and dimension definitions
/// For each tree in the dependency graph, picks the node closest to root from query_data for each branch of the tree.
/// If nothing is found and a local cohort is encountered, picks that local cohort as start point from that branch.
pub fn find_evaluation_start_dimensions(
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
