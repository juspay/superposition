use std::collections::{HashMap, HashSet};

use serde_json::{json, Map, Value};
pub use superposition_types::api::config::MergeStrategy;
use superposition_types::{
    logic::{
        evaluate_cohorts_with_resolved_definitions, evaluate_local_cohorts,
        user_cohort_dimension_names,
    },
    Config, Context, DimensionInfo, Overrides,
};

fn eval_config_once(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: &MergeStrategy,
    on_override_select: Option<&mut dyn FnMut(Context)>,
) -> Result<Map<String, Value>, String> {
    let modified_query_data = evaluate_local_cohorts(dimensions, query_data);
    let overrides_map: Map<String, Value> = get_overrides(
        &modified_query_data,
        contexts,
        overrides,
        merge_strategy,
        on_override_select,
    )?;

    let mut result_config = default_config;
    merge_overrides_on_default_config(&mut result_config, overrides_map, merge_strategy);

    Ok(result_config)
}

fn resolve_user_cohort_query_from_parts(
    default_config: &Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: &MergeStrategy,
) -> Result<Map<String, Value>, String> {
    if user_cohort_dimension_names(dimensions).is_empty() {
        return Ok(query_data.clone());
    }

    let resolved_definitions = eval_config_once(
        default_config.clone(),
        contexts,
        overrides,
        dimensions,
        query_data,
        merge_strategy,
        None,
    )?;

    evaluate_cohorts_with_resolved_definitions(
        dimensions,
        query_data,
        &resolved_definitions,
    )
}

pub fn resolve_user_cohort_query(
    config: &Config,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    resolve_user_cohort_query_from_parts(
        &config.default_configs,
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        query_data,
        &merge_strategy,
    )
}

fn remove_generated_user_cohort_keys(
    resolved_config: &mut Map<String, Value>,
    dimensions: &HashMap<String, DimensionInfo>,
) {
    for dimension in user_cohort_dimension_names(dimensions) {
        resolved_config.remove(&dimension);
    }
}

fn eval_config_without_prefix(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    on_override_select: Option<&mut dyn FnMut(Context)>,
) -> Result<Map<String, Value>, String> {
    let query_data = resolve_user_cohort_query_from_parts(
        &default_config,
        contexts,
        overrides,
        dimensions,
        query_data,
        &merge_strategy,
    )?;
    let mut result_config = eval_config_once(
        default_config,
        contexts,
        overrides,
        dimensions,
        &query_data,
        &merge_strategy,
        on_override_select,
    )?;
    remove_generated_user_cohort_keys(&mut result_config, dimensions);

    Ok(result_config)
}

pub fn eval_cac(
    config: &Config,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    eval_config_without_prefix(
        (*config.default_configs).clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        query_data,
        merge_strategy,
        None,
    )
}

pub fn eval_cac_with_reasoning(
    config: &Config,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let mut reasoning = Vec::new();
    let mut result_config = eval_config_without_prefix(
        (*config.default_configs).clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        query_data,
        merge_strategy,
        Some(&mut |context| {
            reasoning.push(json!({
                "context": context.condition,
                "override": context.override_with_keys
            }));
        }),
    )?;
    result_config.insert("metadata".into(), json!(reasoning));

    Ok(result_config)
}

pub fn eval_config(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
) -> Result<Map<String, Value>, String> {
    let Some(prefixes) = filter_prefixes.filter(|prefixes| !prefixes.is_empty()) else {
        return eval_config_without_prefix(
            default_config,
            contexts,
            overrides,
            dimensions,
            query_data,
            merge_strategy,
            None,
        );
    };

    let config = Config {
        default_configs: default_config.into(),
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
        dimensions: dimensions.clone(),
    }
    .filter_by_prefix(&HashSet::from_iter(prefixes));

    eval_cac(&config, query_data, merge_strategy)
}

pub fn eval_config_with_reasoning(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>, // Optional prefix filtering
) -> Result<Map<String, Value>, String> {
    eval_config(
        default_config,
        contexts,
        overrides,
        dimensions,
        query_data,
        merge_strategy,
        filter_prefixes,
    )
}

pub fn merge(doc: &mut Value, patch: &Value) {
    if !patch.is_object() {
        *doc = patch.clone();
        return;
    }

    if !doc.is_object() {
        *doc = Value::Object(Map::new());
    }

    let map = doc.as_object_mut().unwrap();
    for (key, value) in patch.as_object().unwrap() {
        merge(map.entry(key.as_str()).or_insert(Value::Null), value);
    }
}

fn get_overrides(
    query_data: &Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    merge_strategy: &MergeStrategy,
    mut on_override_select: Option<&mut dyn FnMut(Context)>,
) -> Result<Map<String, Value>, String> {
    let mut required_overrides = Map::new();

    for context in contexts {
        if !superposition_types::apply(&context.condition, query_data) {
            continue;
        }

        let override_key = context.override_with_keys.get_key();
        let Some(overriden_value) = overrides.get(override_key) else {
            continue;
        };

        match merge_strategy {
            MergeStrategy::REPLACE => {
                for (key, value) in overriden_value.iter() {
                    required_overrides.insert(key.clone(), value.clone());
                }
            }
            MergeStrategy::MERGE => {
                for (key, value) in overriden_value.iter() {
                    merge(
                        required_overrides
                            .entry(key.as_str())
                            .or_insert(Value::Null),
                        value,
                    );
                }
            }
        }
        // Only pay for a `Context` clone when a caller actually consumes it; the
        // borrow keeps the common (callback-less) resolution path allocation-free.
        if let Some(ref mut func) = on_override_select {
            func(context.clone());
        }
    }

    Ok(required_overrides)
}

fn merge_overrides_on_default_config(
    default_config: &mut Map<String, Value>,
    overrides: Map<String, Value>,
    merge_strategy: &MergeStrategy,
) {
    overrides.into_iter().for_each(|(key, val)| {
        if let Some(og_val) = default_config.get_mut(&key) {
            match merge_strategy {
                MergeStrategy::REPLACE => {
                    let _ = default_config.insert(key.clone(), val.clone());
                }
                MergeStrategy::MERGE => merge(og_val, &val),
            }
        } else {
            log::error!("Config: found non-default_config key: {key} in overrides");
        }
    })
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use serde_json::{json, Map, Value};
    use superposition_types::{Cac, Condition, Context, OverrideWithKeys, Overrides};

    use super::*;

    fn value_map(values: Vec<(&str, Value)>) -> Map<String, Value> {
        values
            .into_iter()
            .map(|(key, value)| (key.to_string(), value))
            .collect()
    }

    fn condition(values: Vec<(&str, Value)>) -> Condition {
        Cac::<Condition>::try_from(value_map(values))
            .unwrap()
            .into_inner()
    }

    fn overrides(values: Vec<(&str, Value)>) -> Overrides {
        Cac::<Overrides>::try_from(value_map(values))
            .unwrap()
            .into_inner()
    }

    #[test]
    fn eval_config_with_reasoning_does_not_add_metadata_key() {
        let default_config = value_map(vec![("checkout.enabled", json!(false))]);
        let context = Context {
            id: "c0".to_string(),
            condition: condition(vec![("country", json!("IN"))]),
            priority: 0,
            weight: 0,
            override_with_keys: OverrideWithKeys::new("o0".to_string()),
        };
        let overrides = HashMap::from([(
            "o0".to_string(),
            overrides(vec![("checkout.enabled", json!(true))]),
        )]);
        let query_data = value_map(vec![("country", json!("IN"))]);

        let resolved = eval_config_with_reasoning(
            default_config,
            &[context],
            &overrides,
            &HashMap::new(),
            &query_data,
            MergeStrategy::MERGE,
            None,
        )
        .unwrap();

        assert_eq!(resolved.get("checkout.enabled"), Some(&json!(true)));
        assert!(!resolved.contains_key("metadata"));
    }
}
