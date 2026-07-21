use std::collections::{HashMap, HashSet};

use serde_json::{Map, Value};
pub use superposition_types::api::config::MergeStrategy;
use superposition_types::{
    logic::evaluate_local_cohorts, Context, DimensionInfo, Overrides,
};

pub fn eval_config(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
) -> Result<Map<String, Value>, String> {
    // Local cohort evaluation only reads `dimensions`, which prefix filtering
    // leaves untouched, so it is safe to compute once regardless of the path.
    let modified_query_data = evaluate_local_cohorts(dimensions, query_data);

    let filter_prefixes: Option<HashSet<String>> = filter_prefixes
        .filter(|prefixes| !prefixes.is_empty())
        .map(HashSet::from_iter);

    let overrides_map: Map<String, Value> = get_overrides(
        &modified_query_data,
        contexts,
        overrides,
        &merge_strategy,
        filter_prefixes.as_ref(),
        None,
    )?;

    let mut result_config = match &filter_prefixes {
        Some(prefixes) => filter_config_keys_by_prefix(default_config, prefixes),
        None => default_config,
    };
    merge_overrides_on_default_config(&mut result_config, overrides_map, &merge_strategy);

    Ok(result_config)
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
    let modified_query_data = evaluate_local_cohorts(dimensions, query_data);

    let filter_prefixes: Option<HashSet<String>> = filter_prefixes
        .filter(|prefixes| !prefixes.is_empty())
        .map(HashSet::from_iter);

    let overrides_map = get_overrides(
        &modified_query_data,
        contexts,
        overrides,
        &merge_strategy,
        filter_prefixes.as_ref(),
        None,
    )?;

    let mut result_config = match &filter_prefixes {
        Some(prefixes) => filter_config_keys_by_prefix(default_config, prefixes),
        None => default_config,
    };
    merge_overrides_on_default_config(&mut result_config, overrides_map, &merge_strategy);

    Ok(result_config)
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

fn matches_prefix_filter(key: &str, prefix_filter: Option<&HashSet<String>>) -> bool {
    prefix_filter
        .map(|prefixes| prefixes.iter().any(|prefix| key.starts_with(prefix)))
        .unwrap_or(true)
}

fn filter_config_keys_by_prefix(
    config: Map<String, Value>,
    prefix_filter: &HashSet<String>,
) -> Map<String, Value> {
    config
        .into_iter()
        .filter(|(key, _)| matches_prefix_filter(key, Some(prefix_filter)))
        .collect()
}

fn get_overrides(
    query_data: &Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    merge_strategy: &MergeStrategy,
    prefix_filter: Option<&HashSet<String>>,
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
                    if !matches_prefix_filter(key, prefix_filter) {
                        continue;
                    }
                    required_overrides.insert(key.clone(), value.clone());
                }
            }
            MergeStrategy::MERGE => {
                for (key, value) in overriden_value.iter() {
                    if !matches_prefix_filter(key, prefix_filter) {
                        continue;
                    }
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
