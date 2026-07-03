use std::collections::{HashMap, HashSet};

use serde_json::{Map, Value};
pub use superposition_types::api::config::MergeStrategy;
use superposition_types::{
    logic::evaluate_local_cohorts, Config, Context, DimensionInfo, Overrides,
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

    let filter_prefixes = filter_prefixes.filter(|p| !p.is_empty());

    // Fast path: no prefix filtering. Resolve directly against the borrowed
    // contexts/overrides instead of deep-cloning the entire context set (which
    // can be hundreds of thousands of entries) into a temporary `Config`.
    let Some(prefixes) = filter_prefixes else {
        let overrides_map = get_overrides(
            &modified_query_data,
            contexts,
            overrides,
            &merge_strategy,
            None,
        )?;

        let mut result_config = default_config;
        merge_overrides_on_default_config(
            &mut result_config,
            overrides_map,
            &merge_strategy,
        );
        return Ok(result_config);
    };

    // Slow path: prefix filtering needs an owned, filtered `Config`.
    let config = Config {
        default_configs: default_config.into(),
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
        dimensions: dimensions.clone(),
    }
    .filter_by_prefix(&HashSet::from_iter(prefixes));

    let overrides_map: Map<String, Value> = get_overrides(
        &modified_query_data,
        &config.contexts,
        &config.overrides,
        &merge_strategy,
        None,
    )?;

    // Apply overrides to default config
    let mut result_config = config.default_configs;
    merge_overrides_on_default_config(&mut result_config, overrides_map, &merge_strategy);

    Ok(result_config.into_inner())
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

    let filter_prefixes = filter_prefixes.filter(|p| !p.is_empty());

    let Some(prefixes) = filter_prefixes else {
        let overrides_map = get_overrides(
            &modified_query_data,
            contexts,
            overrides,
            &merge_strategy,
            None,
        )?;

        let mut result_config = default_config;
        merge_overrides_on_default_config(
            &mut result_config,
            overrides_map,
            &merge_strategy,
        );
        return Ok(result_config);
    };

    let config = Config {
        default_configs: default_config.into(),
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
        dimensions: dimensions.clone(),
    }
    .filter_by_prefix(&HashSet::from_iter(prefixes));

    let overrides_map = get_overrides(
        &modified_query_data,
        &config.contexts,
        &config.overrides,
        &merge_strategy,
        None,
    )?;

    let mut result_config = config.default_configs;
    merge_overrides_on_default_config(&mut result_config, overrides_map, &merge_strategy);

    Ok(result_config.into_inner())
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
    // Only pay for a `Context` clone when a caller actually consumes it; the
    // borrow keeps the common (callback-less) resolution path allocation-free.
    let mut on_override_select = |context: &Context| {
        if let Some(ref mut func) = on_override_select {
            func(context.clone())
        }
    };

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
        on_override_select(context);
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
