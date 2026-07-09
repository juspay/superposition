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
    let modified_query_data = evaluate_local_cohorts(dimensions, query_data);

    let filter_prefixes = filter_prefixes.filter(|p| !p.is_empty());

    let Some(prefixes) = filter_prefixes else {
        let mut result_config = default_config;
        resolve_overrides_on_default_config(
            &mut result_config,
            contexts,
            overrides,
            &modified_query_data,
            &merge_strategy,
            None,
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

    let mut result_config = config.default_configs;
    resolve_overrides_on_default_config(
        &mut result_config,
        &config.contexts,
        &config.overrides,
        &modified_query_data,
        &merge_strategy,
        None,
    );

    Ok(result_config.into_inner())
}

pub fn eval_config_with_reasoning(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
) -> Result<Map<String, Value>, String> {
    let modified_query_data = evaluate_local_cohorts(dimensions, query_data);

    let filter_prefixes = filter_prefixes.filter(|p| !p.is_empty());

    let Some(prefixes) = filter_prefixes else {
        let mut result_config = default_config;
        resolve_overrides_on_default_config(
            &mut result_config,
            contexts,
            overrides,
            &modified_query_data,
            &merge_strategy,
            None,
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

    let mut result_config = config.default_configs;
    resolve_overrides_on_default_config(
        &mut result_config,
        &config.contexts,
        &config.overrides,
        &modified_query_data,
        &merge_strategy,
        None,
    );

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

const PARALLEL_THRESHOLD: usize = 1000;

fn compute_thread_count(len: usize) -> usize {
    if len <= PARALLEL_THRESHOLD {
        return 1;
    }
    let max = std::thread::available_parallelism()
        .map(|p| p.get())
        .unwrap_or(1);
    if max <= 1 {
        return 1;
    }
    let y = (len as f64 / PARALLEL_THRESHOLD as f64)
        .log2()
        .ceil() as usize
        + 1;
    y.min(max)
}

fn resolve_overrides_on_default_config(
    default_config: &mut Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    query_data: &Map<String, Value>,
    merge_strategy: &MergeStrategy,
    on_override_select: Option<&mut dyn FnMut(Context)>,
) {
    let num_threads = compute_thread_count(contexts.len());
    resolve_overrides_impl(
        default_config,
        contexts,
        overrides,
        query_data,
        merge_strategy,
        on_override_select,
        num_threads,
    );
}

pub fn resolve_overrides_on_default_config_singlethreaded(
    default_config: &mut Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    query_data: &Map<String, Value>,
    merge_strategy: &MergeStrategy,
    on_override_select: Option<&mut dyn FnMut(Context)>,
) {
    resolve_overrides_impl(
        default_config,
        contexts,
        overrides,
        query_data,
        merge_strategy,
        on_override_select,
        1,
    );
}

fn resolve_overrides_impl(
    default_config: &mut Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    query_data: &Map<String, Value>,
    merge_strategy: &MergeStrategy,
    mut on_override_select: Option<&mut dyn FnMut(Context)>,
    num_threads: usize,
) {
    let n = contexts.len();
    let chunk = n.div_ceil(num_threads);

    let indices: Vec<usize> = if num_threads == 1 {
        contexts
            .iter()
            .enumerate()
            .filter(|(_, ctx)| {
                superposition_types::apply(&ctx.condition, query_data)
                    && overrides.contains_key(ctx.override_with_keys.get_key())
            })
            .map(|(i, _)| i)
            .collect()
    } else {
        // Not built for wasm32 currently; std::thread::scope is unavailable there.
        std::thread::scope(|s| {
            (0..num_threads)
                .map(|t| {
                    let start = t * chunk;
                    let end = start.saturating_add(chunk).min(n);
                    s.spawn(move || {
                        contexts[start..end]
                            .iter()
                            .enumerate()
                            .filter(|(_, ctx)| {
                                superposition_types::apply(&ctx.condition, query_data)
                                    && overrides.contains_key(ctx.override_with_keys.get_key())
                            })
                            .map(|(i, _)| start + i)
                            .collect::<Vec<_>>()
                    })
                })
                .flat_map(|h| h.join().unwrap())
                .collect()
        })
    };

    let mut required_overrides = Map::new();
    for &i in &indices {
        let context = &contexts[i];
        let Some(overriden_value) = overrides.get(context.override_with_keys.get_key()) else {
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
        if let Some(ref mut func) = on_override_select {
            func(context.clone());
        }
    }

    for (key, val) in required_overrides {
        if let Some(og_val) = default_config.get_mut(&key) {
            match merge_strategy {
                MergeStrategy::REPLACE => {
                    let _ = default_config.insert(key.clone(), val);
                }
                MergeStrategy::MERGE => merge(og_val, &val),
            }
        } else {
            log::error!("Config: found non-default_config key: {key} in overrides");
        }
    }
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
