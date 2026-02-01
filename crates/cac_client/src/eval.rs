use std::collections::HashMap;

use serde_json::{json, Map, Value};
use superposition_types::{
    logic::evaluate_local_cohorts, Config, ExtendedMap, Overrides,
};

use crate::{Context, MergeStrategy};

pub fn merge(doc: &mut Value, patch: Value) {
    if !patch.is_object() {
        *doc = patch;
        return;
    }

    if !doc.is_object() {
        *doc = Value::Object(Map::new());
    }

    if let (Some(map), Value::Object(obj)) = (doc.as_object_mut(), patch) {
        for (key, value) in obj {
            merge(map.entry(key.as_str()).or_insert(Value::Null), value);
        }
    }
}

fn get_overrides<F: FnMut(Context)>(
    query_data: &Map<String, Value>,
    contexts: Vec<Context>,
    mut overrides: HashMap<String, Overrides>,
    merge_strategy: &MergeStrategy,
    mut on_override_select: F,
) -> Result<Map<String, Value>, String> {
    let mut final_consumer_context: HashMap<String, String> = HashMap::new();

    let contexts = contexts
        .into_iter()
        .filter(|context| {
            if !superposition_types::apply(&context.condition, query_data) {
                return false;
            }

            final_consumer_context.insert(
                context.override_with_keys.get_key().to_string(),
                context.id.to_string(),
            );
            true
        })
        .collect::<Vec<_>>();

    let mut required_overrides: Value = json!({});

    for context in contexts {
        let override_key = context.override_with_keys.get_key();
        // the last matching context needing an override moves it out, the rest clone
        let overriden_value =
            if final_consumer_context.get(override_key.as_str()) == Some(&context.id) {
                overrides.remove(override_key)
            } else {
                overrides.get(override_key).cloned()
            };

        let Some(overriden_value) = overriden_value else {
            continue;
        };

        match merge_strategy {
            MergeStrategy::REPLACE => {
                if let Some(doc) = required_overrides.as_object_mut() {
                    for (key, value) in overriden_value.into_inner() {
                        doc.insert(key, value);
                    }
                }
            }
            MergeStrategy::MERGE => {
                merge(
                    &mut required_overrides,
                    Value::Object(overriden_value.into_inner()),
                );
            }
        }
        on_override_select(context)
    }

    match required_overrides {
        Value::Object(map) => Ok(map),
        _ => Err("Failed to create overrides map".to_string()),
    }
}

fn merge_overrides_on_default_config(
    mut default_config: ExtendedMap,
    overrides: Map<String, Value>,
    merge_strategy: &MergeStrategy,
) -> ExtendedMap {
    overrides.into_iter().for_each(|(key, val)| {
        if let Some(og_val) = default_config.get_mut(&key) {
            match merge_strategy {
                MergeStrategy::REPLACE => {
                    default_config.insert(key, val);
                }
                MergeStrategy::MERGE => merge(og_val, val),
            }
        } else {
            log::error!("CAC: found non-default_config key: {key} in overrides");
        }
    });
    default_config
}

pub fn eval_cac(
    config: Config,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let modified_query_data = evaluate_local_cohorts(&config.dimensions, query_data);
    let overrides: Map<String, Value> = get_overrides(
        &modified_query_data,
        config.contexts,
        config.overrides,
        &merge_strategy,
        drop,
    )?;
    let overriden_config = merge_overrides_on_default_config(
        config.default_configs,
        overrides,
        &merge_strategy,
    );
    Ok(overriden_config.into_inner())
}

pub fn eval_cac_with_reasoning(
    config: Config,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let mut reasoning: Vec<Value> = vec![];

    let modified_query_data = evaluate_local_cohorts(&config.dimensions, query_data);

    let applied_overrides: Map<String, Value> = get_overrides(
        &modified_query_data,
        config.contexts,
        config.overrides,
        &merge_strategy,
        |context| {
            reasoning.push(json!({
                "context": context.condition,
                "override": context.override_with_keys
            }))
        },
    )?;

    let mut overriden_config = merge_overrides_on_default_config(
        config.default_configs,
        applied_overrides,
        &merge_strategy,
    );
    overriden_config.insert("metadata".into(), json!(reasoning));
    Ok(overriden_config.into_inner())
}
