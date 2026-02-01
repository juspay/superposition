use std::collections::HashMap;

use serde_json::{json, Map, Value};
use superposition_types::{logic::evaluate_local_cohorts, Config, Overrides};

use crate::{utils::core::MapError, Context, MergeStrategy};

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

fn get_overrides(
    query_data: &Map<String, Value>,
    contexts: Vec<Context>,
    mut overrides: HashMap<String, Overrides>,
    merge_strategy: &MergeStrategy,
    mut on_override_select: Option<&mut dyn FnMut(Context)>,
) -> serde_json::Result<Value> {
    let mut required_overrides: Value = json!({});
    let mut on_override_select = |context: Context| {
        if let Some(ref mut func) = on_override_select {
            func(context)
        }
    };

    for context in contexts {
        let valid_context = superposition_types::apply(&context.condition, query_data);

        if valid_context {
            let override_key = context.override_with_keys.get_key();
            if let Some(overriden_value) = overrides.remove(override_key) {
                match merge_strategy {
                    MergeStrategy::REPLACE => {
                        if let Some(doc) = required_overrides.as_object_mut() {
                            for (key, value) in overriden_value.into_inner() {
                                doc.insert(key, value);
                            }
                            on_override_select(context)
                        }
                    }
                    MergeStrategy::MERGE => {
                        merge(
                            &mut required_overrides,
                            Value::Object(overriden_value.into_inner()),
                        );
                        on_override_select(context)
                    }
                }
            }
        }
    }

    Ok(required_overrides)
}

fn merge_overrides_on_default_config(
    mut default_config: Map<String, Value>,
    overrides: Map<String, Value>,
    merge_strategy: &MergeStrategy,
) -> Map<String, Value> {
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
    let on_override_select: Option<&mut dyn FnMut(Context)> = None;
    let modified_query_data = evaluate_local_cohorts(config.dimensions, query_data);
    let overrides: Map<String, Value> = get_overrides(
        &modified_query_data,
        config.contexts,
        config.overrides,
        &merge_strategy,
        on_override_select,
    )
    .and_then(serde_json::from_value)
    .map_err_to_string()?;
    let overriden_config = merge_overrides_on_default_config(
        config.default_configs,
        overrides,
        &merge_strategy,
    );
    Ok(overriden_config)
}

pub fn eval_cac_with_reasoning(
    config: Config,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let mut reasoning: Vec<Value> = vec![];

    let modified_query_data = evaluate_local_cohorts(config.dimensions, query_data);

    let applied_overrides: Map<String, Value> = get_overrides(
        &modified_query_data,
        config.contexts,
        config.overrides,
        &merge_strategy,
        Some(&mut |context| {
            reasoning.push(json!({
                "context": context.condition,
                "override": context.override_with_keys
            }))
        }),
    )
    .and_then(serde_json::from_value)
    .map_err_to_string()?;

    let mut overriden_config = merge_overrides_on_default_config(
        config.default_configs,
        applied_overrides,
        &merge_strategy,
    );
    overriden_config.insert("metadata".into(), json!(reasoning));
    Ok(overriden_config)
}
