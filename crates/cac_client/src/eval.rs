//NOTE this code is copied over from sdk-config-server with small changes for compatiblity
//TODO refactor, make eval MJOS agnostic

use std::collections::HashMap;

use crate::{utils::core::MapError, Context, MergeStrategy};
use serde_json::{json, Map, Value};
use superposition_types::Overrides;

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

fn replace_top_level(
    doc: &mut Map<String, Value>,
    patch: &Value,
    mut on_override: impl FnMut(),
    override_key: &String,
) {
    match patch.as_object() {
        Some(patch_map) => {
            for (key, value) in patch_map {
                doc.insert(key.clone(), value.clone());
            }
            on_override();
        }
        None => {
            log::error!("CAC: found non-object override key: {override_key} in overrides")
        }
    }
}

fn get_overrides(
    query_data: &Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    merge_strategy: &MergeStrategy,
    mut on_override_select: Option<&mut dyn FnMut(Context)>,
) -> serde_json::Result<Value> {
    let mut required_overrides: Value = json!({});
    let mut on_override_select = |context: Context| {
        if let Some(ref mut func) = on_override_select {
            func(context)
        }
    };

    #[cfg(feature = "jsonlogic")]
    let query_data = Value::Object(query_data.clone());

    for context in contexts {
        cfg_if::cfg_if! {
            if #[cfg(feature = "jsonlogic")] {
                let valid_context = jsonlogic::apply(
                    &Value::Object(context.condition.clone().into()),
                    &query_data,
                ) == Ok(Value::Bool(true));
            } else {
                let valid_context = superposition_types::apply(&context.condition, query_data);
            }
        }

        if valid_context {
            let override_key = context.override_with_keys.get_key();
            if let Some(overriden_value) = overrides.get(override_key) {
                match merge_strategy {
                    MergeStrategy::REPLACE => replace_top_level(
                        required_overrides.as_object_mut().unwrap(),
                        &Value::Object(overriden_value.clone().into()),
                        || on_override_select(context.clone()),
                        override_key,
                    ),
                    MergeStrategy::MERGE => {
                        merge(
                            &mut required_overrides,
                            &Value::Object(overriden_value.clone().into()),
                        );
                        on_override_select(context.clone())
                    }
                }
            }
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
            log::error!("CAC: found non-default_config key: {key} in overrides");
        }
    })
}

pub fn eval_cac(
    mut default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let on_override_select: Option<&mut dyn FnMut(Context)> = None;
    let overrides: Map<String, Value> = get_overrides(
        query_data,
        contexts,
        overrides,
        &merge_strategy,
        on_override_select,
    )
    .and_then(serde_json::from_value)
    .map_err_to_string()?;
    merge_overrides_on_default_config(&mut default_config, overrides, &merge_strategy);
    let overriden_config = default_config;
    Ok(overriden_config)
}

pub fn eval_cac_with_reasoning(
    mut default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let mut reasoning: Vec<Value> = vec![];

    let applied_overrides: Map<String, Value> = get_overrides(
        query_data,
        contexts,
        overrides,
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

    merge_overrides_on_default_config(
        &mut default_config,
        applied_overrides,
        &merge_strategy,
    );
    let mut overriden_config = default_config;
    overriden_config.insert("metadata".into(), json!(reasoning));
    Ok(overriden_config)
}
