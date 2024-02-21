//NOTE this code is copied over from sdk-config-server with small changes for compatiblity
//TODO refactor, make eval MJOS agnostic

use crate::{utils::core::MapError, Context};
use jsonlogic;
use serde_json::{json, Map, Value};

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
    contexts: &Vec<Context>,
    overrides: &Map<String, Value>,
    mut on_override_select: Option<&mut dyn FnMut(Context)>,
) -> serde_json::Result<Value> {
    let mut required_overrides: Value = json!({});

    for context in contexts.iter() {
        // TODO :: Add semantic version comparator in Lib
        if let Ok(Value::Bool(true)) =
            jsonlogic::apply(&context.condition, &json!(query_data))
        {
            for override_key in &context.override_with_keys {
                if let Some(overriden_value) = overrides.get(override_key) {
                    merge(&mut required_overrides, overriden_value);
                    match on_override_select {
                        Some(ref mut func) => func(context.clone()),
                        None => (),
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
) {
    overrides.into_iter().for_each(|(key, val)| {
        if let Some(og_val) = default_config.get_mut(&key) {
            merge(og_val, &val)
        } else {
            log::error!("CAC: found non-default_config key: {key} in overrides");
        }
    })
}

pub fn eval_cac(
    mut default_config: Map<String, Value>,
    contexts: &Vec<Context>,
    overrides: &Map<String, Value>,
    query_data: &Map<String, Value>,
) -> Result<Map<String, Value>, String> {
    let on_override_select: Option<&mut dyn FnMut(Context)> = None;
    let overrides: Map<String, Value> =
        get_overrides(&query_data, &contexts, &overrides, on_override_select)
            .and_then(serde_json::from_value)
            .map_err_to_string()?;
    merge_overrides_on_default_config(&mut default_config, overrides);
    let overriden_config = default_config;
    Ok(overriden_config)
}

pub fn eval_cac_with_reasoning(
    mut default_config: Map<String, Value>,
    contexts: &Vec<Context>,
    overrides: &Map<String, Value>,
    query_data: &Map<String, Value>,
) -> Result<Map<String, Value>, String> {
    let mut reasoning: Vec<Value> = vec![];

    let applied_overrides: Map<String, Value> = get_overrides(
        &query_data,
        &contexts,
        &overrides,
        Some(&mut |context| {
            reasoning.push(json!({
                "context": context.condition,
                "override": context.override_with_keys
            }))
        }),
    )
    .and_then(serde_json::from_value)
    .map_err_to_string()?;

    merge_overrides_on_default_config(&mut default_config, applied_overrides);
    let mut overriden_config = default_config;
    overriden_config.insert("metadata".into(), json!(reasoning));
    Ok(overriden_config)
}
