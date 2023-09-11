//NOTE this code is copied over from sdk-config-server with small changes for compatiblity
//TODO refactor, make eval MJOS agnostic

use crate::{Context};
use jsonlogic;
use serde_json::{json, Map, Value};


fn get_overrides(
    query_data: &Value,
    contexts: &Vec<Context>,
    overrides: &Map<String, Value>,
) -> serde_json::Result<Value> {
    let mut required_overrides: Value = json!({});

    for context in contexts.iter() {
        // TODO :: Add semantic version comparator in Lib
        if let Ok(Value::Bool(true)) = jsonlogic::apply(&context.condition, query_data) {
            for override_key in &context.override_with_keys {
                if let Some(overriden_value) = overrides.get(override_key) {
                    json_patch::merge(&mut required_overrides, overriden_value)
                }
            }
        }
    }

    Ok(required_overrides)
}

pub fn eval_cac(
    mut default_config: Value,
    contexts: &Vec<Context>,
    overrides: &Map<String, Value>,
    query_data: &Value,
) -> serde_json::Result<Map<String, Value>> {
    let overrides = get_overrides(&query_data, &contexts, &overrides)?;
    json_patch::merge(&mut default_config, &overrides);
    serde_json::from_value(default_config)
}