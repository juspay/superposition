//NOTE this code is copied over from sdk-config-server with small changes for compatiblity
//TODO refactor, make eval MJOS agnostic

use std::collections::HashMap;

use crate::{utils::deep_merge::{deep_merge, patch_json}, Context};
use jsonlogic;
use serde::{de, Deserialize, Serialize};
use serde_json::{from_value, json, to_value, Map, Value};

#[derive(Deserialize, Serialize)]
pub struct PathParams {
    merchant_id: String,
    platform: String,
    environment: String,
}

#[derive(Clone, Debug, Deserialize)]
struct MJOSOverride {
    path: Vec<String>,
    value: Value,
    etag: Option<String>,
}

fn create_nested_json(path_array: Vec<String>, value: Value) -> serde_json::Result<Value> {
    path_array.iter().rev().try_fold(value, |acc, key_string| {
        to_value(HashMap::<String, Value>::from([(
            key_string.to_string(),
            acc,
        )]))
    })
}

type EtagMap = HashMap<String, String>;
pub type Patch = (Value, EtagMap);

fn form_override_object(input_config: Value) -> serde_json::Result<Patch> {
    let input_map: Map<String, Value> = from_value(input_config)?;
    let patch_entries = input_map.get("patch_entries");
    let keys_to_be_overridden: Vec<String> =
        patch_entries.map_or(Ok(Vec::new()), |x| from_value(x.to_owned()))?;
    let mut etags: EtagMap = HashMap::new();
    let patch_obj = keys_to_be_overridden
        .iter()
        .try_fold(json!({}), |acc, mapped_key| {
            if let Some(MJOSOverride {
                path: path_array,
                value,
                etag: mb_etag,
            }) = input_map
                .get(mapped_key)
                .and_then(|x| -> Option<MJOSOverride> { from_value(x.to_owned()).ok() })
            {
                if let (Some(etag), Value::String(url)) = (mb_etag, value.clone()) {
                    etags.insert(url, etag);
                }
                let split_path = &path_array
                    .split_first()
                    .map(|(first, tail)| (first.as_str(), tail));
                let entries = match split_path {
                    Some(("live", tail)) => vec![
                        path_array.clone(),
                        [vec!["new".to_string()], tail.to_vec()].concat(),
                    ],
                    Some(("new", tail)) => vec![
                        path_array.clone(),
                        [vec!["new".to_string()], tail.to_vec()].concat(),
                    ],
                    _ => vec![path_array],
                };

                return entries.iter().try_fold(acc, |accumulator, val| {
                    deep_merge(
                        &accumulator,
                        &create_nested_json(val.to_owned(), json!(value))?,
                    )
                });
            }
            Ok(acc)
        });
    patch_obj.map(|x| (x, etags))
}

fn choose_merge_strategy(
    allow_new_paths: bool,
) -> for<'a, 'b> fn(&'a Value, &'b Value) -> serde_json::Result<Value> {
    if allow_new_paths {
        deep_merge
    } else {
        patch_json
    }
}

fn get_overrides(
    query_data: &Value,
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    allow_new_paths: bool,
) -> serde_json::Result<Value> {
    let merge_fn = choose_merge_strategy(allow_new_paths);
    let mut required_overrides: Value = json!({});

    for context in contexts.iter() {
        // TODO :: Add semantic version comparator in Lib
        if let Ok(Value::Bool(true)) = jsonlogic::apply(&context.condition, query_data) {
            for override_key in &context.override_with_keys {
                if let Some(overriden_value) = overrides.get(override_key) {
                    required_overrides = merge_fn(&required_overrides, overriden_value)?;
                }
            }
        }
    }

    Ok(required_overrides)
}

pub fn eval_cac(
    default_config: Value,
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    query_data: &Value,
) -> serde_json::Result<Value> {
    // get_overrides runs json logic and gives patch_entries from overrides.json
    let overrides = get_overrides(query_data, contexts, overrides, true)?;

    // patch_entries received from overides after json logic are then merged over default_config
    deep_merge(&default_config, &overrides)
}

pub fn get_mjos_override(
    query_data: Value,
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    default_config: Value,
) -> serde_json::Result<Patch> {

    let overrides_config = eval_cac(default_config, contexts, overrides, &query_data)?;

    if Some(&json!([])) == overrides_config.get("patch_entries") {
        return Err(de::Error::custom("No patches found"));
    }
    // overriding object is then generated from default_config
    form_override_object(overrides_config)
}
