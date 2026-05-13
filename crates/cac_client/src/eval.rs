use std::collections::HashMap;

use serde_json::{json, Map, Value};
use superposition_types::{logic::evaluate_local_cohorts, Config, Overrides};

use crate::{utils::core::MapError, Context, MergeStrategy};

type KeyTransitions = Vec<Value>;
type ReasoningHook<'a> = &'a mut dyn FnMut(Context, &Overrides, KeyTransitions);

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

fn merged_value(
    previous: Option<&Value>,
    patch: &Value,
    merge_strategy: &MergeStrategy,
) -> Value {
    match merge_strategy {
        MergeStrategy::REPLACE => patch.clone(),
        MergeStrategy::MERGE => {
            let mut next = previous.cloned().unwrap_or(Value::Null);
            merge(&mut next, patch);
            next
        }
    }
}

fn build_key_transitions(
    current_overrides: &Value,
    override_patch: &Overrides,
    merge_strategy: &MergeStrategy,
) -> KeyTransitions {
    let current_map = current_overrides.as_object();

    override_patch
        .iter()
        .map(|(key, patch)| {
            let previous = current_map.and_then(|map| map.get(key)).cloned();
            let next = merged_value(previous.as_ref(), patch, merge_strategy);

            json!({
                "key": key,
                "previous": previous,
                "next": next,
            })
        })
        .collect()
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
    mut on_override_select: Option<ReasoningHook<'_>>,
) -> serde_json::Result<Value> {
    let mut required_overrides: Value = json!({});
    let mut on_override_select =
        |context: Context,
         override_patch: &Overrides,
         key_transitions: KeyTransitions| {
            if let Some(ref mut func) = on_override_select {
                func(context, override_patch, key_transitions)
            }
        };

    for context in contexts {
        let valid_context = superposition_types::apply(&context.condition, query_data);

        if valid_context {
            let override_key = context.override_with_keys.get_key();
            if let Some(overriden_value) = overrides.get(override_key) {
                let key_transitions = build_key_transitions(
                    &required_overrides,
                    overriden_value,
                    merge_strategy,
                );
                match merge_strategy {
                    MergeStrategy::REPLACE => replace_top_level(
                        required_overrides.as_object_mut().unwrap(),
                        &Value::Object(overriden_value.clone().into()),
                        || {
                            on_override_select(
                                context.clone(),
                                overriden_value,
                                key_transitions.clone(),
                            )
                        },
                        override_key,
                    ),
                    MergeStrategy::MERGE => {
                        merge(
                            &mut required_overrides,
                            &Value::Object(overriden_value.clone().into()),
                        );
                        on_override_select(
                            context.clone(),
                            overriden_value,
                            key_transitions,
                        )
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
    config: &Config,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let mut default_config = (*config.default_configs).clone();
    let on_override_select: Option<ReasoningHook<'_>> = None;
    let modified_query_data = evaluate_local_cohorts(&config.dimensions, query_data);
    let overrides: Map<String, Value> = get_overrides(
        &modified_query_data,
        &config.contexts,
        &config.overrides,
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
    config: &Config,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let mut default_config = (*config.default_configs).clone();
    let mut reasoning: Map<String, Value> = Map::new();

    let modified_query_data = evaluate_local_cohorts(&config.dimensions, query_data);

    let applied_overrides: Map<String, Value> = get_overrides(
        &modified_query_data,
        &config.contexts,
        &config.overrides,
        &merge_strategy,
        Some(&mut |context, _override_patch, key_transitions| {
            let override_id = context.override_with_keys.get_key().clone();

            for transition in key_transitions {
                let Some(key) = transition
                    .get("key")
                    .and_then(Value::as_str)
                    .map(ToOwned::to_owned)
                else {
                    continue;
                };

                let history = reasoning
                    .entry(key)
                    .or_insert_with(|| Value::Array(vec![]));

                if let Value::Array(entries) = history {
                    entries.push(json!({
                        "context_id": context.id.clone(),
                        "condition": context.condition.clone(),
                        "priority": context.priority,
                        "weight": context.weight,
                        "override_id": override_id.clone(),
                        "previous": transition.get("previous").cloned().unwrap_or(Value::Null),
                        "next": transition.get("next").cloned().unwrap_or(Value::Null),
                    }));
                }
            }
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

#[cfg(test)]
mod tests {
    use super::*;
    use superposition_types::{Context, ExtendedMap, OverrideWithKeys};

    fn test_context(
        id: &str,
        condition: Map<String, Value>,
        override_id: &str,
    ) -> Context {
        Context {
            id: id.to_string(),
            condition: serde_json::from_value(Value::Object(condition)).unwrap(),
            priority: 1,
            weight: 1,
            override_with_keys: OverrideWithKeys::new(override_id.to_string()),
        }
    }

    fn base_config() -> Config {
        let mut default_configs = Map::new();
        default_configs.insert("featureA".to_string(), json!(false));
        default_configs.insert("settings".to_string(), json!({ "mode": "default" }));

        let mut first_override = Map::new();
        first_override.insert("featureA".to_string(), json!(true));
        first_override.insert("settings".to_string(), json!({ "mode": "first" }));

        let mut second_override = Map::new();
        second_override.insert("settings".to_string(), json!({ "mode": "second" }));

        let mut first_condition = Map::new();
        first_condition.insert("clientId".to_string(), json!("android"));

        let mut second_condition = Map::new();
        second_condition.insert("country".to_string(), json!("IN"));

        Config {
            contexts: vec![
                test_context("ctx-1", first_condition, "override-1"),
                test_context("ctx-2", second_condition, "override-2"),
            ],
            overrides: HashMap::from([
                (
                    "override-1".to_string(),
                    serde_json::from_value(Value::Object(first_override)).unwrap(),
                ),
                (
                    "override-2".to_string(),
                    serde_json::from_value(Value::Object(second_override)).unwrap(),
                ),
            ]),
            default_configs: ExtendedMap::from(default_configs),
            dimensions: HashMap::new(),
        }
    }

    #[test]
    fn eval_cac_with_reasoning_groups_metadata_by_config_key() {
        let config = base_config();
        let query = Map::from_iter([
            ("clientId".to_string(), json!("android")),
            ("country".to_string(), json!("IN")),
        ]);

        let resolved = eval_cac_with_reasoning(&config, &query, MergeStrategy::MERGE)
            .expect("reasoning config should resolve");

        let metadata = resolved
            .get("metadata")
            .and_then(Value::as_object)
            .expect("metadata should be grouped by config key");

        let settings_history = metadata
            .get("settings")
            .and_then(Value::as_array)
            .expect("settings history should be recorded");
        assert_eq!(settings_history.len(), 2);
        assert_eq!(settings_history[0]["previous"], Value::Null);
        assert_eq!(settings_history[1]["previous"], json!({ "mode": "first" }));
        assert_eq!(settings_history[1]["next"], json!({ "mode": "second" }));

        let feature_history = metadata
            .get("featureA")
            .and_then(Value::as_array)
            .expect("featureA history should be recorded");
        assert_eq!(feature_history.len(), 1);
        assert_eq!(feature_history[0]["next"], json!(true));
    }

    #[test]
    fn eval_cac_without_reasoning_omits_metadata() {
        let config = base_config();
        let query = Map::from_iter([
            ("clientId".to_string(), json!("android")),
            ("country".to_string(), json!("IN")),
        ]);

        let resolved = eval_cac(&config, &query, MergeStrategy::MERGE)
            .expect("config should resolve");

        assert!(resolved.get("metadata").is_none());
    }
}
