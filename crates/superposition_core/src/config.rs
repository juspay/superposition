use std::collections::HashMap;

use serde_json::{json, Map, Value};
pub use superposition_types::api::config::MergeStrategy;
use superposition_types::{
    logic::evaluate_local_cohorts, Config, Context, DimensionInfo, ExtendedMap,
    Overrides, PrefixList,
};

#[allow(clippy::too_many_arguments)]
pub fn eval_config(
    default_configs: ExtendedMap,
    contexts: Vec<Context>,
    overrides: HashMap<String, Overrides>,
    dimensions: HashMap<String, DimensionInfo>,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
) -> Result<Map<String, Value>, String> {
    // Create Config struct to use existing filtering logic
    let mut config = Config {
        default_configs,
        contexts,
        overrides,
        dimensions,
    };

    let filter_prefixes = PrefixList::from(filter_prefixes);
    let filter_exclude_prefixes = PrefixList::from(filter_exclude_prefixes);

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if !filter_prefixes.is_empty() || !filter_exclude_prefixes.is_empty() {
        config = config.filter_by_prefix(&filter_prefixes, &filter_exclude_prefixes);
    }

    let modified_query_data = evaluate_local_cohorts(&config.dimensions, query_data);

    let overrides_map: Map<String, Value> = get_overrides(
        &modified_query_data,
        config.contexts,
        config.overrides,
        &merge_strategy,
        drop,
    )?;

    // Apply overrides to default config
    let result_config = merge_overrides_on_default_config(
        config.default_configs,
        overrides_map,
        merge_strategy,
    );

    Ok(result_config.into_inner())
}

#[allow(clippy::too_many_arguments)]
pub fn eval_config_with_reasoning(
    default_configs: ExtendedMap,
    contexts: Vec<Context>,
    overrides: HashMap<String, Overrides>,
    dimensions: HashMap<String, DimensionInfo>,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>, // Optional prefix filtering
    filter_exclude_prefixes: Option<Vec<String>>, // Optional exclude prefix filtering
) -> Result<Map<String, Value>, String> {
    let mut reasoning: Vec<Value> = vec![];

    let mut config = Config {
        default_configs,
        contexts,
        overrides,
        dimensions,
    };

    let filter_prefixes = PrefixList::from(filter_prefixes);
    let filter_exclude_prefixes = PrefixList::from(filter_exclude_prefixes);

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if !filter_prefixes.is_empty() || !filter_exclude_prefixes.is_empty() {
        config = config.filter_by_prefix(&filter_prefixes, &filter_exclude_prefixes);
    }

    let reasoning_collector = |context: Context| {
        reasoning.push(json!({
            "context": context.condition,
            "override": context.override_with_keys
        }));
    };

    let modified_query_data = evaluate_local_cohorts(&config.dimensions, query_data);

    let overrides_map = get_overrides(
        &modified_query_data,
        config.contexts,
        config.overrides,
        &merge_strategy,
        reasoning_collector,
    )?;

    let mut result_config = merge_overrides_on_default_config(
        config.default_configs,
        overrides_map,
        merge_strategy,
    );

    // Add reasoning metadata
    result_config.insert("metadata".into(), json!(reasoning));

    Ok(result_config.into_inner())
}

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

/// Merges the overrides of every context matching `query_data` into a single map,
/// in context order, so that later contexts win.
///
/// Override ids are derived from the override's contents, so several contexts can
/// share one and each must apply it at its own position. An override is therefore
/// cloned only when more than one matching context needs it, and the last of those
/// contexts moves it out of `overrides` rather than cloning.
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
    merge_strategy: MergeStrategy,
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
            log::error!("Config: found non-default_config key: {key} in overrides");
        }
    });
    default_config
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
        let default_config =
            ExtendedMap::from(value_map(vec![("checkout.enabled", json!(false))]));
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
            vec![context],
            overrides,
            HashMap::new(),
            query_data,
            MergeStrategy::MERGE,
            None,
            None,
        )
        .unwrap();

        assert_eq!(resolved.get("checkout.enabled"), Some(&json!(true)));
        assert!(!resolved.contains_key("metadata"));
    }
}
