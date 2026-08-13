use std::{borrow::Cow, collections::HashMap};

use serde_json::{json, Map, Value};
use superposition_types::{
    logic::evaluate_local_cohorts, Config, DimensionInfo, ExtendedMap, Overrides,
};

use crate::{Context, MergeStrategy};

fn merge(doc: &mut Value, patch: Value) {
    match (doc, patch) {
        (Value::Object(map), Value::Object(obj)) => {
            for (key, value) in obj {
                merge(map.entry(key).or_insert(Value::Null), value);
            }
        }
        (doc, patch) => *doc = patch,
    }
}

/// Merges the overrides of every context matching `query_data` into a single map,
/// in context order, so that later contexts win.
///
/// Override ids are derived from the override's contents, so several contexts can
/// share one and each must apply it at its own position. An override is therefore
/// cloned only when more than one matching context needs it, and the last of those
/// contexts moves it out of `overrides` rather than cloning.
fn get_overrides(
    query_data: &Map<String, Value>,
    mut contexts: Vec<&Context>,
    mut overrides: Cow<'_, HashMap<String, Overrides>>,
    merge_strategy: &MergeStrategy,
) -> Map<String, Value> {
    // borrow the keys instead of allocating two Strings per matching context
    let mut final_consumer_context: HashMap<&str, &str> = HashMap::new();

    contexts.retain(|&context| {
        if !superposition_types::apply(&context.condition, query_data) {
            return false;
        }
        final_consumer_context.insert(
            context.override_with_keys.get_key().as_str(),
            context.id.as_str(),
        );
        true
    });

    let mut required_overrides: Value = json!({});

    for context in contexts {
        let override_key = context.override_with_keys.get_key();
        let is_last = final_consumer_context.get(override_key.as_str())
            == Some(&context.id.as_str());

        // move it out only when we own the map *and* this is its last consumer
        let overriden_value = match (&mut overrides, is_last) {
            (Cow::Owned(map), true) => map.remove(override_key).map(Cow::Owned),
            (map, _) => map.get(override_key).map(Cow::Borrowed),
        };

        let Some(overriden_value) = overriden_value else {
            continue;
        };

        match merge_strategy {
            MergeStrategy::REPLACE => {
                if let Some(doc) = required_overrides.as_object_mut() {
                    for (key, value) in overriden_value.into_owned().into_inner() {
                        doc.insert(key, value);
                    }
                }
            }
            MergeStrategy::MERGE => merge(
                &mut required_overrides,
                Value::Object(overriden_value.into_owned().into_inner()),
            ),
        }
    }

    match required_overrides {
        Value::Object(map) => map,
        _ => Map::new(),
    }
}

fn apply_overrides_on_default_config(
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

/// To be used when you have full ownership of the `Config` struct and
/// want to evaluate it with a given set of dimensions.
pub fn eval_cac(
    config: Config,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Map<String, Value> {
    let modified_query_data = evaluate_local_cohorts(&config.dimensions, query_data);

    let overrides_map: Map<String, Value> = get_overrides(
        &modified_query_data,
        config.contexts.iter().collect(),
        Cow::Owned(config.overrides),
        &merge_strategy,
    );

    // Apply overrides to default config
    let result = apply_overrides_on_default_config(
        config.default_configs,
        overrides_map,
        merge_strategy,
    );

    result.into_inner()
}

/// To be used when the ownership of the `Config` struct is `not available`
/// and you want to evaluate it with a given set of dimensions.
pub fn eval(
    default_configs: ExtendedMap,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Map<String, Value> {
    let modified_query_data = evaluate_local_cohorts(dimensions, query_data);

    let overrides_map = get_overrides(
        &modified_query_data,
        contexts.iter().collect(),
        Cow::Borrowed(overrides),
        &merge_strategy,
    );

    // Apply overrides to default config
    let result =
        apply_overrides_on_default_config(default_configs, overrides_map, merge_strategy);

    result.into_inner()
}
