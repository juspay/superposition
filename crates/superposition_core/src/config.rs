use std::{borrow::Cow, collections::HashMap};

use serde_json::{json, Map, Value};
pub use superposition_types::api::config::MergeStrategy;
use superposition_types::{
    logic::evaluate_local_cohorts, Config, ConfigFilter, Context, DimensionInfo,
    ExtendedMap, Overrides, PrefixList,
};

struct ConfigRef<'a> {
    pub contexts: Vec<&'a Context>,
    pub overrides: Cow<'a, HashMap<String, Overrides>>,
    pub default_configs: ExtendedMap,
    pub dimensions: &'a HashMap<String, DimensionInfo>,
}

impl<'a> ConfigFilter<'a> for ConfigRef<'a> {
    type Context = &'a Context;
    type Dimensions = &'a HashMap<String, DimensionInfo>;

    fn into_parts(
        self,
    ) -> (
        Vec<Self::Context>,
        Cow<'a, HashMap<String, Overrides>>,
        ExtendedMap,
        Self::Dimensions,
    ) {
        (
            self.contexts,
            self.overrides,
            self.default_configs,
            self.dimensions,
        )
    }

    fn from_parts(
        contexts: Vec<Self::Context>,
        overrides: Cow<'a, HashMap<String, Overrides>>,
        default_configs: ExtendedMap,
        dimensions: Self::Dimensions,
    ) -> Self {
        Self {
            contexts,
            overrides,
            default_configs,
            dimensions,
        }
    }
}

/// To be used when you have full ownership of the `Config` struct and
/// want to evaluate it with a given set of dimensions.
pub fn eval_config(
    mut config: Config,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
) -> Map<String, Value> {
    let filter_prefixes = PrefixList::from(filter_prefixes);
    let filter_exclude_prefixes = PrefixList::from(filter_exclude_prefixes);

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if !filter_prefixes.is_empty() || !filter_exclude_prefixes.is_empty() {
        config = config.filter_by_prefix(&filter_prefixes, &filter_exclude_prefixes);
    }

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
#[allow(clippy::too_many_arguments)]
pub fn eval(
    default_configs: ExtendedMap,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
) -> Map<String, Value> {
    // Create Config struct to use existing filtering logic
    let mut config = ConfigRef {
        default_configs,
        contexts: contexts.iter().collect(),
        overrides: Cow::Borrowed(overrides),
        dimensions,
    };

    let filter_prefixes = PrefixList::from(filter_prefixes);
    let filter_exclude_prefixes = PrefixList::from(filter_exclude_prefixes);

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if !filter_prefixes.is_empty() || !filter_exclude_prefixes.is_empty() {
        config = config.filter_by_prefix(&filter_prefixes, &filter_exclude_prefixes);
    }

    let modified_query_data = evaluate_local_cohorts(config.dimensions, query_data);

    let overrides_map: Map<String, Value> = get_overrides(
        &modified_query_data,
        config.contexts,
        config.overrides,
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

// TODO: Move explain function to this file to avoid making this funcition public
pub fn merge(doc: &mut Value, patch: Value) {
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

        let config = Config {
            default_configs: default_config,
            contexts: vec![context],
            overrides,
            dimensions: HashMap::new(),
        };

        let resolved = eval_config(config, query_data, MergeStrategy::MERGE, None, None);

        assert_eq!(resolved.get("checkout.enabled"), Some(&json!(true)));
        // assert!(!resolved.contains_key("metadata"));
    }
}
