use std::collections::{HashMap, HashSet};

use serde_json::{json, Map, Value};
pub use superposition_types::api::config::MergeStrategy;
use superposition_types::{
    logic::evaluate_local_cohorts, Config, Context, DimensionInfo, ExtendedMap, Overrides,
};

pub fn eval_config(
    default_configs: ExtendedMap,
    contexts: Vec<Context>,
    overrides: HashMap<String, Overrides>,
    dimensions: HashMap<String, DimensionInfo>,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
) -> Result<Map<String, Value>, String> {
    // Create Config struct to use existing filtering logic
    let mut config = Config {
        default_configs,
        contexts,
        overrides,
        dimensions,
    };

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if let Some(prefixes) = filter_prefixes {
        if !prefixes.is_empty() {
            config =
                config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
        }
    }

    let modified_query_data = evaluate_local_cohorts(config.dimensions, query_data);

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
        &merge_strategy,
    );

    Ok(result_config.into_inner())
}

pub fn eval_config_with_reasoning(
    default_configs: ExtendedMap,
    contexts: Vec<Context>,
    overrides: HashMap<String, Overrides>,
    dimensions: HashMap<String, DimensionInfo>,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>, // Optional prefix filtering
) -> Result<Map<String, Value>, String> {
    let mut reasoning: Vec<Value> = vec![];

    let mut config = Config {
        default_configs,
        contexts,
        overrides,
        dimensions,
    };

    if let Some(prefixes) = filter_prefixes {
        if !prefixes.is_empty() {
            config =
                config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
        }
    }

    let reasoning_collector = |context: Context| {
        reasoning.push(json!({
            "context": context.condition,
            "override": context.override_with_keys
        }));
    };

    let modified_query_data = evaluate_local_cohorts(config.dimensions, query_data);

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
        &merge_strategy,
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

fn get_overrides<F: FnMut(Context)>(
    query_data: &Map<String, Value>,
    contexts: Vec<Context>,
    mut overrides: HashMap<String, Overrides>,
    merge_strategy: &MergeStrategy,
    mut on_override_select: F,
) -> Result<Map<String, Value>, String> {
    let mut required_overrides: Value = json!({});

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
        }
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
            log::error!("Config: found non-default_config key: {key} in overrides");
        }
    });
    default_config
}
