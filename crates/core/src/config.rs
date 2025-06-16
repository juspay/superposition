use serde_json::{json, Map, Value};
use std::collections::{HashMap, HashSet};
use superposition_types::{Config, Context, Overrides};

// Merge strategy for configuration resolution
#[derive(Clone, Debug, PartialEq, strum_macros::Display, uniffi::Enum)]
pub enum MergeStrategy {
    #[strum(to_string = "merge")]
    MERGE,
    #[strum(to_string = "replace")]
    REPLACE,
}

impl Default for MergeStrategy {
    fn default() -> Self {
        Self::MERGE
    }
}

impl From<&str> for MergeStrategy {
    fn from(value: &str) -> Self {
        match value.to_lowercase().as_str() {
            "replace" => Self::REPLACE,
            "merge" => Self::MERGE,
            _ => Self::default(),
        }
    }
}

#[uniffi::export]
pub fn eval_config(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<&[String]>,
) -> Result<Map<String, Value>, String> {
    // Create Config struct to use existing filtering logic
    let mut config = Config {
        default_configs: default_config,
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
    };

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if let Some(prefixes) = filter_prefixes {
        config = config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
    }

    // Get applicable overrides
    let overrides_map: Map<String, Value> = get_overrides(
        query_data,
        &config.contexts,
        &config.overrides,
        &merge_strategy,
        None,
    )?;

    // Apply overrides to default config
    let mut result_config = config.default_configs;
    merge_overrides_on_default_config(&mut result_config, overrides_map, &merge_strategy);

    Ok(result_config)
}

/// Evaluate config with reasoning metadata and optional prefix filtering
pub fn eval_config_with_reasoning(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<&[String]>, // Optional prefix filtering
) -> Result<Map<String, Value>, String> {
    let mut reasoning: Vec<Value> = vec![];

    // Create Config struct to use existing filtering logic
    let mut config = Config {
        default_configs: default_config,
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
    };

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if let Some(prefixes) = filter_prefixes {
        config = config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
    }

    // Capture reasoning during override selection
    let mut reasoning_collector = |context: Context| {
        reasoning.push(json!({
            "context": context.condition,
            "override": context.override_with_keys
        }));
    };

    // Get applicable overrides with reasoning
    let overrides_map = get_overrides(
        query_data,
        &config.contexts,
        &config.overrides,
        &merge_strategy,
        Some(&mut reasoning_collector),
    )?;

    // Apply overrides to default config
    let mut result_config = config.default_configs;
    merge_overrides_on_default_config(&mut result_config, overrides_map, &merge_strategy);

    // Add reasoning metadata
    result_config.insert("metadata".into(), json!(reasoning));

    Ok(result_config)
}

/// Merges two JSON values recursively
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

/// Replaces top-level fields in a JSON object
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
            log::error!(
                "Config: found non-object override key: {override_key} in overrides"
            );
        }
    }
}

/// Gets applicable overrides based on contexts and query data
fn get_overrides(
    query_data: &Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    merge_strategy: &MergeStrategy,
    mut on_override_select: Option<&mut dyn FnMut(Context)>,
) -> Result<Map<String, Value>, String> {
    let mut required_overrides: Value = json!({});
    let mut on_override_select = |context: Context| {
        if let Some(ref mut func) = on_override_select {
            func(context.clone())
        }
    };

    let query_data_value = Value::Object(query_data.clone());

    for context in contexts {
        if let Ok(Value::Bool(true)) = jsonlogic::apply(
            &Value::Object(context.condition.clone().into()),
            &query_data_value,
        ) {
            for override_key in &context.override_with_keys {
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
    }

    // Convert to map
    match required_overrides {
        Value::Object(map) => Ok(map),
        _ => Err("Failed to create overrides map".to_string()),
    }
}

/// Applies overrides to default configuration
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
            log::error!("Config: found non-default_config key: {key} in overrides");
        }
    })
}
