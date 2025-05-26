use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use std::collections::HashMap;

// Merge strategy for configuration resolution
#[derive(Clone, Debug, PartialEq)]
pub enum MergeStrategy {
    MERGE,
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

impl ToString for MergeStrategy {
    fn to_string(&self) -> String {
        match self {
            MergeStrategy::MERGE => "merge".to_string(),
            MergeStrategy::REPLACE => "replace".to_string(),
        }
    }
}

// Context for configuration resolution
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Context {
    pub condition: Map<String, Value>,
    pub override_with_keys: Vec<String>,
}

// Configuration data structure
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConfigData {
    pub default_configs: Map<String, Value>,
    pub contexts: Vec<Context>,
    pub overrides: HashMap<String, Map<String, Value>>,
}

/// Main configuration evaluation function
pub fn eval_config(
    mut default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Map<String, Value>>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    // Get applicable overrides
    let overrides_map: Map<String, Value> =
        get_overrides(query_data, contexts, overrides, &merge_strategy, None)?;

    // Apply overrides to default config
    merge_overrides_on_default_config(
        &mut default_config,
        overrides_map,
        &merge_strategy,
    );

    Ok(default_config)
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
    overrides: &HashMap<String, Map<String, Value>>,
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
        if let Ok(Value::Bool(true)) =
            jsonlogic::apply(&Value::Object(context.condition.clone()), &query_data_value)
        {
            for override_key in &context.override_with_keys {
                if let Some(overriden_value) = overrides.get(override_key) {
                    match merge_strategy {
                        MergeStrategy::REPLACE => replace_top_level(
                            required_overrides.as_object_mut().unwrap(),
                            &Value::Object(overriden_value.clone()),
                            || on_override_select(context.clone()),
                            override_key,
                        ),
                        MergeStrategy::MERGE => {
                            merge(
                                &mut required_overrides,
                                &Value::Object(overriden_value.clone()),
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

/// Evaluate config with reasoning metadata
pub fn eval_config_with_reasoning(
    mut default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Map<String, Value>>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let mut reasoning: Vec<Value> = vec![];

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
        contexts,
        overrides,
        &merge_strategy,
        Some(&mut reasoning_collector),
    )?;

    // Apply overrides to default config
    merge_overrides_on_default_config(
        &mut default_config,
        overrides_map,
        &merge_strategy,
    );

    // Add reasoning metadata
    let mut result_config = default_config;
    result_config.insert("metadata".into(), json!(reasoning));

    Ok(result_config)
}

// Utility functions for filtering configuration

// Filter map by keys
pub fn filter_map_by_keys(
    data: &Map<String, Value>,
    keys: &[String],
) -> Map<String, Value> {
    let mut result = Map::new();

    for key in keys {
        for (k, v) in data.iter() {
            if k.starts_with(key) {
                result.insert(k.clone(), v.clone());
            }
        }
    }

    result
}

// Filter configuration by keys
pub fn filter_config_by_keys(config: &ConfigData, keys: &[String]) -> ConfigData {
    ConfigData {
        default_configs: filter_map_by_keys(&config.default_configs, keys),
        contexts: config.contexts.clone(),
        overrides: filter_overrides_by_keys(&config.overrides, keys),
    }
}

// Filter overrides by keys
pub fn filter_overrides_by_keys(
    overrides: &HashMap<String, Map<String, Value>>,
    keys: &[String],
) -> HashMap<String, Map<String, Value>> {
    let mut result = HashMap::new();

    for (override_key, override_map) in overrides {
        let filtered_map = filter_map_by_keys(override_map, keys);
        if !filtered_map.is_empty() {
            result.insert(override_key.clone(), filtered_map);
        }
    }

    result
}
