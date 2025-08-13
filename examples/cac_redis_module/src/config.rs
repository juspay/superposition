use serde_json::{json, Map, Value};
use std::collections::{HashMap, HashSet};

// Define the structures locally instead of using superposition_types
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub struct Config {
    pub default_configs: Map<String, Value>,
    pub contexts: Vec<Context>,
    pub overrides: HashMap<String, Overrides>,
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub struct Context {
    pub id: String,
    pub condition: Map<String, Value>,
    pub priority: u32,
    pub weight: u32,
    pub override_with_keys: Vec<String>,
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub struct Overrides(pub Map<String, Value>);

impl From<Map<String, Value>> for Overrides {
    fn from(map: Map<String, Value>) -> Self {
        Overrides(map)
    }
}

impl From<Overrides> for Map<String, Value> {
    fn from(overrides: Overrides) -> Self {
        overrides.0
    }
}

impl Config {
    pub fn filter_by_prefix(&self, prefixes: &HashSet<String>) -> Config {
        let mut filtered_default_configs = Map::new();
        let mut filtered_overrides = HashMap::new();

        // Filter default configs by prefix
        for (key, value) in &self.default_configs {
            if prefixes.iter().any(|prefix| key.starts_with(prefix)) {
                filtered_default_configs.insert(key.clone(), value.clone());
            }
        }

        // Filter overrides by prefix
        for (override_key, override_value) in &self.overrides {
            let mut filtered_override = Map::new();
            for (key, value) in &override_value.0 {
                if prefixes.iter().any(|prefix| key.starts_with(prefix)) {
                    filtered_override.insert(key.clone(), value.clone());
                }
            }
            if !filtered_override.is_empty() {
                filtered_overrides.insert(override_key.clone(), Overrides(filtered_override));
            }
        }

        Config {
            default_configs: filtered_default_configs,
            contexts: self.contexts.clone(),
            overrides: filtered_overrides,
        }
    }
}

// Merge strategy for configuration resolution
#[derive(Clone, Debug, PartialEq, strum_macros::Display, Default)]
#[strum(serialize_all = "snake_case")]
pub enum MergeStrategy {
    #[default]
    MERGE,
    REPLACE,
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

pub fn eval_config(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
) -> Result<Map<String, Value>, String> {
    // Create Config struct to use existing filtering logic
    let mut config = Config {
        default_configs: default_config,
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
    };

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if let Some(prefixes) = filter_prefixes {
        if !prefixes.is_empty() {
            config =
                config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
        }
    }
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

pub fn eval_config_with_reasoning(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>, // Optional prefix filtering
) -> Result<Map<String, Value>, String> {
    let mut reasoning: Vec<Value> = vec![];

    let mut config = Config {
        default_configs: default_config,
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
    };

    if let Some(prefixes) = filter_prefixes {
        if !prefixes.is_empty() {
            config =
                config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
        }
    }

    let mut reasoning_collector = |context: Context| {
        reasoning.push(json!({
            "context": context.condition,
            "override": context.override_with_keys
        }));
    };

    let overrides_map = get_overrides(
        query_data,
        &config.contexts,
        &config.overrides,
        &merge_strategy,
        Some(&mut reasoning_collector),
    )?;

    let mut result_config = config.default_configs;
    merge_overrides_on_default_config(&mut result_config, overrides_map, &merge_strategy);

    // Add reasoning metadata
    result_config.insert("metadata".into(), json!(reasoning));

    Ok(result_config)
}

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
            func(context)
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

    match required_overrides {
        Value::Object(map) => Ok(map),
        _ => Err("Failed to create overrides map".to_string()),
    }
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
            log::error!("Config: found non-default_config key: {key} in overrides");
        }
    })
}
