use std::collections::{HashMap, HashSet};

use serde_json::{json, Map, Value};
use superposition_types::{
    database::models::cac::DimensionType, Config, Context, DimensionInfo, Overrides,
};

#[derive(Clone, Debug, PartialEq, strum_macros::Display, Default, uniffi::Enum)]
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

fn evaluate_cohort(
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
) -> Result<Map<String, Value>, String> {
    let mut cohort_context = Map::new();

    for (dimension_key, dimension_value) in query_data {
        let dimension_info = match dimensions.get(dimension_key) {
            Some(info) => info,
            None => {
                log::error!("CAC: dimension {dimension_key} not found in dimensions");
                return Err(format!(
                    "CAC: dimension {dimension_key} not found in dimensions"
                ));
            }
        };

        if dimension_info.dimension_type != DimensionType::Regular {
            continue;
        }

        for dependency in &dimension_info.dependencies {
            let dependency_info = match dimensions.get(dependency) {
                Some(info) => info,
                None => {
                    log::error!(
                        "CAC: dependency dimension {dependency} not found in dimensions"
                    );
                    return Err(format!(
                        "CAC: dependency dimension {dependency} not found in dimensions"
                    ));
                }
            };

            if dependency_info.dimension_type != DimensionType::LocalCohort {
                continue;
            }

            let schema_object = match &dependency_info.schema {
                Value::Object(logic) => logic,
                _ => {
                    log::error!("CAC: failed to parse schema for dimension {dependency}");
                    return Err(format!(
                        "CAC: failed to parse schema for dimension {dependency}"
                    ));
                }
            };

            for (cohort_name, expression) in schema_object.iter() {
                let evaluation_data = json!({dimension_key: dimension_value});

                match jsonlogic::apply(expression, &evaluation_data) {
                    Ok(Value::Bool(true)) => {
                        cohort_context.insert(
                            dependency.clone(),
                            Value::String(cohort_name.clone()),
                        );
                        break; // stop at the first match
                    }
                    _ => {}
                }
            }
        }
    }

    let modified_query_data = query_data
        .clone()
        .into_iter()
        .chain(cohort_context.clone().into_iter())
        .collect::<Map<String, Value>>();

    Ok(modified_query_data)
}

pub fn eval_config(
    default_config: Map<String, Value>,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
) -> Result<Map<String, Value>, String> {
    // Create Config struct to use existing filtering logic
    let mut config = Config {
        default_configs: default_config,
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
        dimensions: dimensions.clone(),
    };

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if let Some(prefixes) = filter_prefixes {
        if !prefixes.is_empty() {
            config =
                config.filter_by_prefix(&HashSet::from_iter(prefixes.iter().cloned()));
        }
    }

    let modified_query_data = evaluate_cohort(&config.dimensions, query_data)?;

    let overrides_map: Map<String, Value> = get_overrides(
        &modified_query_data,
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
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: &Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>, // Optional prefix filtering
) -> Result<Map<String, Value>, String> {
    let mut reasoning: Vec<Value> = vec![];

    let mut config = Config {
        default_configs: default_config,
        contexts: contexts.to_vec(),
        overrides: overrides.clone(),
        dimensions: dimensions.clone(),
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

    let modified_query_data = evaluate_cohort(&config.dimensions, query_data)?;

    let overrides_map = get_overrides(
        &modified_query_data,
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

    #[cfg(feature = "jsonlogic")]
    let query_data_value = Value::Object(query_data.clone());

    for context in contexts {
        cfg_if::cfg_if! {
            if #[cfg(feature = "jsonlogic")] {
                let valid_context = Ok(Value::Bool(true))
                    == jsonlogic::apply(
                        &Value::Object(context.condition.clone().into()),
                        &query_data_value,
                    );
            } else {
                let valid_context = superposition_types::apply(&context.condition, query_data);
            }
        }

        if valid_context {
            let override_key = context.override_with_keys.get_key();
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
