use std::collections::HashMap;

use super::types::{Config, Context};
use serde_json::{json, Map, Value};
use superposition_types::{result as superposition, Cac, Overrides};

pub fn filter_context(
    contexts: &[Context],
    dimension_data: &Map<String, Value>,
) -> Vec<Context> {
    contexts
        .iter()
        .filter_map(|context| {
            match jsonlogic::partial_apply(
                &json!(context.condition),
                &json!(dimension_data),
            ) {
                Ok(jsonlogic::PartialApplyOutcome::Resolved(Value::Bool(true)))
                | Ok(jsonlogic::PartialApplyOutcome::Ambiguous) => Some(context.clone()),
                _ => None,
            }
        })
        .collect()
}

pub fn filter_config_by_prefix(
    config: &Config,
    prefix_list: &Vec<String>,
) -> superposition::Result<Config> {
    let mut filtered_overrides: HashMap<String, Overrides> = HashMap::new();

    let filtered_default_config: Map<String, Value> = config
        .default_configs
        .clone()
        .into_iter()
        .filter(|(key, _)| {
            prefix_list
                .iter()
                .any(|prefix_str| key.starts_with(prefix_str))
        })
        .collect();

    for (key, overrides) in &config.overrides {
        let override_map: Map<String, Value> = overrides.to_owned().into();
        let filtered_overrides_map: Map<String, Value> = override_map
            .into_iter()
            .filter(|(key, _)| filtered_default_config.contains_key(key))
            .collect();

        if !filtered_overrides_map.is_empty() {
            filtered_overrides.insert(
                key.clone(),
                Cac::<Overrides>::try_from_db(filtered_overrides_map)
                    .map_err(superposition::AppError::BadArgument)?
                    .into_inner(),
            );
        }
    }

    let filtered_context: Vec<Context> = config
        .contexts
        .clone()
        .into_iter()
        .filter(|context| filtered_overrides.contains_key(&context.override_with_keys[0]))
        .collect();

    let filtered_config = Config {
        contexts: filtered_context,
        overrides: filtered_overrides,
        default_configs: filtered_default_config,
    };

    Ok(filtered_config)
}

pub fn filter_config_by_dimensions(
    config: &Config,
    dimension_data: &Map<String, Value>,
) -> superposition::Result<Config> {
    let filtered_context = filter_context(&config.contexts, dimension_data);
    let filtered_overrides: HashMap<String, Overrides> = filtered_context
        .iter()
        .flat_map(|ele| {
            let override_with_key = &ele.override_with_keys[0];
            config
                .overrides
                .get(override_with_key)
                .map(|value| (override_with_key.to_string(), value.clone()))
        })
        .collect();

    let filtered_config = Config {
        contexts: filtered_context,
        overrides: filtered_overrides,
        default_configs: config.default_configs.clone(),
    };

    Ok(filtered_config)
}
