use std::collections::HashSet;

use super::types::{Config, Context};

use serde_json::{Map, Value};
use service_utils::{
    helpers::extract_dimensions, result as superposition, unexpected_error,
};

pub fn filter_context(
    contexts: &[Context],
    query_params_map: &Map<String, Value>,
) -> superposition::Result<Vec<Context>> {
    let mut filtered_context: Vec<Context> = Vec::new();
    for context in contexts.iter() {
        if should_add_ctx(context, query_params_map)? {
            filtered_context.push(context.clone());
        }
    }
    Ok(filtered_context)
}

fn should_add_ctx(
    context: &Context,
    query_params_map: &Map<String, Value>,
) -> superposition::Result<bool> {
    let dimension = extract_dimensions(&context.condition)?;
    Ok(dimension.iter().all(|(key, value)| {
        query_params_map.get(key).map_or(true, |val| {
            val == value || val.as_array().unwrap_or(&vec![]).contains(value)
        })
    }))
}

pub fn filter_config_by_prefix(
    config: &Config,
    prefix_list: &HashSet<&str>,
) -> superposition::Result<Config> {
    let mut filtered_overrides: Map<String, Value> = Map::new();

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
        let overrides_map = overrides
            .as_object()
            .ok_or_else(|| {
                log::error!("failed to decode overrides.");
                unexpected_error!("failed to decode overrides.")
            })?
            .clone();

        let filtered_overrides_map: Map<String, Value> = overrides_map
            .into_iter()
            .filter(|(key, _)| filtered_default_config.contains_key(key))
            .collect();

        if !filtered_overrides_map.is_empty() {
            filtered_overrides.insert(key.clone(), Value::Object(filtered_overrides_map));
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
    query_params_map: &Map<String, Value>,
) -> superposition::Result<Config> {
    let filtered_context = filter_context(&config.contexts, query_params_map)?;
    let filtered_overrides: Map<String, Value> = filtered_context
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
