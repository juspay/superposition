use std::collections::HashMap;

use serde_json::{Map, Value};
use superposition_macros::unexpected_error;
use superposition_types::{result as superposition, Cac, Contextual, Overrides};

use super::types::{Config, Context};

pub fn apply_prefix_filter_to_config(
    query_params_map: &mut Map<String, Value>,
    mut config: Config,
) -> superposition::Result<Config> {
    if let Some(prefix) = query_params_map
        .get("prefix")
        .and_then(|prefix| prefix.as_str())
    {
        config = filter_config_by_prefix(
            &config,
            &prefix.split(',').map(String::from).collect(),
        )?
    }

    query_params_map.remove("prefix");
    Ok(config)
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
        let filtered_overrides_map: Map<String, Value> = overrides
            .clone()
            .into_iter()
            .filter(|(key, _)| prefix_list.contains(key))
            .collect();

        let filtered_override_map =Cac::<Overrides>::try_from_db(filtered_overrides_map)
        .map_err(|err| {
            log::error!("filter_config_by_prefix : failed to decode overrides from db with error {}", err);
            unexpected_error!(err)
        })?;
        filtered_overrides.insert(key.clone(), filtered_override_map.into_inner());
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
    let filtered_context =
        Context::filter_by_eval(config.contexts.clone(), dimension_data);
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
