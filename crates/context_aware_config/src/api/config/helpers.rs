use super::types::{Config, Context};
use actix_web::web::Query;
use serde_json::{json, Map, Value};
use std::collections::HashMap;
use superposition_macros::{bad_argument, unexpected_error};
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
        let override_map: Map<String, Value> = overrides.to_owned().into();
        let filtered_overrides_map: Map<String, Value> = override_map
            .into_iter()
            .filter(|(key, _)| filtered_default_config.contains_key(key))
            .collect();

        if !filtered_overrides_map.is_empty() {
            filtered_overrides.insert(
                key.clone(),
                Cac::<Overrides>::try_from_db(filtered_overrides_map)
                    .map_err(|err| {
                        log::error!("filter_config_by_prefix : failed to decode overrides from db with error {}", err);
                        unexpected_error!(err)
                    })?
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

pub fn get_query_params_map(
    query_string: &str,
) -> superposition::Result<Map<String, Value>> {
    let params =
        Query::<HashMap<String, String>>::from_query(query_string).map_err(|err| {
            log::error!("failed to parse query params with err: {}", err);
            bad_argument!("error getting query params")
        })?;

    let query_params_map = params
        .0
        .into_iter()
        .map(|(key, value)| {
            let parsed_value = value
                .parse::<serde_json::Value>()
                .unwrap_or_else(|_| json!(value));
            (key, parsed_value)
        })
        .collect::<Map<String, Value>>();

    Ok(query_params_map)
}
