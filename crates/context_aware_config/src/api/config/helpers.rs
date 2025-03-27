use serde_json::{Map, Value};
use superposition_types::{result as superposition, Config};

use super::XConfigPrefix;

pub fn apply_prefix_filter_to_config(
    query_params_map: &mut Map<String, Value>,
    config_prefix: Option<XConfigPrefix>,
    mut config: Config,
) -> superposition::Result<Config> {
    if let Some(prefix) = query_params_map
        .remove("prefix")
        .and_then(|prefix| prefix.as_str().map(String::from))
        .or_else(|| config_prefix.map(|p| p.0))
    {
        config = config.filter_by_prefix(&prefix.split(',').map(String::from).collect());
    }

    Ok(config)
}
