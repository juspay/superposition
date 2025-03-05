use serde_json::{Map, Value};
use superposition_types::{result as superposition, Config};

pub fn apply_prefix_filter_to_config(
    query_params_map: &mut Map<String, Value>,
    mut config: Config,
) -> superposition::Result<Config> {
    if let Some(prefix) = query_params_map
        .remove("prefix")
        .and_then(|prefix| prefix.as_str().map(String::from))
    {
        config = config.filter_by_prefix(&prefix.split(',').map(String::from).collect());
    }

    Ok(config)
}
