use serde_json::{Map, Value};
use superposition_macros::unexpected_error;
use superposition_types::{result as superposition, Config};

pub fn apply_prefix_filter_to_config(
    query_params_map: &mut Map<String, Value>,
    mut config: Config,
) -> superposition::Result<Config> {
    if let Some(prefix) = query_params_map
        .get("prefix")
        .and_then(|prefix| prefix.as_str())
    {
        config = config
            .try_filter_by_prefix(&prefix.split(',').map(String::from).collect())
            .map_err(|err| {
                log::error!("config.try_filter_by_prefix error : {err}");
                unexpected_error!(err)
            })?;
    }

    query_params_map.remove("prefix");
    Ok(config)
}
