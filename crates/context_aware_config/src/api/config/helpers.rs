use superposition_types::{
    custom_query::CommaSeparatedStringQParams, result as superposition, Config,
};

pub fn apply_prefix_filter_to_config(
    prefix: &Option<CommaSeparatedStringQParams>,
    mut config: Config,
) -> superposition::Result<Config> {
    if let Some(prefix) = prefix {
        config = config.filter_by_prefix(&prefix.iter().map(Clone::clone).collect());
    }

    Ok(config)
}
