use superposition_types::{result as superposition, Config};

pub fn apply_prefix_filter_to_config(
    prefix: &Option<Vec<String>>,
    mut config: Config,
) -> superposition::Result<Config> {
    if let Some(prefix) = prefix {
        config = config.filter_by_prefix(&prefix.iter().map(Clone::clone).collect());
    }

    Ok(config)
}
