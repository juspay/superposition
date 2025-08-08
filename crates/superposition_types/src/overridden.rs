use std::collections::HashSet;

use serde_json::{Map, Value};

use crate::config::Overrides;

pub(crate) fn filter_config_keys_by_prefix(
    overrides: &Map<String, Value>,
    prefix_list: &HashSet<String>,
) -> Map<String, Value> {
    overrides
        .iter()
        .filter(|(key, _)| {
            prefix_list
                .iter()
                .any(|prefix_str| key.starts_with(prefix_str))
        })
        .map(|(key, value)| (key.clone(), value.clone()))
        .collect()
}

pub trait Overridden<T: TryFrom<Map<String, Value>>>: Clone {
    fn get_overrides(&self) -> Overrides;

    fn filter_keys_by_prefix(
        context: &Self,
        prefix_list: &HashSet<String>,
    ) -> Result<T, <T as TryFrom<Map<String, Value>>>::Error> {
        let filtered_override =
            filter_config_keys_by_prefix(&context.get_overrides(), prefix_list);

        T::try_from(filtered_override)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use serde_json::Map;

    use crate::config::tests::{
        get_config, get_prefix_filtered_config1, get_prefix_filtered_config2,
    };

    use super::filter_config_keys_by_prefix;

    #[test]
    fn test_filter_config_keys_by_prefix() {
        let config = get_config();

        let prefix_list = HashSet::from_iter(vec![String::from("test.")]);

        assert_eq!(
            filter_config_keys_by_prefix(&config.default_configs, &prefix_list),
            get_prefix_filtered_config1().default_configs
        );

        let prefix_list =
            HashSet::from_iter(vec![String::from("test."), String::from("test2.")]);

        assert_eq!(
            filter_config_keys_by_prefix(&config.default_configs, &prefix_list),
            get_prefix_filtered_config2().default_configs
        );

        let prefix_list = HashSet::from_iter(vec![String::from("abcd")]);

        assert_eq!(
            filter_config_keys_by_prefix(&config.default_configs, &prefix_list),
            Map::new()
        );
    }
}
