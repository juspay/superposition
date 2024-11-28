use std::collections::HashSet;

use serde_json::{Map, Value};

use crate::config::Overrides;

pub(crate) fn filter_config_keys_by_prefix(
    overrides: Map<String, Value>,
    prefix_list: &HashSet<String>,
) -> Map<String, Value> {
    overrides
        .into_iter()
        .filter(|(key, _)| {
            prefix_list
                .iter()
                .any(|prefix_str| key.starts_with(prefix_str))
        })
        .collect()
}

pub trait Overridden<T: TryFrom<Map<String, Value>>>: Clone {
    fn get_overrides(&self) -> Overrides;

    fn filter_keys_by_prefix(
        context: &Self,
        prefix_list: &HashSet<String>,
    ) -> Result<T, <T as TryFrom<Map<String, Value>>>::Error> {
        let filtered_override =
            filter_config_keys_by_prefix(context.get_overrides().into(), prefix_list);

        T::try_from(filtered_override)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use serde_json::{json, Map};

    use super::filter_config_keys_by_prefix;

    #[test]
    fn test_filter_config_keys_by_prefix() {
        let config = json!({
            "key1": false,
            "test.test.test1": 1,
            "test.test1": 12,
            "test2.key": false,
            "test2.test": "def_val"
        })
        .as_object()
        .unwrap()
        .clone();

        let prefix_list = HashSet::from_iter(vec![String::from("test.")].into_iter());

        assert_eq!(
            filter_config_keys_by_prefix(config.clone(), &prefix_list),
            json!({
                "test.test.test1": 1,
                "test.test1": 12
            })
            .as_object()
            .unwrap()
            .clone()
        );

        let prefix_list = HashSet::from_iter(
            vec![String::from("test."), String::from("test2.")].into_iter(),
        );

        assert_eq!(
            filter_config_keys_by_prefix(config.clone(), &prefix_list),
            json!({
                "test.test.test1": 1,
                "test.test1": 12,
                "test2.key": false,
                "test2.test": "def_val"
            })
            .as_object()
            .unwrap()
            .clone()
        );

        let prefix_list = HashSet::from_iter(vec![String::from("abcd")].into_iter());

        assert_eq!(
            filter_config_keys_by_prefix(config, &prefix_list),
            Map::new()
        );
    }
}
