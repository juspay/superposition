use std::collections::HashSet;

use derive_more::Deref;
use serde_json::{Map, Value};

use crate::config::Overrides;

#[derive(Deref, Clone, Default, Debug, PartialEq)]
pub struct PrefixList(HashSet<String>);

impl PrefixList {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn matches_any(&self, key: &str) -> bool {
        self.iter().any(|prefix_str| key.starts_with(prefix_str))
    }
}

impl FromIterator<String> for PrefixList {
    fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
        let prefix_set = iter
            .into_iter()
            .map(|prefix| prefix.trim().to_owned())
            .filter(|prefix| !prefix.is_empty())
            .collect::<HashSet<_>>();
        Self(prefix_set)
    }
}

impl<T: IntoIterator<Item = String>> From<Option<T>> for PrefixList {
    fn from(prefixes: Option<T>) -> Self {
        match prefixes {
            Some(prefix) => Self::from_iter(prefix),
            None => Self::new(),
        }
    }
}

/// Returns `true` when `key` should be retained given an allow-list and an
/// exclude-list of prefixes. An empty allow-list means "allow everything", so
/// that exclude-only filtering keeps every non-excluded key. An empty
/// exclude-list excludes nothing.
///
/// Blank entries are already stripped by `PrefixList`'s constructor, so the
/// predicate can treat the sets as sanitised.
fn key_is_retained(
    key: &str,
    prefix_list: &PrefixList,
    exclude_prefix_list: &PrefixList,
) -> bool {
    (prefix_list.is_empty() || prefix_list.matches_any(key))
        && !exclude_prefix_list.matches_any(key)
}

pub(crate) fn filter_config_keys_by_prefix(
    overrides: &Map<String, Value>,
    prefix_list: &PrefixList,
    exclude_prefix_list: &PrefixList,
) -> Map<String, Value> {
    overrides
        .iter()
        .filter(|(key, _)| key_is_retained(key, prefix_list, exclude_prefix_list))
        .map(|(key, value)| (key.clone(), value.clone()))
        .collect()
}

pub(crate) fn filter_into_config_keys_by_prefix(
    overrides: Map<String, Value>,
    prefix_list: &PrefixList,
    exclude_prefix_list: &PrefixList,
) -> Map<String, Value> {
    overrides
        .into_iter()
        .filter(|(key, _)| key_is_retained(key, prefix_list, exclude_prefix_list))
        .collect()
}

pub trait Overridden<T: TryFrom<Map<String, Value>>>: Clone {
    fn get_overrides(&self) -> Overrides;

    fn filter_keys_by_prefix(
        context: &Self,
        prefix_list: &PrefixList,
        exclude_prefix_list: &PrefixList,
    ) -> Result<T, <T as TryFrom<Map<String, Value>>>::Error> {
        let filtered_override = filter_config_keys_by_prefix(
            &context.get_overrides(),
            prefix_list,
            exclude_prefix_list,
        );

        T::try_from(filtered_override)
    }
}

#[cfg(test)]
mod tests {
    use serde_json::{json, Map};

    use crate::{
        config::tests::map::with_dimensions::{
            get_config, get_prefix_filtered_config1, get_prefix_filtered_config2,
        },
        overridden::PrefixList,
    };

    use super::filter_config_keys_by_prefix;

    #[test]
    fn test_filter_config_keys_by_prefix() {
        let config = get_config();

        let prefix_list = PrefixList::from_iter(vec![String::from("test.")]);

        assert_eq!(
            filter_config_keys_by_prefix(
                &config.default_configs,
                &prefix_list,
                &PrefixList::new()
            ),
            get_prefix_filtered_config1().default_configs.into_inner()
        );

        let prefix_list =
            PrefixList::from_iter(vec![String::from("test."), String::from("test2.")]);

        assert_eq!(
            filter_config_keys_by_prefix(
                &config.default_configs,
                &prefix_list,
                &PrefixList::new()
            ),
            get_prefix_filtered_config2().default_configs.into_inner()
        );

        let prefix_list = PrefixList::from_iter(vec![String::from("abcd")]);

        assert_eq!(
            filter_config_keys_by_prefix(
                &config.default_configs,
                &prefix_list,
                &PrefixList::new()
            ),
            Map::new()
        );
    }

    #[test]
    fn test_filter_config_keys_exclude_only_keeps_non_excluded() {
        // An empty allow-list must mean "allow everything", so an exclude-only
        // filter keeps every key that does not match an excluded prefix.
        let config = get_config();

        let exclude_list =
            PrefixList::from_iter(vec![String::from("test."), String::from("test2.")]);

        assert_eq!(
            filter_config_keys_by_prefix(
                &config.default_configs,
                &PrefixList::new(),
                &exclude_list
            ),
            json!({ "key1": false }).as_object().unwrap().clone()
        );
    }

    #[test]
    fn test_filter_config_keys_empty_lists_is_noop() {
        // Empty allow-list + empty exclude-list keeps everything untouched.
        let config = get_config();

        assert_eq!(
            filter_config_keys_by_prefix(
                &config.default_configs,
                &PrefixList::new(),
                &PrefixList::new()
            ),
            config.default_configs.clone().into_inner()
        );
    }

    #[test]
    fn test_filter_config_keys_exclude_takes_precedence_over_allow() {
        // A key matched by both the allow-list and the exclude-list is dropped:
        // exclusion wins. `test.test.` is a sub-prefix of the allowed `test.`.
        let config = get_config();

        let prefix_list = PrefixList::from_iter(vec![String::from("test.")]);
        let exclude_list = PrefixList::from_iter(vec![String::from("test.test.")]);

        assert_eq!(
            filter_config_keys_by_prefix(
                &config.default_configs,
                &prefix_list,
                &exclude_list
            ),
            json!({ "test.test1": 12 }).as_object().unwrap().clone()
        );
    }
}
