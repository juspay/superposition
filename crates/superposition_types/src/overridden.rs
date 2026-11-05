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

/// Matches keys that contain any of the substrings anywhere, so `.external.`
/// selects `payments.external.requires_cvv`.
#[derive(Deref, Clone, Default, Debug, PartialEq)]
pub struct SubstringList(HashSet<String>);

impl SubstringList {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn matches_any(&self, key: &str) -> bool {
        self.iter().any(|substring| key.contains(substring))
    }
}

macro_rules! impl_key_list_from {
    ($list:ident) => {
        impl FromIterator<String> for $list {
            fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
                let set = iter
                    .into_iter()
                    .map(|entry| entry.trim().to_owned())
                    .filter(|entry| !entry.is_empty())
                    .collect::<HashSet<_>>();
                Self(set)
            }
        }

        impl<T: IntoIterator<Item = String>> From<Option<T>> for $list {
            fn from(entries: Option<T>) -> Self {
                match entries {
                    Some(entries) => Self::from_iter(entries),
                    None => Self::new(),
                }
            }
        }
    };
}

impl_key_list_from!(PrefixList);
impl_key_list_from!(SubstringList);

/// Returns `true` when `key` should be retained given an allow-list and an
/// exclude-list of prefixes. An empty allow-list means "allow everything", so
/// that exclude-only filtering keeps every non-excluded key. An empty
/// exclude-list excludes nothing.
///
/// Blank entries are already stripped by `PrefixList`'s constructor, so the
/// predicate can treat the sets as sanitised.
pub(crate) fn key_is_retained(
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

pub(crate) fn filter_config_keys_by_prefix_mut(
    overrides: &mut Map<String, Value>,
    prefix_list: &PrefixList,
    exclude_prefix_list: &PrefixList,
) {
    overrides.retain(|key, _| key_is_retained(key, prefix_list, exclude_prefix_list));
}

pub trait Overridden: Sized {
    fn get_overrides_mut(&mut self) -> &mut Overrides;

    fn filter_keys_by_prefix(
        context: &mut Self,
        prefix_list: &PrefixList,
        exclude_prefix_list: &PrefixList,
    ) -> Result<(), String> {
        filter_config_keys_by_prefix_mut(
            context.get_overrides_mut(),
            prefix_list,
            exclude_prefix_list,
        );

        if context.get_overrides_mut().is_empty() {
            Err("No overrides left after filtering".to_string())
        } else {
            Ok(())
        }
    }

    fn filter_keys_by_substring(
        context: &mut Self,
        substring_list: &SubstringList,
    ) -> Result<(), String> {
        context
            .get_overrides_mut()
            .retain(|key, _| substring_list.matches_any(key));

        if context.get_overrides_mut().is_empty() {
            Err("No overrides left after filtering".to_string())
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use serde_json::{json, Map};

    use crate::{
        config::tests::map::with_dimensions::{
            get_config, get_prefix_filtered_config1, get_prefix_filtered_config2,
        },
        overridden::{Overridden, PrefixList, SubstringList},
        Cac, Overrides,
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

    #[test]
    fn test_substring_list_matches_inside_key() {
        let substring_list =
            SubstringList::from_iter(vec![String::from(".external."), String::from(" ")]);
        assert_eq!(substring_list.len(), 1);
        assert!(substring_list.matches_any("payments.external.requires_cvv"));
        assert!(!substring_list.matches_any("external.payments.requires_cvv"));
        assert!(!substring_list.matches_any("payments.poll_config_external_three_ds"));

        let prefix_list = PrefixList::from_iter(vec![String::from(".external.")]);
        assert!(!prefix_list.matches_any("payments.external.requires_cvv"));
    }

    #[test]
    fn test_filter_keys_by_substring_strips_and_drops_contexts() {
        struct TestContext(Overrides);
        impl Overridden for TestContext {
            fn get_overrides_mut(&mut self) -> &mut Overrides {
                &mut self.0
            }
        }
        let overrides = |value: serde_json::Value| {
            Cac::<Overrides>::try_from(value.as_object().unwrap().clone())
                .unwrap()
                .into_inner()
        };
        let substring_list = SubstringList::from_iter(vec![String::from(".external.")]);

        let mut mixed = TestContext(overrides(json!({
            "payments.external.requires_cvv": false,
            "payments.requires_cvv": true,
        })));
        assert!(TestContext::filter_keys_by_substring(&mut mixed, &substring_list).is_ok());
        assert!(mixed.0.contains_key("payments.external.requires_cvv"));
        assert!(!mixed.0.contains_key("payments.requires_cvv"));

        let mut internal_only =
            TestContext(overrides(json!({ "payments.requires_cvv": true })));
        assert!(
            TestContext::filter_keys_by_substring(&mut internal_only, &substring_list)
                .is_err()
        );
    }
}
