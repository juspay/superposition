use std::str::pattern::Pattern;

use serde_json::{Map, Value};

use crate::config::Overrides;

pub(crate) fn filter_config_keys_by_prefix<'a, K, I>(
    overrides: Map<String, Value>,
    prefix_list: &'a mut K,
) -> Map<String, Value>
where
    K: Iterator<Item = I>,
    I: Pattern<'a>,
{
    overrides
        .into_iter()
        .filter(|(key, _)| prefix_list.any(|prefix_str| key.starts_with(prefix_str)))
        .collect()
}

pub trait Overridden<T: TryFrom<Map<String, Value>>>: Clone {
    fn get_overrides(&self) -> Overrides;

    fn filter_keys_by_prefix<'a, K, I>(
        context: &Self,
        prefix_list: &'a mut K,
    ) -> Result<T, <T as TryFrom<Map<String, Value>>>::Error>
    where
        K: Iterator<Item = I>,
        I: AsRef<str>,
    {
        let filtered_override =
            filter_config_keys_by_prefix(context.get_overrides().into(), prefix_list);

        T::try_from(filtered_override)
    }
}
