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
