use std::{
    cmp::{Ordering, min},
    collections::{HashMap, HashSet},
};

use chrono::{DateTime, Utc};
use diesel::{QueryDsl, RunQueryDsl};
use service_utils::service::types::SchemaName;
use superposition_macros::unexpected_error;
use superposition_types::{
    DBConnection, PaginatedResponse, SortBy,
    api::default_config::{DefaultConfigFilters, ListDefaultConfigResponse, SortOn},
    database::{models::cac::DefaultConfig, schema::default_configs::dsl},
    result as superposition,
};

#[derive(Clone, Debug)]
struct InternalStructure {
    value: Option<DefaultConfig>,
    sub_keys: HashMap<String, InternalStructure>,
}

fn unflatten_map(flat_map: Vec<DefaultConfig>) -> Result<InternalStructure, String> {
    let mut result = InternalStructure {
        value: None,
        sub_keys: HashMap::new(),
    };

    for config in flat_map.into_iter() {
        let mut current_map = &mut result;
        let mut parts = config.key.split('.').peekable();
        let config = DefaultConfig {
            key: config
                .key
                .split('.')
                .next_back()
                .unwrap_or_default()
                .to_string(),
            ..config
        };

        while let Some(part) = parts.next() {
            let is_last = parts.peek().is_none();

            if is_last {
                let entry =
                    current_map
                        .sub_keys
                        .entry(part.to_string())
                        .or_insert_with(|| InternalStructure {
                            value: None,
                            sub_keys: HashMap::new(),
                        });
                entry.value = Some(config.clone());
            } else {
                current_map = current_map
                    .sub_keys
                    .entry(part.to_string())
                    .or_insert_with(|| InternalStructure {
                        value: None,
                        sub_keys: HashMap::new(),
                    });
            }
        }
    }

    Ok(result)
}

fn get_from_unflattened_map(
    unflattened_map: InternalStructure,
    flattened_key: &str,
) -> Option<InternalStructure> {
    let mut current_value = Some(unflattened_map);

    if flattened_key.is_empty() {
        return current_value;
    }

    let flattened_key = flattened_key.strip_suffix(".").unwrap_or(flattened_key);
    for part in flattened_key.split('.') {
        current_value = current_value.and_then(|v| v.sub_keys.get(part).cloned())
    }
    current_value
}

fn get_extreme_date<F>(
    entry: &InternalStructure,
    field_extractor: F,
    comparator: fn(DateTime<Utc>, DateTime<Utc>) -> DateTime<Utc>,
) -> Option<DateTime<Utc>>
where
    F: Fn(&DefaultConfig) -> DateTime<Utc> + Copy,
{
    fn inner<F>(
        value: &InternalStructure,
        field_extractor: F,
        comparator: fn(DateTime<Utc>, DateTime<Utc>) -> DateTime<Utc>,
    ) -> Option<DateTime<Utc>>
    where
        F: Fn(&DefaultConfig) -> DateTime<Utc> + Copy,
    {
        let mut result = value.value.as_ref().map(field_extractor);
        for child in value.sub_keys.values() {
            if let Some(child_date) = inner(child, field_extractor, comparator) {
                result = Some(match result {
                    Some(current) => comparator(current, child_date),
                    None => child_date,
                });
            }
        }
        result
    }
    inner(entry, field_extractor, comparator)
}

fn earliest_created_at(entry: &InternalStructure) -> Option<DateTime<Utc>> {
    get_extreme_date(entry, |config| config.created_at, DateTime::min)
}

fn latest_created_at(entry: &InternalStructure) -> Option<DateTime<Utc>> {
    get_extreme_date(entry, |config| config.created_at, DateTime::max)
}

fn earliest_modified_at(entry: &InternalStructure) -> Option<DateTime<Utc>> {
    get_extreme_date(entry, |config| config.last_modified_at, DateTime::min)
}

fn latest_modified_at(entry: &InternalStructure) -> Option<DateTime<Utc>> {
    get_extreme_date(entry, |config| config.last_modified_at, DateTime::max)
}

fn apply_sort_order(sort_by: SortBy, ord: Ordering) -> Ordering {
    match sort_by {
        SortBy::Asc => ord,
        SortBy::Desc => ord.reverse(),
    }
}

fn sort_keys(mut keys: Vec<String>, sort_by: SortBy) -> Vec<String> {
    keys.sort_by(|a, b| apply_sort_order(sort_by, a.cmp(b)));
    keys
}

fn sort_groups_by_date(
    filtered: &InternalStructure,
    sort_by: SortBy,
    date_for: fn(&InternalStructure) -> Option<DateTime<Utc>>,
) -> Vec<ListDefaultConfigResponse> {
    let mut group_entries = filtered
        .sub_keys
        .iter()
        .filter(|(_, v)| !v.sub_keys.is_empty())
        .map(|(k, v)| (k.clone(), date_for(v)))
        .collect::<Vec<_>>();

    group_entries.sort_by(|(a_key, a_date), (b_key, b_date)| {
        let ord = match (a_date, b_date) {
            (Some(a_dt), Some(b_dt)) => a_dt.cmp(b_dt),
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (None, None) => a_key.cmp(b_key),
        };
        apply_sort_order(sort_by, ord)
    });

    group_entries
        .into_iter()
        .map(|(k, _)| ListDefaultConfigResponse::Group(k))
        .collect::<Vec<_>>()
}

fn build_group_data(
    filtered: &InternalStructure,
    sort_on: SortOn,
    sort_by: SortBy,
) -> Vec<ListDefaultConfigResponse> {
    let date_fn = match (sort_on, sort_by) {
        (SortOn::Key, _) => {
            let keys = filtered
                .sub_keys
                .iter()
                .filter(|(_, v)| !v.sub_keys.is_empty())
                .map(|(k, _)| k.clone())
                .collect::<Vec<_>>();

            return sort_keys(keys, sort_by)
                .into_iter()
                .map(ListDefaultConfigResponse::Group)
                .collect();
        }
        (SortOn::CreatedAt, SortBy::Asc) => earliest_created_at,
        (SortOn::CreatedAt, SortBy::Desc) => latest_created_at,
        (SortOn::LastModifiedAt, SortBy::Asc) => earliest_modified_at,
        (SortOn::LastModifiedAt, SortBy::Desc) => latest_modified_at,
    };

    sort_groups_by_date(filtered, sort_by, date_fn)
}

fn build_config_data(
    filtered: &InternalStructure,
    sort_on: SortOn,
    sort_by: SortBy,
) -> Vec<ListDefaultConfigResponse> {
    let mut configs = filtered
        .sub_keys
        .values()
        .filter_map(|v| v.value.clone())
        .collect::<Vec<_>>();

    configs.sort_by(|a, b| {
        let ord = match sort_on {
            SortOn::Key => a.key.cmp(&b.key),
            SortOn::CreatedAt => a.created_at.cmp(&b.created_at),
            SortOn::LastModifiedAt => a.last_modified_at.cmp(&b.last_modified_at),
        };
        apply_sort_order(sort_by, ord)
    });

    configs
        .into_iter()
        .map(ListDefaultConfigResponse::Config)
        .collect()
}

pub(super) fn list(
    schema_name: &SchemaName,
    conn: &mut DBConnection,
    filters: &DefaultConfigFilters,
    offset: i64,
    count: i64,
    show_all: bool,
) -> superposition::Result<PaginatedResponse<ListDefaultConfigResponse>> {
    let configs = dsl::default_configs
        .schema_name(schema_name)
        .get_results::<DefaultConfig>(conn)?;

    let unflattened_config_map = unflatten_map(configs)
        .map_err(|e| unexpected_error!("Failed to group configs: {}", e))?;

    let prefix = filters.prefix.clone().unwrap_or_default();
    let prefix_filtered = get_from_unflattened_map(unflattened_config_map, &prefix);

    let name_filtered = match (prefix_filtered, filters.name.as_ref()) {
        (Some(filtered), Some(name_filters)) => {
            let name_set = name_filters.iter().cloned().collect::<HashSet<_>>();
            let filtered_sub_keys = filtered
                .sub_keys
                .into_iter()
                .filter(|(k, _)| name_set.contains(k))
                .collect::<HashMap<String, InternalStructure>>();

            Some(InternalStructure {
                value: filtered.value,
                sub_keys: filtered_sub_keys,
            })
        }
        (Some(filtered), None) => Some(filtered),
        (None, _) => None,
    };

    let sort_on = filters.sort_on.unwrap_or_default();
    let sort_by = filters.sort_by.unwrap_or_default();

    let (mut group_data, mut config_data) = match name_filtered.as_ref() {
        Some(filtered) => (
            build_group_data(filtered, sort_on, sort_by),
            build_config_data(filtered, sort_on, sort_by),
        ),
        None => (Vec::new(), Vec::new()),
    };

    let mut data: Vec<ListDefaultConfigResponse> =
        Vec::with_capacity(group_data.len() + config_data.len());
    data.append(&mut group_data);
    data.append(&mut config_data);

    let resp = if show_all {
        PaginatedResponse::all(data)
    } else {
        let total_items = data.len();
        let start = offset as usize;
        let end = min((offset + count) as usize, total_items);
        let data = data
            .get(start..end)
            .map(|slice| slice.to_vec())
            .unwrap_or_default();

        PaginatedResponse {
            total_pages: (total_items as f64 / count as f64).ceil() as i64,
            total_items: total_items as i64,
            data,
        }
    };

    Ok(resp)
}

#[cfg(test)]
mod tests {
    use superposition_types::database::models::{ChangeReason, Description};

    use super::*;

    #[test]
    fn test_unflatten_and_get() {
        let flat_map = vec![
            DefaultConfig {
                key: "a.b.c".to_string(),
                value: "value1".into(),
                created_at: Default::default(),
                created_by: "user1".to_string(),
                schema: Default::default(),
                value_validation_function_name: None,
                last_modified_at: Default::default(),
                last_modified_by: "user1".to_string(),
                description: Description::try_from("desc1".to_string()).unwrap(),
                change_reason: ChangeReason::try_from("reason1".to_string()).unwrap(),
                value_compute_function_name: None,
            },
            DefaultConfig {
                key: "a.b.d".to_string(),
                value: "value2".into(),
                created_at: Default::default(),
                created_by: "user2".to_string(),
                schema: Default::default(),
                value_validation_function_name: None,
                last_modified_at: Default::default(),
                last_modified_by: "user2".to_string(),
                description: Description::try_from("desc2".to_string()).unwrap(),
                change_reason: ChangeReason::try_from("reason2".to_string()).unwrap(),
                value_compute_function_name: None,
            },
            DefaultConfig {
                key: "a.e".to_string(),
                value: "value3".into(),
                created_at: Default::default(),
                created_by: "user3".to_string(),
                schema: Default::default(),
                value_validation_function_name: None,
                last_modified_at: Default::default(),
                last_modified_by: "user3".to_string(),
                description: Description::try_from("desc3".to_string()).unwrap(),
                change_reason: ChangeReason::try_from("reason3".to_string()).unwrap(),
                value_compute_function_name: None,
            },
        ];

        let unflattened = unflatten_map(flat_map).unwrap();
        let result = get_from_unflattened_map(unflattened.clone(), "a.b.c");
        assert_eq!(result.unwrap().value.unwrap().value, "value1".to_string());
        let result = get_from_unflattened_map(unflattened.clone(), "a.b.d");
        assert_eq!(result.unwrap().value.unwrap().value, "value2".to_string());
        let result = get_from_unflattened_map(unflattened.clone(), "a.e");
        assert_eq!(result.unwrap().value.unwrap().value, "value3".to_string());
        let result = get_from_unflattened_map(unflattened.clone(), "a.b");
        assert!(result.is_some());
        let result = get_from_unflattened_map(unflattened.clone(), "a");
        assert!(result.is_some());
        let result = get_from_unflattened_map(unflattened.clone(), "x.y.z");
        assert!(result.is_none());
    }
}
