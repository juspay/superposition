use std::collections::HashMap;

use superposition_types::database::models::cac::DefaultConfig;

#[derive(Clone, Debug)]
pub(super) struct InternalStructure {
    pub(super) value: Option<DefaultConfig>,
    pub(super) sub_keys: HashMap<String, InternalStructure>,
}

pub(super) fn unflatten_map(
    flat_map: Vec<DefaultConfig>,
) -> Result<InternalStructure, String> {
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

pub(super) fn get_from_unflattened_map(
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
