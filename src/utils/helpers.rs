use serde_json::{from_value, to_value, Value};
use std::collections::BTreeMap;

pub fn sort_multi_level_keys_in_stringified_json(json: Value) -> Option<Value> {
    let b_tree: &BTreeMap<String, Value> = &from_value(json).ok()?;
    to_value(b_tree).ok()
}

pub fn split_stringified_key_value_pair(input: &str) -> Vec<Vec<&str>> {
    let conditions_vector_splits: Vec<&str> = input.split("&").collect();
    let mut conditions_vector: Vec<Vec<&str>> = conditions_vector_splits
        .iter()
        .map(|&x| x.split("=").collect())
        .collect();

    conditions_vector.sort_by(|a, b| a[0].cmp(&b[0]));

    return conditions_vector;
}

pub fn strip_double_quotes(str: &str) -> &str {
    if str.starts_with("\"") && str.ends_with("\"") {
        let len = str.len();
        return &str[1..len - 1];
    }

    str
}
