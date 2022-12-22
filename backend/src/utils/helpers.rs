use serde_json::{Value, from_value, to_value};
use std::{collections::BTreeMap};

pub fn sort_multi_level_keys_in_stringified_json(json: Value) -> Option<Value> {
    let b_tree: &BTreeMap<String, Value> = &from_value(json).ok()?;
    to_value(b_tree).ok()
}
