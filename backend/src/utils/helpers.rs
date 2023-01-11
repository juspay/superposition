use serde_json::{Value, from_value, to_value};
use std::{collections::BTreeMap};

pub fn sort_multi_level_keys_in_stringified_json(json: Value) -> Option<Value> {
    let b_tree: &BTreeMap<String, Value> = &from_value(json).ok()?;
    to_value(b_tree).ok()
}

fn _create_all_unique_subsets_helper(s: &Vec<&str> , idx: i32) -> Vec<String> {
    let mut n = idx;
	let mut vector_index = 0;
	let mut result: Vec<String> = Vec::new();

	while n > 0 {
		if (n & 1) == 1 {
			result.push(s[vector_index].to_owned());
		}
		n = n >> 1;
        vector_index += 1;
	}
	return result;
}

pub fn _create_all_unique_subsets(s: &Vec<&str>) -> Vec<Vec<String>> {
	let mut res: Vec<Vec<String>> = Vec::new();
    for i in 1..(1 << s.len()) {
        res.push(create_all_unique_subsets_helper(s ,i));
    }
	return res;
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

pub fn strip_double_quotes (str: &str) -> &str {

    if str.starts_with("\"") && str.ends_with("\"") {
        let len = str.len();
        return &str[1..len-1];
    }

    str
}