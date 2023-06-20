//NOTE this code is copied over from sdk-config-server

use std::collections::HashMap;

use serde_json::{from_value, to_value, Error, Value};

fn type_of(a: &Value) -> String {
    if a.is_boolean() {
        return "Boolean".to_string();
    }

    if a.is_number() {
        return "Number".to_string();
    }

    if a.is_string() {
        return "String".to_string();
    }

    if a.is_array() {
        return "Array".to_string();
    }

    if a.is_null() {
        return "Null".to_string();
    }

    "Object".to_string()
}

fn merge_json_helper(
    tree1: &Value,
    tree2: &Value,
    add_new_paths: bool,
) -> Result<HashMap<String, Value>, Error> {
    let mut result_hash_map: HashMap<String, Value> = from_value(tree1.to_owned())?;

    if let Some(subtree) = tree2.as_object() {
        for (key, overriden_value) in subtree.iter() {
            if let Some(default_value) = tree1.get(key) {
                if type_of(default_value) == "Object" {
                    let merged_sub_tree =
                        merge_json_helper(default_value, overriden_value, add_new_paths)?;
                    result_hash_map.insert(key.to_string(), to_value(merged_sub_tree)?);
                } else {
                    result_hash_map.insert(key.to_string(), overriden_value.clone());
                }
            } else if add_new_paths {
                result_hash_map.insert(key.to_string(), overriden_value.clone());
            }
        }
    }

    Ok(result_hash_map)
}

pub fn patch_json(tree1: &Value, tree2: &Value) -> Result<Value, Error> {
    to_value(merge_json_helper(tree1, tree2, false)?)
}

pub fn deep_merge(tree1: &Value, tree2: &Value) -> Result<Value, Error> {
    to_value(merge_json_helper(tree1, tree2, true)?)
}

// ************ Tests *************

#[cfg(test)]
mod tests {
    use serde_json::json;

    #[test]
    fn patch_json() {
        let a = json!({"x":"y"});
        let b = json!({"x":"z", "p":"q"});
        assert_eq!(super::patch_json(&a, &b).unwrap(), json!({"x":"z"}));
    }

    #[test]
    fn deep_merge() {
        let a = json!({"x":"y"});
        let b = json!({"x":"z", "p":"q"});
        assert_eq!(
            super::deep_merge(&a, &b).unwrap(),
            json!({"x":"z", "p": "q"})
        );
    }
}
