use std::collections::HashMap;

use serde_json::{Value, to_value};

fn are_both_of_same_type(a: &Value, b: &Value) -> bool {
    (a.is_boolean() && b.is_boolean())
    || (a.is_number() && b.is_number())
    || (a.is_string() && b.is_string())
    || (a.is_array() && b.is_array())
    || (a.is_object() && b.is_object())
}

fn type_of(a: &Value) -> String{
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

    return "Object".to_string();
}

fn create_type_mismatch_error(default_value: &Value, overriding_value: &Value, path: &mut Vec<String>) -> Value {

    let mut error_map = HashMap::new();
    error_map.insert("Path".to_string(), path.join("/"));
    error_map.insert("Expected type".to_string(), type_of(default_value));
    error_map.insert("Encountered type".to_string(), type_of(overriding_value));

    let error_message = to_value(error_map).unwrap(); // .map_err(|_| ValidationErrors::ErrorMessageParsingError)?;

    error_message
}

fn create_structure_mismatch_error(path: &mut Vec<String>) -> Value {

    let mut error_map = HashMap::new();

    error_map.insert("Path".to_string(), path.join("/"));
    if let Some(_) = path.last() {
        error_map.insert("Reason".to_string(), "Key not found in default config".to_string());
    }

    let error_message = to_value(error_map).unwrap(); // .map_err(|_| ValidationErrors::ErrorMessageParsingError)?;

    error_message
}

fn validate_sub_tree_helper(default_tree: &Value, overriding_tree: &Value, path: &mut Vec<String>) -> Result<bool, Value> {

    if !are_both_of_same_type(default_tree, overriding_tree) {
        let error_message = create_type_mismatch_error(default_tree, overriding_tree, path);
        return Err(error_message);
    }

    if !default_tree.is_object() {
        return Ok(true);
    }

    let subtree =
        overriding_tree.as_object().unwrap();

    for (key, overriden_value) in (&*subtree).iter() {

        let default_value = default_tree.get(key);

        path.push((&key).to_string());

        let result = match default_value {
            Some(val) => validate_sub_tree_helper(val, overriden_value, path),
            None => Err(create_structure_mismatch_error(path))
        }?;

        if !result {
            return Ok(false);
        }
    }

    return Ok(true);
}


pub fn validate_sub_tree(default_tree: &Value, overriding_tree: &Value) -> Result<bool, Value> {
    validate_sub_tree_helper(default_tree, overriding_tree,&mut vec!["".to_string()])
}


// pub fn just_for_test() {

//     let default_json: Value = from_str(r#"
//     {   "package_dependencies": {
//             "in.juspay.dotp": {
//                 "entry": "base.html",
//                 "root": "payments/in.juspay.dotp/"
//             },
//             "in.juspay.escrow": {
//                 "entry": "base.html",
//                 "root": "payments/in.juspay.escrow/",
//                 "hello": {
//                     "test": 123
//                 }
//             }
//         }
//     }
//     "#).unwrap();

//     let overridden_json: Value = from_str(r#"
//     {   "package_dependencies": {
//             "in.juspay.escrow": {
//                 "entry": "base.html",
//                 "hello": {
//                     "test1": 123
//                 }
//             }
//         }
//     }
//     "#).unwrap();

//     let (ans, reason) = validate_sub_tree(&default_json, &overridden_json);
//     println!("Result : {}", ans);
//     println!("Reason : {}", reason);

// }
