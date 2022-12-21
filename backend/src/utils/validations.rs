use serde_json::{Value, from_str};

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

    return "Object".to_string();
}

fn validate_sub_tree_helper(default_tree: &Value, overriding_tree: &Value, path: &mut Vec<String>) -> (bool, String) {
    if !are_both_of_same_type(default_tree, overriding_tree) {

        let mut result: Vec<String> = Vec::new();
        result.push("Expected type :".to_string());
        result.push(type_of(default_tree));

        result.push("~ Encountered type :".to_string());
        result.push(type_of(overriding_tree));

        result.push("\nPath :".to_string());
        result.push(path.join("/"));

        return (false, result.join(" "));
    }

    if !default_tree.is_object() {
        println!("{} => {}", &default_tree, &overriding_tree, );
        return (true, "".to_string());
    }

    // let tree = default_tree.as_object().unwrap();
    let subtree = overriding_tree.as_object().unwrap();

    for (key, overriden_value) in (&*subtree).iter() {

        let default_value = default_tree.get(key);

        path.push((&key).to_string());

        let (result, reason) = match default_value {
            Some(val) => validate_sub_tree_helper(val, overriden_value, path),
            None => {
                let mut result: Vec<String> = Vec::new();
                result.push("Key not found in default config".to_string());
                result.push("\nPath :".to_string());
                result.push(path.join("/"));
                return (false, result.join(" "));
            },
        };

        if !result {
            return (false, reason);
        }
    }

    return (true, "".to_string());
}


pub fn validate_sub_tree(default_tree: &Value, overriding_tree: &Value) -> (bool, String) {
    validate_sub_tree_helper(default_tree, overriding_tree,&mut Vec::new())
}


pub fn just_for_test() {

    let default_json: Value = from_str(r#"
    {   "package_dependencies": {
            "in.juspay.dotp": {
                "entry": "base.html",
                "root": "payments/in.juspay.dotp/"
            },
            "in.juspay.escrow": {
                "entry": "base.html",
                "root": "payments/in.juspay.escrow/",
                "hello": {
                    "test": 123
                }
            }
        }
    }
    "#).unwrap();

    let overridden_json: Value = from_str(r#"
    {   "package_dependencies": {
            "in.juspay.escrow": {
                "entry": "base.html",
                "hello": {
                    "test1": 123
                }
            }
        }
    }
    "#).unwrap();

    let (ans, reason) = validate_sub_tree(&default_json, &overridden_json);
    println!("Result : {}", ans);
    println!("Reason : {}", reason);

}