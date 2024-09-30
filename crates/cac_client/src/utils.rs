use itertools::{self, Itertools};
use serde_json::Value;

pub mod core;

pub fn json_to_sorted_string(v: &Value) -> String {
    match v {
        Value::Object(m) => {
            let mut new_str: String = String::from("");
            for (i, val) in m.iter().sorted_by_key(|item| item.0) {
                let p: String = json_to_sorted_string(val);
                new_str.push_str(i);
                new_str.push_str(&String::from(":"));
                new_str.push_str(&p);
                new_str.push_str(&String::from("$"));
            }
            new_str
        }
        Value::String(m) => m.to_string(),
        Value::Number(m) => m.to_string(),
        Value::Bool(m) => m.to_string(),
        Value::Null => String::from("null"),
        Value::Array(m) => {
            let mut new_vec =
                m.iter().map(json_to_sorted_string).collect::<Vec<String>>();
            new_vec.sort();
            new_vec.join(",")
        }
    }
}
