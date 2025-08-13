use serde_json::Value;

pub struct AppState {
    pub namespaces: Vec<String>,
    pub tenants: Vec<String>,
}

pub const K8S_API_SERVER: &str = "https://127.0.0.1:33083";
pub const NAMESPACE: &str = "default";
pub const TOKEN: &str = "";

pub fn get_namespace(context: Value) -> String {
    let val: Vec<Value> =
        serde_json::from_value(context["and"][0]["=="].clone()).expect("error decoding context");

    for i in val {
        match i {
            Value::String(x) => return x,
            _ => continue,
        }
    }
    return String::from("mumbai");
}
