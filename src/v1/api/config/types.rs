use serde::Serialize;
use serde_json::{Map, Value};

#[derive(Serialize)]
pub struct Config {
    pub contexts: Vec<Context>,
    pub overrides: Map<String, Value>,
    pub default_configs: Map<String, Value>,
}

#[derive(Serialize)]
pub struct Context {
    pub context_id: String,
    pub condition: Value,
    pub override_with_keys: [String; 1],
}
