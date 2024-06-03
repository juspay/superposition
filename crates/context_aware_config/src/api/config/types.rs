use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

#[derive(Serialize, Deserialize)]
pub struct Config {
    pub contexts: Vec<Context>,
    pub overrides: Map<String, Value>,
    pub default_configs: Map<String, Value>,
}

#[derive(Serialize, Clone, Deserialize)]
pub struct Context {
    pub id: String,
    pub condition: Value,
    pub priority: i32,
    pub override_with_keys: [String; 1],
}
