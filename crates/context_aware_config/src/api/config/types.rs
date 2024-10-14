use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::{Condition, Contextual, Overrides};

#[derive(Serialize, Deserialize)]
pub struct Config {
    pub contexts: Vec<Context>,
    pub overrides: HashMap<String, Overrides>,
    pub default_configs: Map<String, Value>,
}

#[derive(Serialize, Clone, Deserialize)]
pub struct Context {
    pub id: String,
    pub condition: Condition,
    pub priority: i32,
    pub override_with_keys: [String; 1],
}

impl Contextual for Context {
    fn get_condition(&self) -> Condition {
        self.condition.clone()
    }
}
