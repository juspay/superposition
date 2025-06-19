use serde::Deserialize;
use serde_json::{Map, Value};

#[derive(Deserialize)]
pub struct ContextPayload {
    pub context: Map<String, Value>,
}
