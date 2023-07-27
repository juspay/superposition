use serde::Deserialize;
use serde_json::{Map, Value};

#[derive(Deserialize)]
pub struct CreateReq {
    pub value: Value,
    pub schema: Map<String, Value>,
}
