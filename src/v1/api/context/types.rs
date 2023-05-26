use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Deserialize)]
pub struct AddContextReq {
    pub context: Vec<Value>,
    pub r#override: Value,
}

#[derive(Serialize)]
pub struct AddContextResp {
    pub context_id: String,
    pub override_id: String,
}
