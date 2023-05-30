use serde::{Deserialize, Serialize};
use serde_json::{ Value, Map};

#[derive(Deserialize)]
pub struct AddContextReq {
    pub context: Vec<Map<String, Value>>,
    pub r#override: Value,
}

#[derive(Serialize)]
pub struct AddContextResp {
    pub context_id: String,
    pub override_id: String,
    pub priority: i32,
}

#[derive(Deserialize)]
pub struct PaginationParams {
    pub page: Option<u32>,
    pub size: Option<u32>
}
