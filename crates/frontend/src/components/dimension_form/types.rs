use serde::Serialize;
use serde_json::Value;

#[derive(Serialize, Clone)]
pub struct DimensionCreateReq {
    pub dimension: String,
    pub position: u32,
    pub schema: Value,
    pub function_name: Option<Value>,
    pub description: String,
    pub change_reason: String,
}

#[derive(Serialize, Clone)]
pub struct DimensionUpdateReq {
    pub position: Option<u32>,
    pub schema: Option<Value>,
    pub function_name: Option<Value>,
    pub description: String,
    pub change_reason: String,
}
