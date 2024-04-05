use serde::Serialize;
use serde_json::Value;

#[derive(Serialize, Clone)]
pub struct DimensionCreateReq {
    pub dimension: String,
    pub priority: u16,
    pub schema: Value,
    pub function_name: Option<Value>,
}
