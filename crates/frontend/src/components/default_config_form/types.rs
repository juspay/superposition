use serde::Serialize;
use serde_json::Value;

#[derive(Serialize, Clone)]
pub struct DefaultConfigCreateReq {
    pub key: String,
    pub schema: Value,
    pub value: Value,
    pub function_name: Option<Value>,
    pub description: String,
    pub change_reason: String,
}

#[derive(Serialize, Clone)]
pub struct DefaultConfigUpdateReq {
    pub schema: Value,
    pub value: Value,
    pub function_name: Option<Value>,
    pub description: String,
    pub change_reason: String,
}
