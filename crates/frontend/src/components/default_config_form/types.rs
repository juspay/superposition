use serde::Serialize;
use serde_json::Value;

#[derive(Serialize, Clone)]
pub struct DefaultConfigCreateReq {
    pub schema: Value,
    pub value: Value,
}
