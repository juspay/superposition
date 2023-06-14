use serde::Deserialize;
use serde_json::Value;

#[derive(Deserialize)]
pub struct CreateReq {
    pub value: Value,
}
