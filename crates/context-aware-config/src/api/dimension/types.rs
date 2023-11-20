use serde::Deserialize;
use serde_json::Value;

#[derive(Deserialize)]
pub struct CreateReq {
    pub dimension: String,
    pub priority: u16,
    pub schema: Value,
}
