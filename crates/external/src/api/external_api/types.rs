use serde::Serialize;
use serde_json::Value;

#[derive(Serialize)]
pub struct DiffResponse {
    pub before: Value,
    pub after: Value,
}
