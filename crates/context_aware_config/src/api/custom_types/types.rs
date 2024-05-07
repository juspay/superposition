use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CreateTypeRequest {
    pub type_schema: Value,
    pub display_name: String,
    pub type_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CustomTypeResponse {
    pub id: uuid::Uuid,
    pub type_schema: Value,
    pub display_name: String,
    pub type_name: String,
    pub created_at: String,
    pub last_modified: String,
    pub created_by: String,
}
