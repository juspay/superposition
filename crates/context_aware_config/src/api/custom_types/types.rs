use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TypeTemplateRequest {
    pub type_schema: Value,
    pub type_name: String,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CustomTypeResponse {
    pub type_schema: Value,
    pub type_name: String,
    pub created_at: String,
    pub last_modified: String,
    pub created_by: String,
}
#[derive(Debug, Clone, Deserialize)]
pub struct QueryFilters {
    pub count: Option<i64>,
    pub page: Option<i64>,
}
