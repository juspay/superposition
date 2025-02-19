use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::database::models::{ChangeReason, Description};

/********** Context Bulk API Type *************/

#[derive(Deserialize, Serialize)]
pub struct ContextPutReq {
    pub context: Map<String, Value>,
    pub r#override: Value,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Deserialize, Serialize)]
pub enum ContextAction {
    PUT(ContextPutReq),
    REPLACE(ContextPutReq),
    DELETE(String),
    MOVE((String, ContextMoveReq)),
}

#[derive(Deserialize, Serialize, Debug)]
pub struct ContextPutResp {
    pub context_id: String,
    pub override_id: String,
    pub weight: String,
}

#[derive(Serialize, Deserialize, Debug)]
pub enum ContextBulkResponse {
    PUT(ContextPutResp),
    REPLACE(ContextPutResp),
    DELETE(String),
    MOVE(ContextPutResp),
}

#[derive(Deserialize, Serialize, Clone)]
pub struct ContextMoveReq {
    pub context: Map<String, Value>,
    pub description: Description,
    pub change_reason: ChangeReason,
}
