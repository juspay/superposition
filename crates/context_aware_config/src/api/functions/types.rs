use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Deserialize)]
pub struct UpdateFunctionRequest {
    pub function: Option<String>,
    pub runtime_version: Option<String>,
    pub description: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct CreateFunctionRequest {
    pub function_name: String,
    pub function: String,
    pub runtime_version: String,
    pub description: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionResponse {
    pub function_name: String,
    pub function: String,
    pub function_description: String,
    pub runtime_version: String,
    pub status: String,
    pub published_at: String,
    pub drafted_at: String,
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize, strum_macros::Display)]
#[serde(rename_all = "UPPERCASE")]
#[strum(serialize_all = "lowercase")]
pub enum Stage {
    Draft,
    Published,
}

#[derive(Deserialize)]
pub struct TestParam {
    pub function_name: String,
    pub stage: Stage,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TestFunctionRequest {
    pub key: String,
    pub value: Value,
}
