use derive_more::{AsRef, Deref, DerefMut, Into};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use superposition_types::RegexEnum;

#[derive(Debug, Deserialize)]
pub struct UpdateFunctionRequest {
    pub function: Option<String>,
    pub runtime_version: Option<String>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Debug, Deserialize)]
pub struct CreateFunctionRequest {
    pub function_name: FunctionName,
    pub function: String,
    pub runtime_version: String,
    pub description: String,
    pub change_reason: String,
}

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into)]
#[serde(try_from = "String")]
pub struct FunctionName(String);
impl FunctionName {
    pub fn validate_data(name: String) -> Result<Self, String> {
        let name = name.trim();
        RegexEnum::FunctionName
            .match_regex(name)
            .map(|_| Self(name.to_string()))
    }
}

impl TryFrom<String> for FunctionName {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::validate_data(value)
    }
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
    pub function_name: FunctionName,
    pub stage: Stage,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TestFunctionRequest {
    pub key: String,
    pub value: Value,
}
