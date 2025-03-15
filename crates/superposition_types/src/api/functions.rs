#[cfg(feature = "diesel_derives")]
use crate::database::schema::functions;
use crate::{
    database::models::cac::{FunctionCode, FunctionTypes},
    RegexEnum,
};

use derive_more::{AsRef, Deref, DerefMut, Into};

#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

#[derive(Debug, Serialize, Deserialize, AsChangeset)]
#[cfg(feature = "diesel_derives")]
#[diesel(table_name = functions)]
pub struct UpdateFunctionRequest {
    #[serde(rename = "function")]
    pub draft_code: Option<FunctionCode>,
    #[serde(rename = "runtime_version")]
    pub draft_runtime_version: Option<String>,
    pub description: Option<String>,
    pub change_reason: String,
    #[serde(default = "FunctionTypes::default")]
    pub function_type: FunctionTypes,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateFunctionRequest {
    pub function_name: FunctionName,
    pub function: FunctionCode,
    #[serde(default = "FunctionTypes::default")]
    pub function_type: FunctionTypes,
    pub runtime_version: String,
    pub description: String,
    pub change_reason: String,
}

#[derive(Debug, Serialize, Deserialize, AsRef, Deref, DerefMut, Into)]
#[serde(try_from = "String")]
pub struct FunctionName(pub String);
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
    pub function: FunctionCode,
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FunctionExecutionRequest {
    ValidateFunctionRequest {
        key: String,
        value: Value,
    },
    AutocompleteFunctionRequest {
        name: String,
        prefix: String,
        environment: Value,
    },
}

impl FunctionExecutionRequest {
    pub fn validation_default() -> Self {
        Self::ValidateFunctionRequest {
            key: String::new(),
            value: Value::String(String::new()),
        }
    }
    pub fn autocomplete_default() -> Self {
        Self::AutocompleteFunctionRequest {
            name: String::new(),
            prefix: String::new(),
            environment: json!({
                "context": [],
                "overrides": {}
            }),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionExecutionResponse {
    pub fn_output: Value,
    pub stdout: String,
    pub function_type: FunctionTypes,
}
