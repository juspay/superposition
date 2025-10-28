use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use strum_macros::Display;
use superposition_derives::{IsEmpty, QueryParam};

#[cfg(feature = "diesel_derives")]
use crate::database::schema::functions;
use crate::{
    custom_query::{CommaSeparatedQParams, QueryParam},
    database::models::{
        cac::{FunctionCode, FunctionType},
        ChangeReason, Description,
    },
    IsEmpty, RegexEnum,
};

#[derive(Debug, Serialize, Deserialize)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = functions))]
pub struct UpdateFunctionRequest {
    #[serde(rename = "function")]
    pub draft_code: Option<FunctionCode>,
    #[serde(rename = "runtime_version")]
    pub draft_runtime_version: Option<String>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateFunctionRequest {
    pub function_name: FunctionName,
    pub function: FunctionCode,
    #[serde(default = "FunctionType::default")]
    pub function_type: FunctionType,
    pub runtime_version: String,
    pub description: Description,
    pub change_reason: ChangeReason,
}

#[derive(Serialize, Deserialize)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = functions))]
pub struct FunctionStateChangeRequest {
    pub change_reason: ChangeReason,
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

#[derive(
    Copy,
    Clone,
    Debug,
    Serialize,
    Deserialize,
    PartialEq,
    strum_macros::Display,
    strum_macros::EnumIter,
)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum Stage {
    Published,
    Draft,
}

#[derive(Deserialize)]
pub struct TestParam {
    pub function_name: FunctionName,
    pub stage: Stage,
}

#[derive(Debug, Display, Clone, Serialize, Deserialize)]
pub enum KeyType {
    ConfigKey,
    Dimension,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FunctionExecutionRequest {
    ValueValidationFunctionRequest {
        key: String,
        value: Value,
        r#type: KeyType,
        context: Value,
    },
    ValueComputeFunctionRequest {
        name: String,
        prefix: String,
        r#type: KeyType,
        context: Value,
        environment: Value,
    },
    ContextValidationFunctionRequest {
        context: Value,
    },
}

impl FunctionExecutionRequest {
    pub fn value_validation_default() -> Self {
        Self::ValueValidationFunctionRequest {
            key: String::new(),
            value: Value::String(String::new()),
            r#type: KeyType::ConfigKey,
            context: json!({}),
        }
    }
    pub fn value_compute_default() -> Self {
        Self::ValueComputeFunctionRequest {
            name: String::new(),
            prefix: String::new(),
            r#type: KeyType::Dimension,
            context: json!({}),
            environment: json!({
                "context": [],
                "overrides": {}
            }),
        }
    }
    pub fn context_validation_default() -> Self {
        Self::ContextValidationFunctionRequest { context: json!({}) }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionExecutionResponse {
    pub fn_output: Value,
    pub stdout: String,
    pub function_type: FunctionType,
}

#[derive(
    Debug, Default, Clone, Serialize, Deserialize, PartialEq, IsEmpty, QueryParam,
)]
pub struct ListFunctionFilters {
    #[query_param(skip_if_empty)]
    pub function_type: Option<CommaSeparatedQParams<FunctionType>>,
}
