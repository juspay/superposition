use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use strum_macros::Display;
use superposition_derives::{IsEmpty, QueryParam};

#[cfg(feature = "diesel_derives")]
use crate::database::schema::functions;
use crate::{
    custom_query::{CommaSeparatedQParams, QueryParam},
    database::models::{
        cac::{FunctionCode, FunctionRuntimeVersion, FunctionType},
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
    pub draft_runtime_version: Option<FunctionRuntimeVersion>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateFunctionRequest {
    pub function_name: FunctionName,
    pub function: FunctionCode,
    #[serde(default = "FunctionType::default")]
    pub function_type: FunctionType,
    pub runtime_version: FunctionRuntimeVersion,
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

#[derive(
    Debug, Display, Clone, Serialize, Deserialize, PartialEq, strum_macros::EnumIter,
)]
pub enum KeyType {
    ConfigKey,
    Dimension,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FunctionExecutionRequest {
    #[serde(rename = "value_validate")]
    ValueValidationFunctionRequest {
        key: String,
        value: Value,
        r#type: KeyType,
        environment: FunctionEnvironment,
    },
    #[serde(rename = "value_compute")]
    ValueComputeFunctionRequest {
        name: String,
        prefix: String,
        r#type: KeyType,
        environment: FunctionEnvironment,
    },
    #[serde(rename = "context_validate")]
    ContextValidationFunctionRequest { environment: FunctionEnvironment },
    #[serde(rename = "change_reason_validate")]
    ChangeReasonValidationFunctionRequest { change_reason: ChangeReason },
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq)]
pub struct FunctionEnvironment {
    pub context: Map<String, Value>,
    pub overrides: Map<String, Value>,
}

pub const CONTEXT_VALIDATION_FN_NAME: &str = "context_validation";
pub const CHANGE_REASON_VALIDATION_FN_NAME: &str = "change_reason_validation";

impl FunctionExecutionRequest {
    pub fn value_validation_default() -> Self {
        Self::ValueValidationFunctionRequest {
            key: String::new(),
            value: Value::String(String::new()),
            r#type: KeyType::ConfigKey,
            environment: FunctionEnvironment::default(),
        }
    }
    pub fn value_compute_default() -> Self {
        Self::ValueComputeFunctionRequest {
            name: String::new(),
            prefix: String::new(),
            r#type: KeyType::Dimension,
            environment: FunctionEnvironment::default(),
        }
    }
    pub fn context_validation_default() -> Self {
        Self::ContextValidationFunctionRequest {
            environment: FunctionEnvironment::default(),
        }
    }
    pub fn change_reason_validation_default() -> Self {
        Self::ChangeReasonValidationFunctionRequest {
            change_reason: ChangeReason::default(),
        }
    }
    pub fn function_identifier(&self) -> String {
        match self {
            FunctionExecutionRequest::ValueValidationFunctionRequest { key, .. } => {
                String::from(key)
            }
            FunctionExecutionRequest::ValueComputeFunctionRequest { name, .. } => {
                String::from(name)
            }
            FunctionExecutionRequest::ContextValidationFunctionRequest { .. } => {
                String::from(CONTEXT_VALIDATION_FN_NAME)
            }
            FunctionExecutionRequest::ChangeReasonValidationFunctionRequest {
                ..
            } => String::from(CHANGE_REASON_VALIDATION_FN_NAME),
        }
    }
}

impl From<&FunctionType> for FunctionExecutionRequest {
    fn from(function_type: &FunctionType) -> Self {
        match function_type {
            FunctionType::ValueValidation => {
                FunctionExecutionRequest::value_validation_default()
            }
            FunctionType::ValueCompute => {
                FunctionExecutionRequest::value_compute_default()
            }
            FunctionType::ContextValidation => {
                FunctionExecutionRequest::context_validation_default()
            }
            FunctionType::ChangeReasonValidation => {
                FunctionExecutionRequest::change_reason_validation_default()
            }
        }
    }
}

impl From<&FunctionExecutionRequest> for FunctionType {
    fn from(value: &FunctionExecutionRequest) -> Self {
        match value {
            FunctionExecutionRequest::ValueValidationFunctionRequest { .. } => {
                FunctionType::ValueValidation
            }
            FunctionExecutionRequest::ValueComputeFunctionRequest { .. } => {
                FunctionType::ValueCompute
            }
            FunctionExecutionRequest::ContextValidationFunctionRequest { .. } => {
                FunctionType::ContextValidation
            }
            FunctionExecutionRequest::ChangeReasonValidationFunctionRequest {
                ..
            } => FunctionType::ChangeReasonValidation,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionExecutionResponse {
    pub fn_output: Value,
    pub stdout: String,
    pub function_type: FunctionType,
}

#[derive(Debug, Default, Clone, Deserialize, PartialEq, IsEmpty, QueryParam)]
pub struct ListFunctionFilters {
    #[query_param(skip_if_empty, iterable)]
    pub function_type: Option<CommaSeparatedQParams<FunctionType>>,
}
