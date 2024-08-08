use derive_more::{AsRef, Deref, DerefMut, Into};
use regex::Regex;
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
    pub function_name: FunctionName,
    pub function: String,
    pub runtime_version: String,
    pub description: String,
}

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into)]
#[serde(try_from = "String")]
pub struct FunctionName(String);
impl FunctionName {
    pub fn validate_data(name: String) -> Result<Self, String> {
        let regex_str: &str = "^[a-zA-Z0-9-_]([a-zA-Z0-9-_.]{0,254}[a-zA-Z0-9-_])?$";

        let regex = Regex::new(regex_str).map_err(|err| {
            log::error!("error while validating functionName : {err}");
            "Something went wrong".to_string()
        })?;

        if !regex.is_match(&name) {
            return Err(format!(
                "The key name {} is invalid, it should obey the regex {}. \
                It can contain the following characters only [a-zA-Z0-9-_.] \
                and it should not start or end with a '.' character.",
                name, regex_str
            ));
        } else {
            Ok(Self(name))
        }
    }
}

impl TryFrom<String> for FunctionName {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Ok(FunctionName::validate_data(value)?)
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
    pub function_name: String,
    pub stage: Stage,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TestFunctionRequest {
    pub key: String,
    pub value: Value,
}
