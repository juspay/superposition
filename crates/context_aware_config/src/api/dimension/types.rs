use derive_more::{AsRef, Deref, DerefMut, Into};
use regex::Regex;
use serde::{Deserialize, Deserializer};
use serde_json::Value;

#[derive(Debug, Deserialize)]
pub struct CreateReq {
    pub dimension: DimensionName,
    pub priority: Priority,
    pub schema: Value,
    #[serde(default, deserialize_with = "deserialize_option")]
    pub function_name: Option<Value>,
}

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into)]
#[serde(try_from = "i32")]
pub struct Priority(i32);
impl Priority {
    fn validate_data(priority_val: i32) -> Result<Self, String> {
        if priority_val <= 0 {
            return Err("Priority should be greater than 0".to_string());
        } else {
            Ok(Self(priority_val))
        }
    }
}

impl TryFrom<i32> for Priority {
    type Error = String;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        Ok(Priority::validate_data(value)?)
    }
}

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into)]
#[serde(try_from = "String")]
pub struct DimensionName(String);
impl DimensionName {
    pub fn validate_data(name: String) -> Result<Self, String> {
        let regex_str: &str = "^[a-zA-Z0-9-_]([a-zA-Z0-9-_.]{0,254}[a-zA-Z0-9-_])?$";

        let regex = Regex::new(regex_str).map_err(|err| {
            log::error!("error while validating dimensionName : {err}");
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

impl TryFrom<String> for DimensionName {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Ok(DimensionName::validate_data(value)?)
    }
}

fn deserialize_option<'de, D>(deserializer: D) -> Result<Option<Value>, D::Error>
where
    D: Deserializer<'de>,
{
    let value: Value = Deserialize::deserialize(deserializer)?;
    Ok(Some(value))
}
