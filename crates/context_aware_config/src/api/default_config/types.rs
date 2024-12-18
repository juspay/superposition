use derive_more::{AsRef, Deref, DerefMut, Into};
use serde::{Deserialize, Deserializer};
use serde_json::{Map, Value};
use superposition_types::RegexEnum;

#[derive(Debug, Deserialize)]
pub struct CreateReq {
    pub key: DefaultConfigKey,
    pub value: Value,
    pub schema: Map<String, Value>,
    pub function_name: Option<String>,
    pub description: String,
    pub change_reason: String,
}

#[derive(Debug, Deserialize)]
pub struct UpdateReq {
    #[serde(default, deserialize_with = "deserialize_option")]
    pub value: Option<Value>,
    pub schema: Option<Map<String, Value>>,
    pub function_name: Option<FunctionNameEnum>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Debug, Clone)]
pub enum FunctionNameEnum {
    Name(String),
    Remove,
}

impl<'de> Deserialize<'de> for FunctionNameEnum {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let map: Value = Deserialize::deserialize(deserializer)?;
        match map {
            Value::String(func_name) => Ok(Self::Name(func_name)),
            Value::Null => Ok(Self::Remove),
            _ => {
                log::error!("Expected a string or null literal as the function name.");
                Err("Expected a string or null literal as the function name.")
                    .map_err(serde::de::Error::custom)
            }
        }
    }
}

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into)]
#[serde(try_from = "String")]
pub struct DefaultConfigKey(String);
impl DefaultConfigKey {
    pub fn validate_data(name: String) -> Result<Self, String> {
        let name = name.trim();
        RegexEnum::DefaultConfigKey
            .match_regex(name)
            .map(|_| Self(name.to_string()))
    }
}

impl TryFrom<String> for DefaultConfigKey {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Ok(Self::validate_data(value)?)
    }
}

fn deserialize_option<'de, D>(deserializer: D) -> Result<Option<Value>, D::Error>
where
    D: Deserializer<'de>,
{
    let value: Value = Deserialize::deserialize(deserializer)?;
    Ok(Some(value))
}
