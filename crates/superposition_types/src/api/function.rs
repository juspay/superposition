use serde::{Deserialize, Deserializer};
use serde_json::Value;

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
                Err(serde::de::Error::custom(
                    "Expected a string or null literal as the function name.",
                ))
            }
        }
    }
}

impl FunctionNameEnum {
    pub fn to_option(&self) -> Option<String> {
        match self {
            Self::Name(name) => Some(name.to_owned()),
            Self::Remove => None,
        }
    }
}
