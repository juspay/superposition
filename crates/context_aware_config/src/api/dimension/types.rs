use derive_more::{AsRef, Deref, DerefMut, Into};
use serde::{Deserialize, Deserializer};
use serde_json::Value;
use superposition_types::RegexEnum;

#[derive(Debug, Deserialize)]
pub struct CreateReq {
    pub dimension: DimensionName,
    pub position: Position,
    pub schema: Value,
    pub function_name: Option<String>,
}

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into, Clone)]
#[serde(try_from = "i32")]
pub struct Position(i32);
impl Position {
    fn validate_data(position_val: i32) -> Result<Self, String> {
        if position_val < 0 {
            return Err("Position should be greater than equal to 0".to_string());
        } else {
            Ok(Self(position_val))
        }
    }
}

impl TryFrom<i32> for Position {
    type Error = String;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        Ok(Self::validate_data(value)?)
    }
}

impl Default for Position {
    fn default() -> Self {
        Position(0)
    }
}

#[derive(Debug, Deserialize, Clone)]
pub struct UpdateReq {
    pub position: Option<Position>,
    pub schema: Option<Value>,
    pub function_name: Option<FunctionNameEnum>,
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

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into, Clone)]
#[serde(try_from = "String")]
pub struct DimensionName(String);
impl DimensionName {
    pub fn validate_data(name: String) -> Result<Self, String> {
        let name = name.trim();
        RegexEnum::DimensionName
            .match_regex(name)
            .map(|_| Self(name.to_string()))
    }
}

impl TryFrom<String> for DimensionName {
    type Error = String;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Ok(Self::validate_data(value)?)
    }
}

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into)]
#[serde(try_from = "String")]
pub struct DeleteReq(String);

impl TryFrom<String> for DeleteReq {
    type Error = String;
    fn try_from(name: String) -> Result<Self, Self::Error> {
        let name = name.trim();
        if name == "variantIds" {
            Err("variantIds cannot be deleted".to_string())
        } else {
            Ok(Self(name.to_owned()))
        }
    }
}
