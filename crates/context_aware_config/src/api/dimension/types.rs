use chrono::{DateTime, NaiveDateTime, Utc};
use derive_more::{AsRef, Deref, DerefMut, Into};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
use superposition_types::{cac::models::Dimension, RegexEnum};

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
        Ok(Self::validate_data(value)?)
    }
}

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into)]
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

#[derive(Debug, Deserialize, AsRef, Deref, DerefMut, Into)]
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

fn deserialize_option<'de, D>(deserializer: D) -> Result<Option<Value>, D::Error>
where
    D: Deserializer<'de>,
{
    let value: Value = Deserialize::deserialize(deserializer)?;
    Ok(Some(value))
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DimensionWithMandatory {
    pub dimension: String,
    pub priority: i32,
    pub position: i32,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
    pub function_name: Option<String>,
    pub last_modified_at: NaiveDateTime,
    pub last_modified_by: String,
    pub mandatory: bool,
}

impl DimensionWithMandatory {
    pub fn new(value: Dimension, mandatory: bool) -> Self {
        DimensionWithMandatory {
            dimension: value.dimension,
            priority: value.priority,
            position: value.position,
            created_at: value.created_at,
            created_by: value.created_by,
            schema: value.schema,
            function_name: value.function_name,
            last_modified_at: value.last_modified_at,
            last_modified_by: value.last_modified_by,
            mandatory,
        }
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
