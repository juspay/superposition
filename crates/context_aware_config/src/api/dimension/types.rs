use derive_more::{AsRef, Deref, DerefMut, Into};
use diesel::AsChangeset;
use serde::Deserialize;
use serde_json::Value;
use superposition_types::{
    database::models::cac::{deserialize_function_name, Position},
    database::schema::dimensions,
    RegexEnum,
};

#[derive(Debug, Deserialize)]
pub struct CreateReq {
    pub dimension: DimensionName,
    pub position: Position,
    pub schema: Value,
    pub function_name: Option<String>,
    pub description: String,
    pub change_reason: String,
}

#[derive(Debug, Deserialize, Clone, AsChangeset)]
#[diesel(table_name = dimensions)]
pub struct UpdateReq {
    pub position: Option<Position>,
    pub schema: Option<Value>,
    #[serde(default, deserialize_with = "deserialize_function_name")]
    pub function_name: Option<Option<String>>,
    pub description: Option<String>,
    pub change_reason: String,
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
        Self::validate_data(value)
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
