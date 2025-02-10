use derive_more::{AsRef, Deref, DerefMut, Into};
use diesel::AsChangeset;
use serde::Deserialize;
use serde_json::Value;
use superposition_types::{
    api::function::FunctionNameEnum, database::schema::dimensions, Position, RegexEnum,
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

#[derive(Debug, Deserialize, Clone)]
pub struct UpdateReq {
    pub position: Option<Position>,
    pub schema: Option<Value>,
    pub function_name: Option<FunctionNameEnum>,
    pub description: Option<String>,
    pub change_reason: String,
}

impl From<UpdateReq> for UpdateReqChangeset {
    fn from(req: UpdateReq) -> Self {
        Self {
            position: req.position.map(|x| x.into()),
            schema: req.schema,
            function_name: req.function_name.map(|x| x.to_option()),
            description: req.description,
            change_reason: req.change_reason,
        }
    }
}

//changeset type for update request type
#[derive(AsChangeset)]
#[diesel(table_name = dimensions)]
pub struct UpdateReqChangeset {
    pub position: Option<i32>,
    pub schema: Option<Value>,
    //function_name is nullable column, so to support null update with changeset
    //we had to make it Option<Option<String>>
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
