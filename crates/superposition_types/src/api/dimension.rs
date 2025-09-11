use chrono::{DateTime, Utc};
use derive_more::{AsRef, Deref, DerefMut, Into};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[cfg(feature = "diesel_derives")]
use crate::database::schema::dimensions;
use crate::{
    database::models::{
        cac::{
            deserialize_function_name, DependencyGraph, Dimension, DimensionType,
            Position,
        },
        ChangeReason, Description,
    },
    RegexEnum,
};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DimensionResponse {
    pub dimension: String,
    pub position: Position,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
    pub function_name: Option<String>,
    pub last_modified_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub mandatory: bool,
    pub dependency_graph: DependencyGraph,
    pub dependents: Vec<String>,
    pub dependencies: Vec<String>,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub autocomplete_function_name: Option<String>,
    pub dimension_type: DimensionType,
}

impl DimensionResponse {
    pub fn new(value: Dimension, mandatory: bool) -> Self {
        Self {
            dimension: value.dimension,
            position: value.position,
            created_at: value.created_at,
            created_by: value.created_by,
            schema: value.schema,
            function_name: value.function_name,
            last_modified_at: value.last_modified_at,
            last_modified_by: value.last_modified_by,
            mandatory,
            dependency_graph: value.dependency_graph,
            dependents: value.dependents,
            dependencies: value.dependencies,
            description: value.description,
            change_reason: value.change_reason,
            autocomplete_function_name: value.autocomplete_function_name,
            dimension_type: value.dimension_type,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct CreateRequest {
    pub dimension: DimensionName,
    pub position: Position,
    pub schema: Value,
    pub function_name: Option<String>,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub autocomplete_function_name: Option<String>,
    #[serde(default)]
    pub dimension_type: DimensionType,
    pub cohort_based_on: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = dimensions))]
pub struct UpdateRequest {
    pub position: Option<Position>,
    pub schema: Option<Value>,
    #[serde(default, deserialize_with = "deserialize_function_name")]
    pub function_name: Option<Option<String>>,
    #[serde(default, deserialize_with = "deserialize_function_name")]
    pub autocomplete_function_name: Option<Option<String>>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Debug, Deserialize, Serialize, AsRef, Deref, DerefMut, Into, Clone)]
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
pub struct DeleteRequest(String);

impl TryFrom<String> for DeleteRequest {
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
