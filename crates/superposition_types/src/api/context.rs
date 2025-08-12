use bigdecimal::BigDecimal;
use serde::{Deserialize, Serialize};
use superposition_derives::{IsEmpty, QueryParam};

use crate::{
    custom_query::{CommaSeparatedStringQParams, QueryParam},
    database::models::{cac::Context, ChangeReason, Description},
    Cac, Condition, IsEmpty, Overrides, SortBy,
};

#[derive(
    Deserialize, PartialEq, Clone, strum_macros::EnumIter, strum_macros::Display,
)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum SortOn {
    Weight,
    CreatedAt,
    LastModifiedAt,
}

impl SortOn {
    pub fn label(&self) -> String {
        match self {
            Self::CreatedAt => "Created At".to_string(),
            Self::LastModifiedAt => "Last Modified At".to_string(),
            Self::Weight => "Weight".to_string(),
        }
    }
}

impl Default for SortOn {
    fn default() -> Self {
        Self::Weight
    }
}

#[derive(Deserialize, PartialEq, Default, Clone, IsEmpty, QueryParam)]
pub struct ContextListFilters {
    #[query_param(skip_if_empty)]
    pub prefix: Option<CommaSeparatedStringQParams>,
    pub sort_on: Option<SortOn>,
    pub sort_by: Option<SortBy>,
    #[query_param(skip_if_empty)]
    pub created_by: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty)]
    pub last_modified_by: Option<CommaSeparatedStringQParams>,
    pub plaintext: Option<String>,
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum Identifier {
    Context(Cac<Condition>),
    Id(String),
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct UpdateRequest {
    pub context: Identifier,
    #[serde(rename = "override")]
    pub override_: Cac<Overrides>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct ContextValidationRequest {
    pub context: Cac<Condition>,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct PutRequest {
    pub context: Cac<Condition>,
    pub r#override: Cac<Overrides>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct MoveRequest {
    pub context: Cac<Condition>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Deserialize, Serialize, Clone)]
#[serde(rename_all = "UPPERCASE")]
pub enum ContextAction {
    Put(PutRequest),
    Replace(UpdateRequest),
    Delete(String),
    Move((String, MoveRequest)),
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "UPPERCASE")]
pub enum ContextBulkResponse {
    Put(Context),
    Replace(Context),
    Delete(String),
    Move(Context),
}

#[derive(Deserialize, Serialize)]
pub struct WeightRecomputeResponse {
    pub id: String,
    pub condition: Condition,
    pub old_weight: BigDecimal,
    pub new_weight: BigDecimal,
}

#[derive(Deserialize, Serialize)]
pub struct BulkOperation {
    pub operations: Vec<ContextAction>,
}

#[derive(Deserialize, Serialize)]
pub struct BulkOperationResponse {
    pub output: Vec<ContextBulkResponse>,
}
