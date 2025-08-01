use std::fmt::{self, Display};

use bigdecimal::BigDecimal;
use serde::{Deserialize, Serialize};
use superposition_derives::IsEmpty;

use crate::{
    custom_query::CommaSeparatedStringQParams,
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

#[derive(Deserialize, PartialEq, Default, Clone, IsEmpty)]
pub struct ContextListFilters {
    pub prefix: Option<CommaSeparatedStringQParams>,
    pub sort_on: Option<SortOn>,
    pub sort_by: Option<SortBy>,
    pub created_by: Option<CommaSeparatedStringQParams>,
    pub last_modified_by: Option<CommaSeparatedStringQParams>,
    pub plaintext: Option<String>,
}

impl Display for ContextListFilters {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = vec![];

        if let Some(prefix) = &self.prefix {
            if !prefix.is_empty() {
                parts.push(format!("prefix={prefix}"));
            }
        }

        if let Some(sort_on) = &self.sort_on {
            parts.push(format!("sort_on={sort_on}"));
        }

        if let Some(sort_by) = &self.sort_by {
            parts.push(format!("sort_by={sort_by}"));
        }

        if let Some(created_by) = &self.created_by {
            if !created_by.is_empty() {
                parts.push(format!("created_by={created_by}"));
            }
        }

        if let Some(last_modified_by) = &self.last_modified_by {
            if !last_modified_by.is_empty() {
                parts.push(format!("last_modified_by={last_modified_by}"));
            }
        }

        if let Some(plaintext) = &self.plaintext {
            parts.push(format!("plaintext={plaintext}"));
        }

        write!(f, "{}", parts.join("&"))
    }
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
