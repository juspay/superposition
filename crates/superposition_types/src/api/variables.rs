#[cfg(feature = "diesel_derives")]
use diesel::query_builder::AsChangeset;
use serde::{Deserialize, Serialize};
use strum_macros;
use superposition_derives::{IsEmpty, QueryParam};

use crate::custom_query::{CommaSeparatedStringQParams, QueryParam};
use crate::database::models::{
    others::VariableName,
    {ChangeReason, Description},
};
#[cfg(feature = "diesel_derives")]
use crate::database::schema::variables;
use crate::{IsEmpty, SortBy};

#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Default,
    PartialEq,
    strum_macros::EnumIter,
    strum_macros::Display,
)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum SortOn {
    Name,
    CreatedAt,
    #[default]
    LastModifiedAt,
}

#[derive(Debug, Clone, Deserialize, PartialEq, IsEmpty, QueryParam, Default)]
pub struct VariableFilters {
    #[query_param(skip_if_empty, iterable)]
    pub name: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub created_by: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub last_modified_by: Option<CommaSeparatedStringQParams>,
    pub sort_on: Option<SortOn>,
    pub sort_by: Option<SortBy>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateVariableRequest {
    pub name: VariableName,
    pub value: String,
    pub description: Description,
    pub change_reason: ChangeReason,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = variables))]
pub struct UpdateVariableRequest {
    pub value: Option<String>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}
