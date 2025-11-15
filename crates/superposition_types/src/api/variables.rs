#[cfg(feature = "diesel_derives")]
use diesel::query_builder::AsChangeset;

use serde::{Deserialize, Serialize};
use superposition_derives::{IsEmpty, QueryParam};

use crate::custom_query::QueryParam;
use crate::database::models::{
    others::VariableName,
    {ChangeReason, Description},
};
#[cfg(feature = "diesel_derives")]
use crate::database::schema::variables;

use crate::IsEmpty;

#[derive(
    Debug, Clone, PartialEq, Serialize, Deserialize, Default, QueryParam, IsEmpty,
)]
pub struct VariableFilters {
    #[query_param(skip_if_empty)]
    pub name: Option<String>,
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
