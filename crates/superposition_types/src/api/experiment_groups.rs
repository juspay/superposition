use core::fmt;
use std::fmt::Display;

#[cfg(feature = "diesel_derives")]
use crate::database::schema::experiment_groups;
use crate::{
    database::models::{
        experimentation::{i64_vec_deserialize, i64_vec_formatter, TrafficPercentage},
        ChangeReason, Description,
    },
    Cac, Condition, IsEmpty, SortBy,
};
#[cfg(feature = "diesel_derives")]
use diesel::query_builder::AsChangeset;
use serde::{Deserialize, Serialize};
use superposition_derives::IsEmpty;

#[derive(Debug, Clone, Deserialize)]
pub struct ExpGroupCreateRequest {
    pub name: String,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub context: Cac<Condition>,
    pub traffic_percentage: TrafficPercentage,
    #[serde(default, deserialize_with = "i64_vec_deserialize")]
    pub member_experiment_ids: Option<Vec<i64>>,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = experiment_groups))]
pub struct ExpGroupUpdateRequest {
    pub change_reason: ChangeReason,
    pub description: Option<Description>,
    pub traffic_percentage: Option<TrafficPercentage>,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = experiment_groups))]
pub struct ExpGroupMemberRequest {
    pub change_reason: ChangeReason,
    #[serde(with = "i64_vec_formatter")]
    pub member_experiment_ids: Vec<i64>,
}

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

#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, IsEmpty)]
pub struct ExpGroupFilters {
    pub name: Option<String>,
    pub created_by: Option<String>,
    pub last_modified_by: Option<String>,
    pub sort_on: Option<SortOn>,
    pub sort_by: Option<SortBy>,
}

impl Display for ExpGroupFilters {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut query_params = vec![];
        if let Some(key_name) = &self.name {
            query_params.push(format!("name={}", key_name));
        }
        if let Some(created_by) = &self.created_by {
            query_params.push(format!("created_by={}", created_by));
        }
        if let Some(last_modified_by) = &self.last_modified_by {
            query_params.push(format!("last_modified_by={}", last_modified_by));
        }
        if let Some(sort_on) = &self.sort_on {
            query_params.push(format!("sort_on={}", sort_on));
        }
        if let Some(sort_by) = &self.sort_by {
            query_params.push(format!("sort_by={}", sort_by));
        }
        write!(f, "{}", query_params.join("&"))
    }
}
