#[cfg(feature = "diesel_derives")]
use crate::database::schema::experiment_groups;
use crate::{
    custom_query::{CommaSeparatedQParams, QueryParam},
    database::models::{
        experimentation::{
            i64_vec_deserialize, i64_vec_formatter, GroupType, TrafficPercentage,
        },
        ChangeReason, Description,
    },
    Condition, Exp, IsEmpty, SortBy,
};
#[cfg(feature = "diesel_derives")]
use diesel::query_builder::AsChangeset;
use serde::{Deserialize, Serialize};
use superposition_derives::{IsEmpty, QueryParam};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpGroupCreateRequest {
    pub name: String,
    pub description: Description,
    pub change_reason: ChangeReason,
    pub context: Exp<Condition>,
    pub traffic_percentage: TrafficPercentage,
    #[serde(default, deserialize_with = "i64_vec_deserialize")]
    pub member_experiment_ids: Option<Vec<i64>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = experiment_groups))]
pub struct ExpGroupUpdateRequest {
    pub change_reason: ChangeReason,
    pub description: Option<Description>,
    pub traffic_percentage: Option<TrafficPercentage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, IsEmpty, QueryParam)]
pub struct ExpGroupFilters {
    pub name: Option<String>,
    pub created_by: Option<String>,
    pub last_modified_by: Option<String>,
    pub sort_on: Option<SortOn>,
    pub sort_by: Option<SortBy>,
    #[query_param(skip_if_empty)]
    pub group_type: Option<CommaSeparatedQParams<GroupType>>,
}

// default with group type which contains only usercreated as value
impl Default for ExpGroupFilters {
    fn default() -> Self {
        ExpGroupFilters {
            name: None,
            created_by: None,
            last_modified_by: None,
            sort_on: Some(SortOn::default()),
            sort_by: Some(SortBy::Desc),
            group_type: Some(CommaSeparatedQParams(
                vec![GroupType::UserCreated].into_iter().collect(),
            )),
        }
    }
}
