use serde::Deserialize;
use superposition_derives::DisplayQuery;

use crate::{custom_query::CommaSeparatedStringQParams, SortBy};

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

#[derive(Deserialize, PartialEq, Default, Clone, DisplayQuery)]
pub struct ContextListFilters {
    pub prefix: Option<CommaSeparatedStringQParams>,
    pub sort_on: Option<SortOn>,
    pub sort_by: Option<SortBy>,
    pub created_by: Option<CommaSeparatedStringQParams>,
    pub last_modified_by: Option<CommaSeparatedStringQParams>,
    pub plaintext: Option<String>,
}
