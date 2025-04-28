use std::fmt::{self, Display};

use serde::Deserialize;
use superposition_derives::IsEmpty;

use crate::{custom_query::CommaSeparatedStringQParams, IsEmpty, SortBy};

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
            parts.push(format!("prefix={prefix}"));
        }

        if let Some(sort_on) = &self.sort_on {
            parts.push(format!("sort_on={sort_on}"));
        }

        if let Some(sort_by) = &self.sort_by {
            parts.push(format!("sort_by={sort_by}"));
        }

        if let Some(created_by) = &self.created_by {
            parts.push(format!("created_by={created_by}"));
        }

        if let Some(last_modified_by) = &self.last_modified_by {
            parts.push(format!("last_modified_by={last_modified_by}"));
        }

        if let Some(plaintext) = &self.plaintext {
            parts.push(format!("plaintext={plaintext}"));
        }

        write!(f, "{}", parts.join("&"))
    }
}
