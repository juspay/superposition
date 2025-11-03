use chrono::{DateTime, Utc};
#[cfg(feature = "diesel_derives")]
use diesel::{sql_types::Text, AsExpression};
use serde::{Deserialize, Serialize};
use strum::IntoEnumIterator;
#[cfg(feature = "diesel_derives")]
use superposition_derives::TextToSql;
use superposition_derives::{IsEmpty, QueryParam};

use crate::{
    custom_query::{CommaSeparatedQParams, CommaSeparatedStringQParams, QueryParam},
    IsEmpty, SortBy,
};

#[derive(Debug, Clone, PartialEq, Deserialize, QueryParam, IsEmpty)]
pub struct AuditQueryFilters {
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    #[query_param(skip_if_empty, iterable)]
    pub table: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub action: Option<CommaSeparatedQParams<AuditAction>>,
    #[query_param(skip_if_empty)]
    pub username: Option<String>,
    pub sort_by: Option<SortBy>,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Deserialize,
    Serialize,
    strum_macros::EnumIter,
    strum_macros::EnumString,
    strum_macros::Display,
)]
#[serde(rename_all = "UPPERCASE")]
#[strum(serialize_all = "UPPERCASE")]
#[cfg_attr(feature = "diesel_derives", derive(AsExpression, TextToSql))]
#[cfg_attr(feature = "diesel_derives", diesel(sql_type = Text))]
pub enum AuditAction {
    Insert,
    Update,
    Delete,
}

impl From<&AuditAction> for String {
    fn from(value: &AuditAction) -> Self {
        value.to_string()
    }
}

impl Default for AuditQueryFilters {
    fn default() -> Self {
        Self {
            action: Some(CommaSeparatedQParams(AuditAction::iter().collect())),
            from_date: None,
            to_date: None,
            table: None,
            username: None,
            sort_by: Some(SortBy::Desc),
        }
    }
}
