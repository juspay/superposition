use chrono::{DateTime, Utc};
use serde::Deserialize;
use strum::IntoEnumIterator;
use superposition_derives::{IsEmpty, QueryParam};

use crate::{
    custom_query::{CommaSeparatedQParams, CommaSeparatedStringQParams, QueryParam},
    database::models::cac::EventAction,
    IsEmpty, SortBy,
};

#[derive(Clone, PartialEq, Deserialize, QueryParam, IsEmpty)]
pub struct AuditQueryFilters {
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    #[query_param(skip_if_empty, iterable)]
    pub table: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub action: Option<CommaSeparatedQParams<EventAction>>,
    #[query_param(skip_if_empty)]
    pub username: Option<String>,
    pub sort_by: Option<SortBy>,
}

impl Default for AuditQueryFilters {
    fn default() -> Self {
        Self {
            action: Some(CommaSeparatedQParams(EventAction::iter().collect())),
            from_date: None,
            to_date: None,
            table: None,
            username: None,
            sort_by: Some(SortBy::Desc),
        }
    }
}
