use crate::{
    custom_query::{CommaSeparatedStringQParams, QueryParam},
    IsEmpty,
};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_derives::{IsEmpty, QueryParam};

#[derive(Deserialize)]
pub struct ContextPayload {
    pub context: Map<String, Value>,
}

#[derive(Deserialize, Serialize, IsEmpty, QueryParam, Default)]
pub struct ConfigQuery {
    pub version: Option<String>,
    #[query_param(skip_if_empty)]
    pub prefix: Option<CommaSeparatedStringQParams>,
}

#[derive(Deserialize, Serialize, IsEmpty, QueryParam, Default)]
pub struct ResolveConfigQuery {
    pub version: Option<String>,
    pub show_reasoning: Option<bool>,
    pub context_id: Option<String>,
    #[query_param(skip_if_empty)]
    pub prefix: Option<CommaSeparatedStringQParams>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, QueryParam)]
pub struct AuditQueryFilters {
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    #[query_param(skip_if_empty)]
    pub table: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty)]
    pub action: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty)]
    pub username: Option<String>,
}

impl Default for AuditQueryFilters {
    fn default() -> Self {
        Self {
            action: Some("INSERT,UPDATE,DELETE".parse().unwrap()),
            from_date: None,
            to_date: None,
            table: None,
            username: None,
        }
    }
}

impl IsEmpty for AuditQueryFilters {
    fn is_empty(&self) -> bool {
        self.from_date.is_none()
            && self.to_date.is_none()
            && self.table.is_none()
            && self.action.is_none()
            && self.username.is_none()
    }
}
