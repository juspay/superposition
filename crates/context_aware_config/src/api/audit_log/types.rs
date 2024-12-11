use chrono::NaiveDateTime;
use serde::Deserialize;
use superposition_types::custom_query::CommaSeparatedStringQParams;

#[derive(Debug, Clone, Deserialize)]
pub struct AuditQueryFilters {
    pub from_date: Option<NaiveDateTime>,
    pub to_date: Option<NaiveDateTime>,
    pub table: Option<CommaSeparatedStringQParams>,
    pub action: Option<CommaSeparatedStringQParams>,
    pub username: Option<String>,
    pub count: Option<i64>,
    pub page: Option<i64>,
}
