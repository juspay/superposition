use chrono::NaiveDateTime;
use serde::Deserialize;
use superposition_types::custom_query::StringArgs;

#[derive(Debug, Clone, Deserialize)]
pub struct AuditQueryFilters {
    pub from_date: Option<NaiveDateTime>,
    pub to_date: Option<NaiveDateTime>,
    pub table: Option<StringArgs>,
    pub action: Option<StringArgs>,
    pub username: Option<String>,
    pub count: Option<i64>,
    pub page: Option<i64>,
}
