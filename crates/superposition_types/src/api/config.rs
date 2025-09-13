use crate::custom_query::CommaSeparatedStringQParams;
use chrono::{DateTime, Duration, Utc};
use serde::Deserialize;
use serde_json::{Map, Value};

#[derive(Deserialize)]
pub struct ContextPayload {
    pub context: Map<String, Value>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct AuditQueryFilters {
    pub from_date: Option<DateTime<Utc>>,
    pub to_date: Option<DateTime<Utc>>,
    pub table: Option<CommaSeparatedStringQParams>,
    pub action: Option<CommaSeparatedStringQParams>,
    pub username: Option<String>,
    pub count: Option<i64>,
    pub page: Option<i64>,
}

impl Default for AuditQueryFilters {
    fn default() -> Self {
        Self {
            action: Some("INSERT,UPDATE,DELETE".parse().unwrap()),
            from_date: Some(Utc::now() - Duration::days(30)),
            to_date: None,
            table: None,
            count: Some(10),
            page: Some(1),
            username: None,
        }
    }
}
