use chrono::NaiveDateTime;
use serde::Deserialize;
use service_utils::helpers::deserialize_stringified_list;

#[derive(Deserialize, Debug, Clone)]
pub struct StringArgs(
    #[serde(deserialize_with = "deserialize_stringified_list")] pub Vec<String>,
);

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
