use crate::db::schema::{
    config_versions, contexts, default_configs, dimensions, event_log, functions,
};
use chrono::{offset::Utc, DateTime, NaiveDateTime};
use diesel::{AsChangeset, Insertable, Queryable, Selectable};
use serde::Serialize;
use serde_json::Value;

#[derive(Queryable, Selectable, Insertable, AsChangeset, Clone, Serialize, Debug)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(id))]
pub struct Context {
    pub id: String,
    pub value: Value,
    pub override_id: String,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub priority: i32,
    #[serde(rename(serialize = "override"))]
    pub override_: Value,
}

#[derive(Queryable, Selectable, Insertable, AsChangeset, Serialize)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(dimension))]
#[diesel(treat_none_as_null = true)]
pub struct Dimension {
    pub dimension: String,
    pub priority: i32,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
    pub function_name: Option<String>,
}

#[derive(Queryable, Selectable, Insertable, AsChangeset, Serialize, Clone)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(key))]
#[diesel(treat_none_as_null = true)]
pub struct DefaultConfig {
    pub key: String,
    pub value: Value,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
    pub schema: Value,
    pub function_name: Option<String>,
}

#[derive(Queryable, Selectable, Insertable, AsChangeset, Serialize, Clone, Debug)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(name))]
pub struct Function {
    pub function_name: String,
    pub published_code: Option<String>,
    pub draft_code: String,
    pub function_description: String,
    pub published_runtime_version: Option<String>,
    pub draft_runtime_version: String,
    pub published_at: Option<NaiveDateTime>,
    pub draft_edited_at: NaiveDateTime,
    pub published_by: Option<String>,
    pub draft_edited_by: String,
}

#[derive(Queryable, Selectable, Insertable, Serialize, Clone, Debug)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(table_name = event_log)]
#[diesel(primary_key(id))]
pub struct EventLog {
    pub id: uuid::Uuid,
    pub table_name: String,
    pub user_name: String,
    pub timestamp: NaiveDateTime,
    pub action: String,
    pub original_data: Option<Value>,
    pub new_data: Option<Value>,
    pub query: String,
}

#[derive(Queryable, Selectable, Insertable, Serialize, Clone, Debug)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(id))]
pub struct ConfigVersion {
    pub id: i64,
    pub config: Value,
    pub config_hash: String,
    pub tags: Option<Vec<String>>,
    pub created_at: NaiveDateTime,
}
