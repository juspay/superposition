use crate::v1::db::schema::cac_v1::*;
use chrono::offset::Utc;
use chrono::DateTime;
use diesel::{AsChangeset, Insertable, Queryable, Selectable};
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Queryable, Selectable, Insertable, Clone, Serialize, Debug)]
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

#[derive(Debug, Clone, Copy, diesel_derive_enum::DbEnum, Deserialize, Serialize)]
#[ExistingTypePath = "crate::v1::db::schema::cac_v1::sql_types::DimensionType"]
#[DbValueStyle = "UPPERCASE"]
#[ExistingTypePath = "crate::v1::db::schema::sql_types::DimensionType"]
pub enum DimensionType {
    BOOL,
    NUMBER,
    STRING,
    ARRAY,
    OBJECT,
}

#[derive(Queryable, Selectable, Insertable, AsChangeset)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(dimension))]
pub struct Dimension {
    pub dimension: String,
    pub priority: i32,
    pub type_: DimensionType,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
}

#[derive(Queryable, Selectable, Insertable, AsChangeset)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(key))]
pub struct DefaultConfig {
    pub key: String,
    pub value: Value,
    pub schema: Value,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
}
