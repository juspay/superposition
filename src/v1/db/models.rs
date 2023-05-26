use crate::v1::db::schema::*;
use chrono::offset::Utc;
use chrono::DateTime;
use diesel::{Insertable, Queryable, Selectable};
use serde_json::Value;

#[derive(Queryable, Selectable, Insertable)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(id))]
pub struct Context {
    pub id: String,
    pub value: Value,
    pub override_id: String,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
}

#[derive(Queryable, Selectable, Insertable)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(primary_key(id))]
pub struct Override {
    pub id: String,
    pub value: Value,
    pub created_at: DateTime<Utc>,
    pub created_by: String,
}
