use diesel::Insertable;
use serde::Serialize;
use serde_json::Value;

use crate::db::schema::overrides;

#[derive(Debug, Insertable, Serialize)]
#[diesel(table_name = overrides)]
pub struct OverrideInsertion {
    pub key : String,
    pub value : Value,
}