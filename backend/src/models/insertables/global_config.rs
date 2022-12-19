use crate::db::schema::global_config;
use diesel::Insertable;
use serde::Serialize;
use serde_json::Value;

#[derive(Insertable, Serialize, Clone)]
#[diesel(table_name=global_config)]
pub struct NewGlobalConfigKey {
    pub key: String,
    pub value: Value,
}
