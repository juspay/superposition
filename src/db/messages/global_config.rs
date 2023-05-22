use crate::db::models::db_models::GlobalConfig;
use actix::Message;
use diesel::QueryResult;
use serde_json::Value;

#[derive(Message)]
#[rtype(result = "QueryResult<Vec<GlobalConfig>>")]
pub struct FetchGlobalConfig;

#[derive(Message)]
#[rtype(result = "QueryResult<GlobalConfig>")]
pub struct FetchConfigKey {
    pub key: String,
}

#[derive(Message)]
#[rtype(result = "QueryResult<GlobalConfig>")]
pub struct CreateGlobalKey {
    pub key: String,
    pub value: Value,
}
