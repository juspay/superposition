
use actix::Message;
use crate::models::db_models::Overrides;
use diesel::QueryResult;
use serde_json::Value;

#[derive(Message)]
#[rtype(result="QueryResult<Overrides>")]
pub struct CreateOverride {
    pub key: String,
    pub value: Value,
}

#[derive(Message)]
#[rtype(result="QueryResult<Vec<Overrides>>")]
pub struct FetchAllOverrides;

#[derive(Message)]
#[rtype(result="QueryResult<Overrides>")]
pub struct FetchOverride {
    pub key: String,
}

#[derive(Message)]
#[rtype(result="QueryResult<Overrides>")]
pub struct DeleteOverride {
    pub key: String,
}
