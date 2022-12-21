
use actix::Message;
use crate::models::db_models::Contexts;
use diesel::QueryResult;
use serde_json::Value;

#[derive(Message)]
#[rtype(result="QueryResult<Contexts>")]
pub struct CreateContext {
    pub key: String,
    pub value: Value,
}

#[derive(Message)]
#[rtype(result="QueryResult<Contexts>")]
pub struct FetchContext {
    pub key: String,
}

#[derive(Message)]
#[rtype(result="QueryResult<Contexts>")]
pub struct DeleteContext {
    pub key: String,
}
