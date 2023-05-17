
use actix::Message;
use crate::models::db_models::Contexts;
use diesel::QueryResult;

#[derive(Message)]
#[rtype(result="QueryResult<Contexts>")]
pub struct CreateContext {
    pub key: String,
    pub value: String,
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
