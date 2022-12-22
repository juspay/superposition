
use actix::Message;
use crate::models::db_models::CtxOverrides;
use diesel::QueryResult;

#[derive(Message)]
#[rtype(result="QueryResult<CtxOverrides>")]
pub struct CreateCtxOverrides {
    pub key: String,
    pub context_id: String,
    pub override_id: String
}

#[derive(Message)]
#[rtype(result="QueryResult<CtxOverrides>")]
pub struct FetchCtxOverrides {
    pub key: String,
}

#[derive(Message)]
#[rtype(result="QueryResult<CtxOverrides>")]
pub struct DeleteCtxOverrides {
    pub key: String,
}