
use actix::Message;
use crate::models::db_models::CtxOverrides;
use diesel::QueryResult;

#[derive(Message)]
#[rtype(result="QueryResult<CtxOverrides>")]
pub struct CreateCtxOverrides {
    pub context_id: String,
    pub override_id: String
}

#[derive(Message)]
#[rtype(result="QueryResult<Vec<CtxOverrides>>")]
pub struct FetchAllCtxOverrides;

#[derive(Message)]
#[rtype(result="QueryResult<CtxOverrides>")]
pub struct FetchCtxOverrides {
    pub context_id: String,
}

#[derive(Message)]
#[rtype(result="QueryResult<CtxOverrides>")]
pub struct DeleteCtxOverrides {
    pub context_id: String,
}