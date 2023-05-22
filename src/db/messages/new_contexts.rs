use crate::db::models::db_models::NewContexts;
use actix::Message;
use diesel::QueryResult;
use serde_json::Value;

#[derive(Message)]
#[rtype(result = "QueryResult<NewContexts>")]
pub struct CreateNewContext {
    pub key: String,
    pub value: Value,
    pub column1: Option<String>,
    pub column2: Option<String>,
    pub column3: Option<String>,
    pub column4: Option<String>,
}

#[derive(Message)]
#[rtype(result = "QueryResult<Vec<NewContexts>>")]
pub struct FetchAllNewContexts;

#[derive(Message)]
#[rtype(result = "QueryResult<Vec<NewContexts>>")]
pub struct FetchNewContext {
    pub column1: Option<String>,
    pub column2: Option<String>,
    pub column3: Option<String>,
    pub column4: Option<String>,
}

#[derive(Message)]
#[rtype(result = "QueryResult<NewContexts>")]
pub struct DeleteNewContext {
    pub key: String,
}
