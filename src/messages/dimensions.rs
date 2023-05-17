use crate::models::db_models::Dimension;
use actix::Message;
use diesel::QueryResult;

#[derive(Message)]
#[rtype(result = "QueryResult<Vec<Dimension>>")]
pub struct FetchDimensions;


#[derive(Message)]
#[rtype(result = "QueryResult<Dimension>")]
pub struct FetchDimension {
    pub dimension: String
}

#[derive(Message)]
#[rtype(result = "QueryResult<Dimension>")]
pub struct CreateDimension {
    pub dimension: String,
    pub priority: i32
}
