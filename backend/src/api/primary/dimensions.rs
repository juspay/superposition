use actix_web:: {
    get,
    post,
    error::ResponseError,
    web::{Path, Json, Data},
    HttpResponse,
    http::{header::ContentType, StatusCode}
};
use crate::models::db_models::Dimension;
use strum_macros::{EnumString, Display};
use serde::{Serialize, Deserialize};

use crate::{
    messages::dimensions::{
        FetchDimensions,
        FetchDimension,
        CreateDimension
    }, AppState, DbActor};

use actix::Addr;
// Error codes and their implementation
#[derive(Debug, Display, EnumString)]
pub enum DimensionError {
    DimensionNotFound,
    BadRequest,
    FailedToCreateDimension,
    SomethingWentWrong,
    FailedToGetDimensions,
}


impl ResponseError for DimensionError{
    fn error_response(&self) -> HttpResponse<actix_web::body::BoxBody> {
        HttpResponse::build(self.status_code())
        .insert_header(ContentType::json())
        .body(self.to_string())
    }

    fn status_code(&self) -> StatusCode {
        match self {
            DimensionError::DimensionNotFound => StatusCode::NOT_FOUND,
            DimensionError::FailedToGetDimensions => StatusCode::FAILED_DEPENDENCY,
            DimensionError::SomethingWentWrong => StatusCode::INTERNAL_SERVER_ERROR,
            DimensionError::BadRequest => StatusCode::BAD_REQUEST,
            DimensionError::FailedToCreateDimension => StatusCode::FAILED_DEPENDENCY
        }
    }
}

// Get dimension table
#[get("")]
pub async fn get_dimensions(state: Data<AppState>) -> Result<Json<Vec<Dimension>>, DimensionError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db.send(FetchDimensions).await {
        Ok(Ok(result)) => Ok(Json(result)),
        Ok(Err(_)) => Err(DimensionError::DimensionNotFound),
        _ => Err(DimensionError::SomethingWentWrong)
    }

}

// Get request to fetch dimension from dimension name
#[derive(Deserialize, Serialize)]
pub struct Key {
    dimension: String,
}

#[get("/{dimension}")]
pub async fn get_dimension_key(state: Data<AppState>, params: Path<Key>) -> Result<Json<Dimension>, DimensionError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();
    let dimension_key = params.into_inner().dimension;

    match db.send(FetchDimension {dimension: dimension_key}).await {
        Ok(Ok(result)) => Ok(Json(result)),
        Ok(Err(_)) => Err(DimensionError::DimensionNotFound),
        _ => Err(DimensionError::SomethingWentWrong)
    }

}


// Post request to add key, value to dimension table
#[derive(Deserialize, Serialize, Clone)]
pub struct KeyValue {
    dimension: String,
    priority: i32,
}

#[post("")]
pub async fn post_dimension(state: Data<AppState>, body: Json<KeyValue>) -> Result<Json<Dimension>, DimensionError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db.send(CreateDimension {
        dimension: body.dimension.clone(),
        priority: body.priority
    }).await {

        Ok(Ok(result)) => Ok(Json(result)),
        _ => Err(DimensionError::FailedToCreateDimension)
    }

}

