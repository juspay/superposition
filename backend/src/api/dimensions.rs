use actix_web:: {
    get,
    post,
    error::ResponseError,
    web::Path,
    web::Json, 
    HttpResponse, 
    http::{header::ContentType, StatusCode}
};
use crate::model::dimensions::Dimension;
use strum_macros::{EnumString, Display};
use serde::{Serialize, Deserialize};

// Error codes and their implementation 
#[derive(Debug, Display, EnumString)]
pub enum DimensionError {
    DimensionNotFound,
    BadRequest,
    FailedToAddDimension,
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
            DimensionError::FailedToAddDimension => StatusCode::FAILED_DEPENDENCY
        }
    }
}

// Get global whole global config 
#[get("")]
pub async fn get_config() -> Result<Json<Vec<Dimension>>, DimensionError> {
    return Err(DimensionError::FailedToGetDimensions)

}

// Get request to fetch value for given key
#[derive(Deserialize, Serialize)]
pub struct Key {
    dimension: String,
}

#[get("/{dimension}")]
pub async fn get_key(_params: Path<Key>) -> Result<Json<Dimension>, DimensionError> {
    return Ok(Json(Dimension::new("something".to_string(),2)));    
}


// Post request to add key, value
#[derive(Deserialize, Serialize)]
pub struct KeyValue {
    dimension: String,
    priority: i32,
}

#[post("")]
pub async fn post_config_key_value(_body: Json<KeyValue>) -> Result<Json<Dimension>, DimensionError> {
    return Err(DimensionError::FailedToAddDimension);

}

