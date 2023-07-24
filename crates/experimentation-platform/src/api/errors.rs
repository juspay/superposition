use actix_web::{
    error,
    http::{header::ContentType, StatusCode},
    web::Json,
    HttpResponse,
};
use derive_more::{Display, Error};
use serde::{Deserialize, Serialize};

#[derive(Debug, Error, Display)]
#[display(fmt = "experiment error: {}", message)]
pub struct AppError {
    pub message: String,
    pub possible_fix: String,
    pub status_code: StatusCode,
}

#[derive(Deserialize, Serialize)]
struct ErrorResponse {
    message: String,
    possible_fix: String,
}

impl error::ResponseError for AppError {
    fn error_response(&self) -> HttpResponse {
        HttpResponse::build(self.status_code)
            .insert_header(ContentType::json())
            .json(Json(ErrorResponse {
                message: self.message.to_string(),
                possible_fix: self.possible_fix.to_string(),
            }))
    }
}
