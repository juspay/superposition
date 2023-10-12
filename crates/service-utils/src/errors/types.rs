use actix_web::{
    error,
    http::{header::ContentType, StatusCode},
    web::Json,
    HttpResponse,
};
use derive_more::{Display, Error};
use serde::{Deserialize, Serialize};
use std::error as err;

#[derive(Debug, Error, Display, Clone)]
#[display(
    fmt = "server returned an error: {} with status code {}",
    message,
    status_code
)]
pub struct ServerError {
    pub message: String,
    pub possible_fix: String,
    pub status_code: StatusCode,
}

impl error::ResponseError for ServerError {
    fn error_response(&self) -> HttpResponse {
        HttpResponse::build(self.status_code)
            .insert_header(ContentType::json())
            .json(Json(ErrorResponse {
                message: self.message.to_string(),
                possible_fix: self.possible_fix.to_string(),
            }))
    }
}

#[derive(Deserialize, Serialize, Debug, Display, Error, Clone)]
#[display(
    fmt = "error sent to client {} that can be fixed by {}",
    message,
    possible_fix
)]
pub struct ErrorResponse {
    pub message: String,
    pub possible_fix: String,
}

#[derive(Debug, Display, Clone)]
pub enum Error {
    #[display(fmt = "Connection failed to {}, reason: {}", "_0", "_1")]
    ConnectionFailed(String, String),
    #[display(fmt = "Error occured with postgres: {}", "_0")]
    DB(String),
    #[display(fmt = "The resource(s) requested were not found: {}", "_0")]
    NotFound(ErrorResponse),
    #[display(fmt = "Bad Request: {}", "_0")]
    BadRequest(ErrorResponse),
    #[display(fmt = "Bad Arugment: {}", "_0")]
    BadArgument(ErrorResponse),
    #[display(fmt = "Something went wrong, reason: {}", "_0")]
    InternalServerErr(String),
    #[display(fmt = "{}", "_0")]
    Generic(ServerError),
}

impl err::Error for Error {}

impl From<diesel::result::Error> for Error {
    fn from(value: diesel::result::Error) -> Self {
        log::error!("{}", value);
        match value {
            diesel::result::Error::InvalidCString(e) => {
                Self::ConnectionFailed("DATABASE".into(), e.to_string())
            }
            diesel::result::Error::DatabaseError(kind, error) => Self::DB(format!(
                "Database error of kind {:?} occurred due to {}",
                kind,
                error.message()
            )),
            diesel::result::Error::NotFound => Self::NotFound(ErrorResponse {
                message: "No records found".into(),
                possible_fix: "Please refine or correct your search parameters".into(),
            }),
            diesel::result::Error::DeserializationError(_) => {
                Self::BadRequest(ErrorResponse {
                    message: "The server could not understand your request".into(),
                    possible_fix: "Please check your request for issues".into(),
                })
            }
            diesel::result::Error::SerializationError(_) => {
                Self::BadRequest(ErrorResponse {
                    message: "The server could not understand your request".into(),
                    possible_fix: "Please check your request for issues".into(),
                })
            }
            e => Self::InternalServerErr(format!("What went wrong: {:?}", e)),
        }
    }
}

impl Error {
    fn generate_err_response(
        code: StatusCode,
        message: String,
        possible_fix: String,
    ) -> HttpResponse {
        HttpResponse::build(code)
            .insert_header(ContentType::json())
            .json(Json(ErrorResponse {
                message,
                possible_fix,
            }))
    }
}

impl error::ResponseError for Error {
    fn error_response(&self) -> HttpResponse {
        let please_try_again: String = String::from("Please try again later");
        log::error!("{}", self);
        match self {
            Error::ConnectionFailed(_, _) => Self::generate_err_response(
                StatusCode::SERVICE_UNAVAILABLE,
                "Something went wrong".into(),
                please_try_again,
            ),
            Error::DB(_) => Self::generate_err_response(
                StatusCode::INTERNAL_SERVER_ERROR,
                "Something went wrong".into(),
                please_try_again,
            ),
            Error::NotFound(reason) => Self::generate_err_response(
                StatusCode::NOT_FOUND,
                reason.message.clone(),
                reason.possible_fix.clone(),
            ),
            Error::BadRequest(cause) => Self::generate_err_response(
                StatusCode::BAD_REQUEST,
                cause.message.clone(),
                cause.possible_fix.clone(),
            ),
            Error::BadArgument(cause) => Self::generate_err_response(
                StatusCode::BAD_REQUEST,
                cause.message.clone(),
                cause.possible_fix.clone(),
            ),
            Error::InternalServerErr(_) => Self::generate_err_response(
                StatusCode::INTERNAL_SERVER_ERROR,
                "Something went wrong".into(),
                please_try_again,
            ),
            Error::Generic(server_error) => server_error.error_response(),
        }
    }
}
