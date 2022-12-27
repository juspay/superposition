use serde::Serialize;
use actix_web::{error::ResponseError, http::StatusCode, HttpResponse, Either::{Left, Right}};
use actix_web::Either;
use serde_json::{Value, Error, to_value};
use std::fmt;

#[derive(Debug, Clone, Serialize)]
pub enum AppErrorType {
    DataExists,
    DBError,
    NotFound,
    SomethingWentWrong,
    BadRequest
}

#[derive(Debug)]
pub struct AppError {
    pub message: Option<Either<String, Value>>,
    pub cause: Option<String>,
    pub status: AppErrorType,
}

#[derive(Serialize)]
pub struct ErrorResponse {
    status: AppErrorType,
    message: Option<Value>,
    cause: String,
}

impl AppError {
    fn message(&self) -> Result<Value, Error> {
        match &*self {
            AppError {message, ..} => match message {
                Some(Left(val)) => to_value(val.clone()),
                Some(Right(val)) => Ok(val.clone()),
                _ => to_value("An unexpected error has occured".to_string()),
            }
        }
    }

    fn cause(&self) -> String {
        match &*self {
            AppError { cause: Some(cause), ..} => cause.clone(),
            _ => "Reason not available".to_string()
        }
    }
}

impl fmt::Display for AppError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error>{
        write!(f, "{:?}", self)
    }
}

impl ResponseError for AppError {
    fn status_code(&self) -> StatusCode {
        match self.status {
            AppErrorType::SomethingWentWrong => StatusCode::INTERNAL_SERVER_ERROR,
            AppErrorType::DBError => StatusCode::INTERNAL_SERVER_ERROR,
            AppErrorType::NotFound => StatusCode::NOT_FOUND,
            AppErrorType::BadRequest => StatusCode::BAD_REQUEST,
            AppErrorType::DataExists => StatusCode::ALREADY_REPORTED,
        }
    }

    fn error_response(&self) -> HttpResponse {
        let status_code = self.status_code();

        let json_body = ErrorResponse
            { message: self.message().ok()
            , cause: self.cause()
            , status: self.status.clone()
            };

        HttpResponse::build(status_code).json(json_body)
    }
}

