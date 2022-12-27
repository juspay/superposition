use serde::Serialize;
use actix_web::{error::ResponseError, http::StatusCode, HttpResponse};
use std::fmt;

#[derive(Debug, Clone, Serialize)]
pub enum AppErrorType {
    DBError,
    NotFound,
    SomethingWentWrong,
}

#[derive(Debug)]
pub struct AppError {
    pub message: Option<String>,
    pub cause: Option<String>,
    pub status: AppErrorType
}

#[derive(Serialize)]
pub struct ErrorResponse {
    pub message: String,
    pub cause: String,
    pub status: AppErrorType
}

impl AppError {


    fn message(&self) -> String {
        match &*self {
            AppError {message: Some(message), cause: _, status: _} => message.clone(),
            _ => "An unexpected error has occured".to_string()
        }
    }

    fn cause(&self) -> String {
        match &*self {
            AppError { message: _, cause: Some(cause), status: _ } => cause.clone(),
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
            AppErrorType::NotFound => StatusCode::NOT_FOUND
        }
    }

    fn error_response(&self) -> HttpResponse {
        HttpResponse::build(self.status_code())
            .json(ErrorResponse {message: self.message(), cause: self.cause(), status: self.status.clone()})
    }
}

