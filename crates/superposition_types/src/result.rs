use actix_web::{
    error,
    http::{header::ContentType, StatusCode},
    HttpResponse,
};
use derive_more::Display;
use serde::{Deserialize, Serialize};
use thiserror::Error as this_error;

#[derive(this_error)]
pub enum AppError {
    #[error("validation failed ( `{0}` )")]
    ValidationError(String),
    #[error("bad arguments ( `{0}` )")]
    BadArgument(String),
    #[error("not found ( `{0}` )")]
    NotFound(String),
    #[error(transparent)]
    DbError(#[from] diesel::result::Error),
    #[error(transparent)]
    ResponseError(#[from] ResponseError),
    #[error(transparent)]
    UnexpectedError(anyhow::Error),
    #[error("webhook error ( `{0}` )")]
    WebhookError(String),
}

#[derive(Debug, this_error, Display, Clone)]
#[display(
    fmt = "server returned an error: {} with status code {}",
    message,
    status_code
)]
pub struct ResponseError {
    pub message: String,
    pub status_code: StatusCode,
}

#[derive(Debug, Clone, Serialize, Deserialize, Display)]
pub struct ErrorResponse {
    pub message: String,
}

pub type Result<T> = core::result::Result<T, AppError>;

impl AppError {
    fn generate_err_response(code: StatusCode, msg: &str) -> HttpResponse {
        let response = ErrorResponse {
            message: msg.into(),
        };
        HttpResponse::build(code)
            .insert_header(ContentType::json())
            .json(response)
    }
    pub fn message(&self) -> String {
        use diesel::result::DatabaseErrorKind as diesel_error_kind;
        use diesel::result::Error as diesel_error;
        match self {
            AppError::ValidationError(msg)
            | AppError::BadArgument(msg)
            | AppError::NotFound(msg) => msg.to_owned(),
            AppError::UnexpectedError(_) => String::from("Something went wrong"),
            AppError::ResponseError(error) => error.message.clone(),
            AppError::DbError(diesel_error::InvalidCString(_)) => {
                String::from("Something went wrong")
            }
            AppError::DbError(diesel_error::DatabaseError(
                diesel_error_kind::UniqueViolation
                | diesel_error_kind::CheckViolation
                | diesel_error_kind::NotNullViolation
                | diesel_error_kind::ForeignKeyViolation,
                error,
            )) => error.message().to_owned(),
            AppError::DbError(_) => String::from("Something went wrong"),
            AppError::WebhookError(error) => error.to_string(),
        }
    }
}

impl error::ResponseError for AppError {
    fn error_response(&self) -> HttpResponse {
        use diesel::result::DatabaseErrorKind as diesel_error_kind;
        use diesel::result::Error as diesel_error;

        log::error!("{}", self);
        match self {
            AppError::ValidationError(msg) | AppError::BadArgument(msg) => {
                Self::generate_err_response(StatusCode::BAD_REQUEST, msg)
            }
            AppError::NotFound(msg) => {
                Self::generate_err_response(StatusCode::NOT_FOUND, msg)
            }
            AppError::UnexpectedError(_) => Self::generate_err_response(
                StatusCode::INTERNAL_SERVER_ERROR,
                "Something went wrong",
            ),
            AppError::ResponseError(error) => {
                Self::generate_err_response(error.status_code, &error.message)
            }

            AppError::DbError(diesel_error::InvalidCString(_)) => {
                Self::generate_err_response(
                    StatusCode::SERVICE_UNAVAILABLE,
                    "Something went wrong",
                )
            }

            AppError::DbError(diesel_error::NotFound) => Self::generate_err_response(
                StatusCode::NOT_FOUND,
                "No records found. Please refine or correct your search parameters",
            ),

            AppError::DbError(diesel_error::DatabaseError(
                diesel_error_kind::UniqueViolation
                | diesel_error_kind::CheckViolation
                | diesel_error_kind::NotNullViolation
                | diesel_error_kind::ForeignKeyViolation,
                error,
            )) => Self::generate_err_response(StatusCode::CONFLICT, error.message()),

            AppError::DbError(_) => Self::generate_err_response(
                StatusCode::INTERNAL_SERVER_ERROR,
                "Something went wrong",
            ),

            AppError::WebhookError(error) => Self::generate_err_response(
                StatusCode::from_u16(512).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR),
                error,
            ),
        }
    }
}

fn error_chain_fmt(
    e: &dyn std::error::Error,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    writeln!(f, "{}\n", e)?;
    let mut current = e.source();
    while let Some(cause) = current {
        writeln!(f, "Caused by:\n\t{}", cause)?;
        current = cause.source();
    }
    Ok(())
}

impl std::fmt::Debug for AppError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        error_chain_fmt(self, f)
    }
}
