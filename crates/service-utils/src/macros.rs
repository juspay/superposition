#[macro_export]
macro_rules! bad_argument {
    ($msg: literal, $($args: tt)*) => {
        service_utils::result::AppError::BadArgument(format!($msg, $($args)*))
    };
    ($err: tt) => {
        service_utils::result::AppError::BadArgument($err.to_string())
    };
}

#[macro_export]
macro_rules! validation_error {
    ($msg: literal, $($args: tt)*) => {
        service_utils::result::AppError::ValidationError(format!($msg, $($args)*))
    };
    ($err: tt) => {
        service_utils::result::AppError::ValidationError($err.to_string())
    };
}

#[macro_export]
macro_rules! unexpected_error {
    ($msg: literal, $($args: tt)*) => {
        service_utils::result::AppError::UnexpectedError(anyhow::anyhow!(format!($msg, $($args)*)))
    };
    ($err: tt) => {
        service_utils::result::AppError::UnexpectedError(anyhow::anyhow!($err.to_string()))
    };
}

#[macro_export]
macro_rules! not_found {
    ($msg: literal, $($args: tt)*) => {
        service_utils::result::AppError::NotFound(format!($msg, $($args)*))
    };
    ($err: tt) => {
        service_utils::result::AppError::NotFound($err.to_string())
    };
}

#[macro_export]
macro_rules! db_error {
    ($error: expr) => {
        service_utils::result::AppError::DbError($error)
    };
}

#[macro_export]
macro_rules! response_error {
    ($status: expr, $msg: expr) => {
        service_utils::result::AppError::ResponseError(
            service_utils::result::ResponseError {
                status_code: $status,
                message: $msg.to_string(),
            },
        )
    };
}
