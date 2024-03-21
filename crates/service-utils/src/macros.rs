#[macro_export]
macro_rules! bad_argument {
    ($msg: literal) => {
        service_utils::result::AppError::BadArgument($msg.into())
    };
    ($msg: literal, $($args: tt)*) => {
        service_utils::result::AppError::BadArgument(format!($msg, $($args)*))
    };
    ($err: tt) => {
        service_utils::result::AppError::BadArgument($err.to_string())
    };
}

#[macro_export]
macro_rules! validation_error {
    ($msg: literal) => {
        service_utils::result::AppError::ValidationError($msg.into())
    };
    ($msg: literal, $($args: tt)*) => {
        service_utils::result::AppError::ValidationError(format!($msg, $($args)*))
    };
    ($err: tt) => {
        service_utils::result::AppError::ValidationError($err.to_string())
    };
}

#[macro_export]
macro_rules! unexpected_error {
    ($msg: literal) => {
        service_utils::result::AppError::UnexpectedError(anyhow::anyhow!($msg))
    };
    ($msg: literal, $($args: tt)*) => {
        service_utils::result::AppError::UnexpectedError(anyhow::anyhow!(format!($msg, $($args)*)))
    };
    ($err: tt) => {
        service_utils::result::AppError::UnexpectedError(anyhow::anyhow!($err.to_string()))
    };
}

#[macro_export]
macro_rules! db_error {
    ($error: expr) => {
        service_utils::result::AppError::DbError($error)
    };
}

#[macro_export]
macro_rules! server_error {
    ($status: expr, $msg: expr) => {
        service_utils::result::AppError::ServerError(service_utils::result::ServerError {
            status_code: $status,
            message: $msg,
        })
    };
}