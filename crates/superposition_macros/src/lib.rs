#![deny(unused_crate_dependencies)]
#[macro_export]
macro_rules! bad_argument {
    ($msg: literal, $($args: tt)*) => {
        superposition_types::result::AppError::BadArgument(format!($msg, $($args)*))
    };
    ($err: tt) => {
        superposition_types::result::AppError::BadArgument($err.to_string())
    };
}

#[macro_export]
macro_rules! validation_error {
    ($msg: literal, $($args: tt)*) => {
        superposition_types::result::AppError::ValidationError(format!($msg, $($args)*))
    };
    ($err: tt) => {
        superposition_types::result::AppError::ValidationError($err.to_string())
    };
}

#[macro_export]
macro_rules! unexpected_error {
    ($msg: literal, $($args: tt)*) => {
        superposition_types::result::AppError::UnexpectedError(anyhow::anyhow!(format!($msg, $($args)*)))
    };
    ($err: tt) => {
        superposition_types::result::AppError::UnexpectedError(anyhow::anyhow!($err.to_string()))
    };
}

#[macro_export]
macro_rules! not_found {
    ($msg: literal, $($args: tt)*) => {
        superposition_types::result::AppError::NotFound(format!($msg, $($args)*))
    };
    ($err: tt) => {
        superposition_types::result::AppError::NotFound($err.to_string())
    };
}

#[macro_export]
macro_rules! db_error {
    ($error: expr) => {
        superposition_types::result::AppError::DbError($error)
    };
}

#[macro_export]
macro_rules! response_error {
    ($status: expr, $msg: expr) => {
        superposition_types::result::AppError::ResponseError(
            superposition_types::result::ResponseError {
                status_code: $status,
                message: $msg.to_string(),
            },
        )
    };
}

#[macro_export]
macro_rules! box_params {
    ($($param:expr),* $(,)?) => {
        vec![
            $(Box::new($param),)*
        ]
    };
}
