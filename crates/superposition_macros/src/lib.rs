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
macro_rules! webhook_error {
    ($msg: literal, $($args: tt)*) => {
        superposition_types::result::AppError::WebhookError(anyhow::anyhow!(format!($msg, $($args)*)))
    };
    ($err: tt) => {
        superposition_types::result::AppError::WebhookError(anyhow::anyhow!($err.to_string()))
    };
}

#[macro_export]
// Macro to create a nested tuple from a list of expressions
macro_rules! nested_pair_tuple {
    ($first:expr $(, $rest:expr)* $(,)?) => {
        ($first, nested_pair_tuple!($($rest),*))
    };
    () => {
        ()
    };
}
