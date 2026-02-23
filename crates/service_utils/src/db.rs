use diesel::{
    PgConnection,
    r2d2::{ConnectionManager, Pool},
};

pub mod utils;

pub type PgSchemaConnectionPool = Pool<ConnectionManager<PgConnection>>;

/// Helper macro to run a database query with connection management and error handling.
/// Supports both raw connection mode and transaction mode.
/// Example usage:
/// ```rust,ignore
/// // Using raw connection mode
/// run_query!(db_pool, conn, {
///     // Your query logic here, using `conn` as the database connection
/// });
/// // Using transaction mode
/// run_query!(db_pool, tx conn, {
///     // Your query logic here, using `conn` as the database connection within a transaction
/// });
/// ```
#[macro_export]
macro_rules! run_query {
    // Public API — raw connection mode
    ($db_pool:expr, $conn:ident, $body:expr) => {
        run_query!(@execute $db_pool, raw, $conn, $body)
    };

    // Public API — transaction mode
    ($db_pool:expr, tx $conn:ident, $body:expr) => {
        run_query!(@execute $db_pool, tx, $conn, $body)
    };

    // Shared connection acquisition and execution logic
    (@execute $db_pool:expr, $mode:ident, $conn:ident, $body:expr) => {{
        match $db_pool.get() {
            Ok(mut $conn) => {
                diesel::Connection::set_prepared_statement_cache_size(
                    &mut $conn,
                    diesel::connection::CacheSize::Disabled,
                );

                run_query!(@dispatch $mode, $conn, $body)
            }
            Err(e) => Err(
                superposition_macros::unexpected_error!(
                    "Unable to get db connection from pool, error: {}", e
                ),
            ),
        }
    }};

    // Dispatch logic based on execution mode
    (@dispatch raw, $conn:ident, $body:expr) => {{
        let $conn = &mut $conn;
        $body.map_err(Into::into)
    }};


    // For transaction mode, we wrap the body in a transaction block
    (@dispatch tx, $conn:ident, $body:expr) => {
        diesel::Connection::transaction(&mut $conn, |$conn| $body).map_err(Into::into)
    };
}
