use diesel::{
    PgConnection,
    r2d2::{ConnectionManager, Pool},
};
use superposition_types::{DBConnection, result};

pub mod utils;

pub type PgSchemaConnectionPool = Pool<ConnectionManager<PgConnection>>;

/// Helper macro to run a database query with connection management and error handling.
/// Example usage:
/// ```rust,ignore
/// run_query!(db_pool, conn, {
///     // Your query logic here, using `conn` as the database connection
/// });
/// ```
#[macro_export]
macro_rules! run_query {
    ($db_pool:expr, $conn:ident, $body:expr) => {{
        let mut $conn = $db_pool.get().map_err(|e| {
            superposition_macros::unexpected_error!(
                "Unable to get db connection from pool, error: {}",
                e
            )
        })?;
        diesel::Connection::set_prepared_statement_cache_size(
            &mut $conn,
            diesel::connection::CacheSize::Disabled,
        );

        $body
    }};
}

/// Helper function to run a database transaction with connection management and error handling.
/// Example usage:
/// ```rust,ignore
/// run_transaction(&db_pool, |conn| {
///     // Your transactional query logic here, using `conn` as the database
///     // connection within the transaction
///     Ok(result) // Return a result from the transaction block
/// });
/// ```
pub fn run_transaction<F, T>(
    db_pool: &PgSchemaConnectionPool,
    query_fn: F,
) -> result::Result<T>
where
    F: FnOnce(&mut DBConnection) -> result::Result<T>,
{
    let mut conn = db_pool.get().map_err(|e| {
        superposition_macros::unexpected_error!(
            "Unable to get db connection from pool, error: {}",
            e
        )
    })?;
    diesel::Connection::set_prepared_statement_cache_size(
        &mut conn,
        diesel::connection::CacheSize::Disabled,
    );

    diesel::Connection::transaction(&mut conn, query_fn)
}
