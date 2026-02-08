use diesel::{
    PgConnection,
    r2d2::{ConnectionManager, Pool},
};

pub mod utils;

pub type PgSchemaConnectionPool = Pool<ConnectionManager<PgConnection>>;

#[macro_export]
macro_rules! run_query {
    ($db_pool:expr, $query_fn:expr) => {
        match $db_pool.get() {
            Ok(mut conn) => {
                diesel::Connection::set_prepared_statement_cache_size(
                    &mut conn,
                    diesel::connection::CacheSize::Disabled,
                );
                diesel::Connection::transaction(&mut conn, |transaction_conn| {
                    $query_fn(transaction_conn).map_err(Into::into)
                })
            }
            Err(e) => Err(superposition_types::result::AppError::UnexpectedError(
                anyhow::anyhow!("Unable to get db connection from pool, error: {e}"),
            )),
        }
    };
}
