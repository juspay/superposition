use crate::db::PgSchemaConnectionPool;
use diesel::Connection;
use superposition_macros::unexpected_error;
use superposition_types::result as superposition;
use types::DbConnection;

pub mod types;

pub fn get_db_connection(
    db_pool: PgSchemaConnectionPool,
) -> superposition::Result<DbConnection> {
    match db_pool.get() {
        Ok(mut conn) => {
            conn.set_prepared_statement_cache_size(
                diesel::connection::CacheSize::Disabled,
            );
            Ok(DbConnection(conn))
        }
        Err(e) => {
            log::info!("Unable to get db connection from pool, error: {e}");
            Err(unexpected_error!("Could not get a DB connection, contact an admin and check logs for further information"))
        }
    }
}
