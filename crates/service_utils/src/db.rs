use diesel::{
    PgConnection,
    r2d2::{ConnectionManager, Pool},
};
use superposition_types::{DBConnection, result};

pub mod utils;

pub type PgSchemaConnectionPool = Pool<ConnectionManager<PgConnection>>;

// this should not be made public, instead we should have helper functions that use
// this internally to run queries/transactions with proper error handling and connection management
pub fn checkout_connection(
    db_pool: &PgSchemaConnectionPool,
) -> result::Result<DBConnection> {
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

    Ok(conn)
}

/// Helper function to run a database query with connection management and error handling.
/// Example usage:
/// ```rust,ignore
/// run_query(&db_pool, |conn| {
///    // Your query logic here, using `conn` as the database connection
/// });
/// ```
pub fn run_query<T, F>(db_pool: &PgSchemaConnectionPool, query_fn: F) -> result::Result<T>
where
    F: FnOnce(&mut DBConnection) -> Result<T, diesel::result::Error>,
{
    let mut conn = checkout_connection(db_pool)?;
    query_fn(&mut conn).map_err(Into::into)
}

/// Helper function to run a database transaction with connection management and error handling.
/// Example usage:
/// ```rust,ignore
/// run_transaction(&db_pool, |query_context| {
///     // Use query_context.run_query() for individual queries - they return diesel errors
///     // which need conversion to result::Result
///     let data = query_context.run_query(|conn| {
///         // Your diesel query here
///     })?;
///    // You can run multiple queries within the transaction using query_context.run_query()
///    Ok(data) // Return the final result of the transaction
/// ```
pub fn run_transaction<T, F>(
    db_pool: &PgSchemaConnectionPool,
    query_fn: F,
) -> result::Result<T>
where
    F: FnOnce(&mut DBConnection) -> result::Result<T>,
{
    let mut conn = checkout_connection(db_pool)?;
    diesel::Connection::transaction(&mut conn, query_fn)
}

/// A helper enum to represent the context in which a query is being run, either directly from a connection pool or within a transaction.
/// This allows us to abstract away the connection management and error handling logic when running queries or transactions.
/// When running a query (or a bunch of queries) without transaction semantics, we can use the `NonTransaction` variant to get a connection from the pool and run them directly.
/// When running a transaction, we can use the `Transaction` variant to run the query within the transaction context,
/// ensuring that all queries within the transaction are executed on the same connection and that the transaction is properly
/// committed or rolled back based on the success or failure of the queries.
pub enum QueryContext<'a> {
    Pooled(&'a PgSchemaConnectionPool),
    Pinned(&'a mut DBConnection),
}

pub enum QueryConnection<'a> {
    Pooled(DBConnection),
    Pinned(&'a mut DBConnection),
}

impl std::ops::Deref for QueryConnection<'_> {
    type Target = DBConnection;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Pooled(conn) => conn,
            Self::Pinned(conn) => conn,
        }
    }
}

impl std::ops::DerefMut for QueryConnection<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Pooled(conn) => conn,
            Self::Pinned(conn) => conn,
        }
    }
}

impl QueryContext<'_> {
    pub fn checkout(&mut self) -> result::Result<QueryConnection<'_>> {
        match self {
            QueryContext::Pooled(db_pool) => {
                Ok(QueryConnection::Pooled(checkout_connection(db_pool)?))
            }
            QueryContext::Pinned(conn) => Ok(QueryConnection::Pinned(conn)),
        }
    }

    pub fn run_query<T, F>(&mut self, query_fn: F) -> result::Result<T>
    where
        F: FnOnce(&mut DBConnection) -> Result<T, diesel::result::Error>,
    {
        match self {
            QueryContext::Pooled(db_pool) => run_query(db_pool, query_fn),
            QueryContext::Pinned(conn) => query_fn(conn).map_err(Into::into),
        }
    }

    pub fn transaction<T, F>(&mut self, query_fn: F) -> result::Result<T>
    where
        F: FnOnce(&mut DBConnection) -> result::Result<T>,
    {
        match self {
            QueryContext::Pooled(db_pool) => run_transaction(db_pool, query_fn),
            QueryContext::Pinned(conn) => {
                diesel::Connection::transaction(*conn, query_fn)
            }
        }
    }
}
