use actix_web::web::Data;
use actix_web::{FromRequest, HttpMessage};
use derive_more::{Deref, DerefMut};
use diesel::connection::{
    AnsiTransactionManager, Connection, ConnectionSealed, DefaultLoadingMode,
    LoadConnection, SimpleConnection, TransactionManager,
};
use diesel::pg::{Pg, PgQueryBuilder};
use diesel::r2d2::{ConnectionManager, PooledConnection};
use diesel::PgConnection;
use diesel::RunQueryDsl;

use crate::service::types::{AppState, SchemaName};

pub struct TransactionManagerImpl;

impl TransactionManager<ConnectionImpl> for TransactionManagerImpl {
    type TransactionStateData = <AnsiTransactionManager as TransactionManager<
        PgConnection,
    >>::TransactionStateData;

    fn begin_transaction(conn: &mut ConnectionImpl) -> diesel::prelude::QueryResult<()> {
        AnsiTransactionManager::begin_transaction(&mut *conn.conn)?;
        let result = diesel::sql_query("SELECT set_config('search_path', $1, true)")
            .bind::<diesel::sql_types::Text, _>(&conn.namespace)
            .execute(&mut *conn.conn)?;
        log::info!("{:?}", result);
        Ok(())
    }

    fn rollback_transaction(
        conn: &mut ConnectionImpl,
    ) -> diesel::prelude::QueryResult<()> {
        AnsiTransactionManager::rollback_transaction(&mut *conn.conn)
    }

    fn commit_transaction(conn: &mut ConnectionImpl) -> diesel::prelude::QueryResult<()> {
        AnsiTransactionManager::commit_transaction(&mut *conn.conn)
    }

    fn transaction_manager_status_mut(
        conn: &mut ConnectionImpl,
    ) -> &mut diesel::connection::TransactionManagerStatus {
        AnsiTransactionManager::transaction_manager_status_mut(&mut *conn.conn)
    }
}

pub struct ConnectionImpl {
    namespace: String,
    conn: PooledConnection<ConnectionManager<PgConnection>>,
}

impl ConnectionImpl {
    pub fn new(
        namespace: String,
        mut conn: PooledConnection<ConnectionManager<PgConnection>>,
    ) -> Self {
        conn.set_prepared_statement_cache_size(diesel::connection::CacheSize::Disabled);
        ConnectionImpl { namespace, conn }
    }

    pub fn set_namespace(&mut self, namespace: String) {
        self.namespace = namespace;
    }

    pub fn from_request_override(
        req: &actix_web::HttpRequest,
        schema_name: String,
    ) -> Result<Self, actix_web::Error> {
        let app_state = match req.app_data::<Data<AppState>>() {
            Some(state) => state,
            None => {
                log::info!(
                    "DbConnection-FromRequest: Unable to get app_data from request"
                );
                return Err(actix_web::error::ErrorInternalServerError(""));
            }
        };

        match app_state.db_pool.get() {
            Ok(conn) => Ok(ConnectionImpl::new(schema_name, conn)),
            Err(e) => {
                log::info!("Unable to get db connection from pool, error: {e}");
                Err(actix_web::error::ErrorInternalServerError(""))
            }
        }
    }
}

impl ConnectionSealed for ConnectionImpl {}

impl SimpleConnection for ConnectionImpl {
    fn batch_execute(&mut self, query: &str) -> diesel::prelude::QueryResult<()> {
        self.conn.batch_execute(query)
    }
}

impl Connection for ConnectionImpl {
    type Backend = Pg;
    type TransactionManager = TransactionManagerImpl;

    // NOTE: this function will never be used, so namespace here doesn't matter
    fn establish(database_url: &str) -> diesel::prelude::ConnectionResult<Self> {
        let conn = PooledConnection::establish(database_url)?;
        Ok(ConnectionImpl {
            namespace: String::new(),
            conn,
        })
    }

    fn execute_returning_count<T>(
        &mut self,
        source: &T,
    ) -> diesel::prelude::QueryResult<usize>
    where
        T: diesel::query_builder::QueryFragment<Self::Backend>
            + diesel::query_builder::QueryId,
    {
        log::info!("{:?}", source.to_sql(&mut PgQueryBuilder::default(), &Pg));
        self.transaction::<usize, diesel::result::Error, _>(|conn| {
            (*conn.conn).execute_returning_count(source)
        })
    }

    fn transaction_state(&mut self,) ->  &mut<Self::TransactionManager as diesel::connection::TransactionManager<Self>>::TransactionStateData{
        self.conn.transaction_state()
    }

    fn set_prepared_statement_cache_size(&mut self, size: diesel::connection::CacheSize) {
        self.conn.set_prepared_statement_cache_size(size)
    }

    fn set_instrumentation(
        &mut self,
        instrumentation: impl diesel::connection::Instrumentation,
    ) {
        self.conn.set_instrumentation(instrumentation)
    }

    fn instrumentation(&mut self) -> &mut dyn diesel::connection::Instrumentation {
        self.conn.instrumentation()
    }
}

impl LoadConnection<DefaultLoadingMode> for ConnectionImpl {
    type Cursor<'conn, 'query> =
        <PgConnection as LoadConnection<DefaultLoadingMode>>::Cursor<'conn, 'query>;
    type Row<'conn, 'query> =
        <PgConnection as LoadConnection<DefaultLoadingMode>>::Row<'conn, 'query>;

    fn load<'conn, 'query, T>(
        &'conn mut self,
        source: T,
    ) -> diesel::prelude::QueryResult<Self::Cursor<'conn, 'query>>
    where
        T: diesel::query_builder::Query
            + diesel::query_builder::QueryFragment<Self::Backend>
            + diesel::query_builder::QueryId
            + 'query,
        Self::Backend: diesel::expression::QueryMetadata<T::SqlType>,
    {
        self.transaction::<Self::Cursor<'conn, 'query>, diesel::result::Error, _>(
            |conn| {
                log::info!("{:?}", source.to_sql(&mut PgQueryBuilder::default(), &Pg));
                <PgConnection as LoadConnection<DefaultLoadingMode>>::load::<T>(
                    &mut *conn.conn,
                    source,
                )
            },
        )
    }
}

impl FromRequest for ConnectionImpl {
    type Error = actix_web::Error;
    type Future = std::future::Ready<Result<ConnectionImpl, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let schema_name = req.extensions().get::<SchemaName>().cloned().unwrap().0;
        std::future::ready(ConnectionImpl::from_request_override(req, schema_name))
    }
}

#[derive(Deref, DerefMut)]
pub struct PublicConnection(pub ConnectionImpl);
impl FromRequest for PublicConnection {
    type Error = actix_web::Error;
    type Future = std::future::Ready<Result<PublicConnection, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        std::future::ready(
            ConnectionImpl::from_request_override(req, String::from("public"))
                .map(|conn| PublicConnection(conn)),
        )
    }
}

#[derive(Deref, DerefMut)]
pub struct SuperpositionConnection(pub ConnectionImpl);
impl FromRequest for SuperpositionConnection {
    type Error = actix_web::Error;
    type Future = std::future::Ready<Result<SuperpositionConnection, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        std::future::ready(
            ConnectionImpl::from_request_override(req, String::from("superposition"))
                .map(|conn| SuperpositionConnection(conn)),
        )
    }
}
