use std::{
    collections::HashSet,
    future::{Ready, ready},
    str::FromStr,
    sync::{Arc, Mutex},
    time::Duration,
};

use actix_web::http::StatusCode;
use actix_web::{Error, FromRequest, HttpMessage, HttpResponseBuilder, error, web::Data};
use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use diesel::r2d2::{ConnectionManager, PooledConnection};
use diesel::{Connection, PgConnection, QueryableByName, RunQueryDsl};
use futures_util::future::LocalBoxFuture;
use reqwest::header::HeaderValue;
use secrecy::SecretString;
use snowflake::SnowflakeIdGenerator;
use superposition_types::database::models::Workspace;
use tokio::time::{Instant, sleep};

use crate::{
    db::{PgSchemaConnectionPool, QueryConnection, QueryContext, checkout_connection},
    workspace_lock::{
        AcquireLockError, RedisLockGuard, WorkspaceLockTarget, acquire_redis_lock,
    },
};

pub struct ExperimentationFlags {
    pub allow_same_keys_overlapping_ctx: bool,
    pub allow_diff_keys_overlapping_ctx: bool,
    pub allow_same_keys_non_overlapping_ctx: bool,
}

#[derive(Copy, Clone, Debug)]
pub enum AppEnv {
    PROD,
    SANDBOX,
    TEST,
    DEV,
}

#[derive(Copy, Clone, Debug, strum_macros::Display)]
#[strum(serialize_all = "kebab-case")]
pub enum AppHeader {
    XConfigVersion,
    LastModified,
}

impl AppHeader {
    pub fn add_last_modified(
        max_created_at: Option<DateTime<Utc>>,
        is_smithy: bool,
        resp_builder: &mut HttpResponseBuilder,
    ) {
        let Some(date) = max_created_at else {
            return;
        };
        let value = if is_smithy {
            // Smithy needs to be in this format otherwise they can't
            // deserialize it.
            HeaderValue::from_str(date.to_rfc3339().as_str())
        } else {
            HeaderValue::from_str(date.to_rfc2822().as_str())
        };

        match value {
            Ok(header_value) => {
                resp_builder
                    .insert_header((Self::LastModified.to_string(), header_value));
            }
            Err(e) => {
                log::error!("failed parsing datetime_utc {:?}, error: {:?}", date, e);
            }
        }
    }

    pub fn add_config_version(
        config_version: &Option<i64>,
        resp_builder: &mut HttpResponseBuilder,
    ) {
        if let Some(val) = config_version {
            resp_builder.insert_header((
                Self::XConfigVersion.to_string(),
                val.clone().to_string(),
            ));
        }
    }
}

pub struct EncryptionKey {
    pub current_key: SecretString,
    pub previous_key: Option<SecretString>,
}

pub struct AppState {
    pub cac_host: String,
    pub app_env: AppEnv,
    pub cac_version: String,
    pub db_pool: PgSchemaConnectionPool,
    pub experimentation_flags: ExperimentationFlags,
    pub snowflake_generator: Arc<Mutex<SnowflakeIdGenerator>>,
    pub tenant_middleware_exclusion_list: HashSet<String>,
    pub service_prefix: String,
    pub superposition_token: String,
    pub redis: Option<fred::clients::RedisPool>,
    pub redis_lock_ttl: Duration,
    pub redis_lock_heartbeat_enabled: bool,
    pub workspace_lock_retry_interval: Duration,
    pub workspace_lock_retry_timeout: Duration,
    pub http_client: reqwest::Client,
    pub master_encryption_key: Option<EncryptionKey>,
}

impl FromStr for AppEnv {
    type Err = String;
    fn from_str(val: &str) -> Result<AppEnv, Self::Err> {
        match val {
            "PROD" => Ok(AppEnv::PROD),
            "SANDBOX" => Ok(AppEnv::SANDBOX),
            "DEV" => Ok(AppEnv::DEV),
            "TEST" => Ok(AppEnv::TEST),
            _ => Err("invalid app env!!".to_string()),
        }
    }
}

#[derive(Deref, DerefMut, Clone, Debug)]
pub struct WorkspaceId(pub String);

impl FromRequest for WorkspaceId {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let result = req.extensions().get::<Self>().cloned().ok_or_else(|| {
            log::error!("Workspace Id not found");
            actix_web::error::ErrorInternalServerError("Workspace Id not found")
        });

        ready(result)
    }
}

#[derive(Deref, DerefMut, Clone, Debug)]
pub struct OrganisationId(pub String);

impl Default for OrganisationId {
    fn default() -> Self {
        Self(String::from("superposition"))
    }
}

impl FromRequest for OrganisationId {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let result = req.extensions().get::<Self>().cloned().ok_or_else(|| {
            log::error!("Organisation Id not found");
            actix_web::error::ErrorInternalServerError("Organisation Id not found")
        });

        ready(result)
    }
}

#[derive(Deref, DerefMut, Clone, Debug)]
pub struct SchemaName(pub String);

impl Default for SchemaName {
    fn default() -> Self {
        Self(String::from("superposition"))
    }
}

impl FromRequest for SchemaName {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let result = req.extensions().get::<Self>().cloned().ok_or_else(|| {
            log::error!("Please check that the organisation id and workspace id are being properly sent");
            actix_web::error::ErrorInternalServerError("Please check that the organisation id and workspace id are being properly sent")
        });

        ready(result)
    }
}

#[derive(Clone)]
pub struct WorkspaceContext {
    pub workspace_id: WorkspaceId,
    pub organisation_id: OrganisationId,
    pub schema_name: SchemaName,
    pub settings: Workspace,
}

impl FromRequest for WorkspaceContext {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let result = req.extensions().get::<Self>().cloned().ok_or_else(|| {
            log::error!("Please check that the organisation id and workspace id are being properly sent");
            actix_web::error::ErrorInternalServerError("Please check that the organisation id and workspace id are being properly sent")
        });

        ready(result)
    }
}

#[derive(Deref, DerefMut)]
pub struct DbConnection(pub PooledConnection<ConnectionManager<PgConnection>>);
impl FromRequest for DbConnection {
    type Error = Error;
    type Future = Ready<Result<DbConnection, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let app_state = match req.app_data::<Data<AppState>>() {
            Some(state) => state,
            None => {
                log::info!(
                    "DbConnection-FromRequest: Unable to get app_data from request"
                );
                return ready(Err(error::ErrorInternalServerError("")));
            }
        };

        let result = checkout_db_connection(app_state);

        ready(result)
    }
}

enum WorkspaceWriteBackend {
    Redis {
        _guard: RedisLockGuard,
        db_pool: PgSchemaConnectionPool,
    },
    Postgres {
        db_conn: Option<superposition_types::DBConnection>,
        lock_key: Option<i64>,
    },
}

/// Explicit permit for serialized workspace write flows.
///
/// - On the Redis path, it owns the Redis lock guard and borrows DB connections
///   from the pool only when work is executed.
/// - On the PostgreSQL path, it owns the same DB connection that acquired the
///   advisory lock, so all critical DB work stays on the lock-owning session.
pub struct WorkspaceWritePermit {
    backend: WorkspaceWriteBackend,
}

#[derive(QueryableByName)]
struct LockResult {
    #[diesel(sql_type = diesel::sql_types::Bool)]
    locked: bool,
}

#[derive(Clone, Copy, Debug)]
struct WorkspaceLockRetryPolicy {
    retry_interval: Duration,
    retry_timeout: Duration,
}

impl WorkspaceLockRetryPolicy {
    fn from_app_state(app_state: &AppState) -> Self {
        Self {
            retry_interval: normalize_retry_interval(
                app_state.workspace_lock_retry_interval,
            ),
            retry_timeout: app_state.workspace_lock_retry_timeout,
        }
    }

    fn deadline(self) -> Instant {
        Instant::now() + self.retry_timeout
    }

    fn next_delay(self, now: Instant, deadline: Instant) -> Option<Duration> {
        if now >= deadline {
            return None;
        }

        Some(std::cmp::min(
            normalize_retry_interval(self.retry_interval),
            deadline.saturating_duration_since(now),
        ))
    }
}

fn normalize_retry_interval(retry_interval: Duration) -> Duration {
    if retry_interval.is_zero() {
        Duration::from_millis(1)
    } else {
        retry_interval
    }
}

fn workspace_busy_error() -> Error {
    error::ErrorConflict("Workspace is busy with another write request; retry later")
}

impl WorkspaceWritePermit {
    async fn acquire(
        app_state: Data<AppState>,
        organisation_id: OrganisationId,
        workspace_id: WorkspaceId,
    ) -> Result<Self, Error> {
        let target = WorkspaceLockTarget::new(&organisation_id.0, &workspace_id.0);
        let retry_policy = WorkspaceLockRetryPolicy::from_app_state(app_state.get_ref());

        if let Some(redis_pool) = app_state.redis.as_ref() {
            if let Some(lock_context) = Self::try_acquire_redis_with_retry(
                &app_state,
                redis_pool,
                &target,
                retry_policy,
            )
            .await?
            {
                return Ok(lock_context);
            }
        }

        Self::try_acquire_postgres_with_retry(&app_state, &target, retry_policy).await
    }

    async fn try_acquire_redis_with_retry(
        app_state: &AppState,
        redis_pool: &fred::clients::RedisPool,
        target: &WorkspaceLockTarget,
        retry_policy: WorkspaceLockRetryPolicy,
    ) -> Result<Option<Self>, Error> {
        let deadline = retry_policy.deadline();

        loop {
            match acquire_redis_lock(
                redis_pool,
                target,
                app_state.redis_lock_ttl,
                app_state.redis_lock_heartbeat_enabled,
            )
            .await
            {
                Ok(guard) => {
                    return Ok(Some(Self::new_redis(app_state.db_pool.clone(), guard)));
                }
                Err(AcquireLockError::LockContended { .. }) => {
                    let now = Instant::now();
                    let Some(delay) = retry_policy.next_delay(now, deadline) else {
                        return Err(workspace_busy_error());
                    };

                    log::debug!(
                        "workspace redis lock busy; retrying in {} ms (redis_key: {})",
                        delay.as_millis(),
                        target.redis_key,
                    );
                    sleep(delay).await;
                }
                Err(e) => {
                    log::warn!(
                        "redis lock unavailable ({}), falling back to PG advisory lock \
                         (redis_key: {})",
                        e,
                        target.redis_key,
                    );
                    return Ok(None);
                }
            }
        }
    }

    async fn try_acquire_postgres_with_retry(
        app_state: &AppState,
        target: &WorkspaceLockTarget,
        retry_policy: WorkspaceLockRetryPolicy,
    ) -> Result<Self, Error> {
        let deadline = retry_policy.deadline();

        loop {
            let DbConnection(mut db_conn) = checkout_db_connection(app_state)?;
            let acquired = diesel::sql_query("SELECT pg_try_advisory_lock($1) AS locked")
                .bind::<diesel::sql_types::BigInt, _>(target.pg_lock_key)
                .get_result::<LockResult>(&mut db_conn)
                .map_err(|e| {
                    log::error!(
                        "failed to acquire PG advisory lock (key: {}): {}",
                        target.pg_lock_key,
                        e
                    );
                    error::ErrorInternalServerError("Failed to acquire workspace lock")
                })?
                .locked;

            if acquired {
                return Ok(Self::new_postgres(db_conn, target.pg_lock_key));
            }

            drop(db_conn);

            let now = Instant::now();
            let Some(delay) = retry_policy.next_delay(now, deadline) else {
                return Err(workspace_busy_error());
            };

            log::debug!(
                "workspace PG advisory lock busy; retrying in {} ms (key: {})",
                delay.as_millis(),
                target.pg_lock_key,
            );
            sleep(delay).await;
        }
    }

    fn new_redis(db_pool: PgSchemaConnectionPool, guard: RedisLockGuard) -> Self {
        Self {
            backend: WorkspaceWriteBackend::Redis {
                _guard: guard,
                db_pool,
            },
        }
    }

    fn new_postgres(db_conn: superposition_types::DBConnection, lock_key: i64) -> Self {
        Self {
            backend: WorkspaceWriteBackend::Postgres {
                db_conn: Some(db_conn),
                lock_key: Some(lock_key),
            },
        }
    }

    fn ensure_healthy(&self) -> superposition_types::result::Result<()> {
        match &self.backend {
            WorkspaceWriteBackend::Redis { _guard, .. } => {
                _guard.ensure_healthy().map_err(|message| {
                    superposition_types::result::AppError::ResponseError(
                        superposition_types::result::ResponseError {
                            message,
                            status_code: StatusCode::CONFLICT,
                        },
                    )
                })
            }
            WorkspaceWriteBackend::Postgres { .. } => Ok(()),
        }
    }

    pub fn query_context(
        &mut self,
    ) -> superposition_types::result::Result<QueryContext<'_>> {
        self.ensure_healthy()?;

        match &mut self.backend {
            WorkspaceWriteBackend::Redis { db_pool, .. } => {
                Ok(QueryContext::Pooled(db_pool))
            }
            WorkspaceWriteBackend::Postgres { db_conn, .. } => {
                Ok(QueryContext::Pinned(db_conn.as_mut().expect(
                    "workspace write permit must hold a db connection",
                )))
            }
        }
    }

    pub fn checkout(
        &mut self,
    ) -> superposition_types::result::Result<QueryConnection<'_>> {
        self.ensure_healthy()?;

        match &mut self.backend {
            WorkspaceWriteBackend::Redis { db_pool, .. } => {
                Ok(QueryConnection::Pooled(checkout_connection(db_pool)?))
            }
            WorkspaceWriteBackend::Postgres { db_conn, .. } => {
                Ok(QueryConnection::Pinned(db_conn.as_mut().expect(
                    "workspace write permit must hold a db connection",
                )))
            }
        }
    }

    pub fn run_query<T, F>(
        &mut self,
        query_fn: F,
    ) -> superposition_types::result::Result<T>
    where
        F: FnOnce(
            &mut superposition_types::DBConnection,
        ) -> Result<T, diesel::result::Error>,
    {
        let mut query_context = self.query_context()?;
        query_context.run_query(query_fn)
    }

    pub fn transaction<T, F>(
        &mut self,
        query_fn: F,
    ) -> superposition_types::result::Result<T>
    where
        F: FnOnce(
            &mut superposition_types::DBConnection,
        ) -> superposition_types::result::Result<T>,
    {
        let mut query_context = self.query_context()?;
        query_context.transaction(query_fn)
    }

    pub async fn release(mut self) {
        match &mut self.backend {
            WorkspaceWriteBackend::Redis { _guard, .. } => {
                if let Err(e) = _guard.release().await {
                    log::error!(
                        "failed to release redis lock on normal completion: {}",
                        e
                    );
                }
            }
            WorkspaceWriteBackend::Postgres { .. } => {
                self.release_postgres_lock();
            }
        }
    }

    fn release_postgres_lock(&mut self) {
        let Some(lock_key) = (match &mut self.backend {
            WorkspaceWriteBackend::Postgres { lock_key, .. } => lock_key.take(),
            WorkspaceWriteBackend::Redis { .. } => None,
        }) else {
            return;
        };

        let WorkspaceWriteBackend::Postgres { db_conn, .. } = &mut self.backend else {
            return;
        };

        let Some(db_conn) = db_conn.as_mut() else {
            return;
        };

        let result = diesel::sql_query("SELECT pg_advisory_unlock($1)")
            .bind::<diesel::sql_types::BigInt, _>(lock_key)
            .execute(db_conn);

        match result {
            Ok(_) => log::debug!("released PG advisory lock (key: {})", lock_key),
            Err(e) => log::error!(
                "failed to release PG advisory lock (key: {}): {}",
                lock_key,
                e
            ),
        }
    }
}

impl Drop for WorkspaceWritePermit {
    fn drop(&mut self) {
        self.release_postgres_lock();
    }
}

impl FromRequest for WorkspaceWritePermit {
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let app_state = req.app_data::<Data<AppState>>().cloned();
        let organisation_id = req.extensions().get::<OrganisationId>().cloned();
        let workspace_id = req.extensions().get::<WorkspaceId>().cloned();

        Box::pin(async move {
            let app_state = app_state.ok_or_else(|| {
                log::info!(
                    "WorkspaceWritePermit-FromRequest: Unable to get app_data from request"
                );
                error::ErrorInternalServerError("")
            })?;
            let organisation_id = organisation_id.ok_or_else(|| {
                log::error!("Organisation Id not found");
                error::ErrorInternalServerError("Organisation Id not found")
            })?;
            let workspace_id = workspace_id.ok_or_else(|| {
                log::error!("Workspace Id not found");
                error::ErrorInternalServerError("Workspace Id not found")
            })?;

            WorkspaceWritePermit::acquire(app_state, organisation_id, workspace_id).await
        })
    }
}

fn checkout_db_connection(app_state: &AppState) -> Result<DbConnection, Error> {
    match app_state.db_pool.get() {
        Ok(mut conn) => {
            conn.set_prepared_statement_cache_size(
                diesel::connection::CacheSize::Disabled,
            );
            Ok(DbConnection(conn))
        }
        Err(e) => {
            log::info!("Unable to get db connection from pool, error: {e}");
            Err(error::ErrorInternalServerError(""))
        }
    }
}

pub struct CustomHeaders {
    pub config_tags: Option<String>,
}
impl FromRequest for CustomHeaders {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let header_val = req.headers();
        let val = CustomHeaders {
            config_tags: header_val.get("x-config-tags").and_then(|header_val| {
                header_val.to_str().map_or(None, |v| Some(v.to_string()))
            }),
        };
        ready(Ok(val))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zero_retry_interval_is_normalized() {
        let normalized = normalize_retry_interval(Duration::ZERO);

        assert_eq!(normalized, Duration::from_millis(1));
    }

    #[test]
    fn next_delay_caps_to_remaining_budget() {
        let policy = WorkspaceLockRetryPolicy {
            retry_interval: Duration::from_millis(100),
            retry_timeout: Duration::from_millis(250),
        };
        let start = Instant::now();
        let deadline = start + Duration::from_millis(50);

        let delay = policy.next_delay(start, deadline);

        assert_eq!(delay, Some(Duration::from_millis(50)));
    }

    #[test]
    fn next_delay_stops_at_deadline() {
        let policy = WorkspaceLockRetryPolicy {
            retry_interval: Duration::from_millis(25),
            retry_timeout: Duration::from_millis(100),
        };
        let deadline = Instant::now();

        let delay = policy.next_delay(deadline, deadline);

        assert_eq!(delay, None);
    }
}
