use std::{
    collections::HashSet,
    future::{Ready, ready},
    str::FromStr,
    sync::{Arc, Mutex},
    time::Duration,
};

use kronos_worker::KronosClient;

use actix_web::{Error, FromRequest, HttpMessage, HttpResponseBuilder, error, web::Data};
use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use diesel::PgConnection;
use diesel::r2d2::{ConnectionManager, PooledConnection};
use reqwest::header::HeaderValue;
use secrecy::SecretString;
use snowflake::SnowflakeIdGenerator;
use superposition_types::{User, database::models::Workspace};

use crate::{
    db::{PgSchemaConnectionPool, checkout_connection},
    workspace_lock::{
        WorkspaceLockLease, WorkspaceLockRequest, acquire_workspace_lease,
        release_workspace_lease,
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
    pub superposition_host: String,
    pub app_env: AppEnv,
    pub cac_version: String,
    pub db_pool: PgSchemaConnectionPool,
    pub experimentation_flags: ExperimentationFlags,
    pub snowflake_generator: Arc<Mutex<SnowflakeIdGenerator>>,
    pub tenant_middleware_exclusion_list: HashSet<String>,
    pub service_prefix: String,
    pub superposition_token: String,
    pub kronos_dispatch_token: String,
    pub redis: Option<fred::clients::RedisPool>,
    pub workspace_lock_default_ttl: Duration,
    pub workspace_lock_batch_ttl: Duration,
    pub http_client: reqwest::Client,
    pub master_encryption_key: Option<EncryptionKey>,
    pub kronos_client: Arc<dyn KronosClient>,
    /// Service mode: `Some(<shared workspace slug>)` — all SP schemas share one Kronos
    /// workspace, and webhook routing rides in the job input (templated headers).
    /// Library mode: `None` — Kronos tables live per SP schema, so we target the SP
    /// schema directly and provision per workspace.
    pub kronos_workspace: Option<String>,
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

        let result = checkout_connection(&app_state.db_pool)
            .map(DbConnection)
            .map_err(|e| {
                log::info!("DbConnection-FromRequest: Unable to get db connection from pool, error: {e}");
                error::ErrorInternalServerError("")
            });

        ready(result)
    }
}

/// A permit that holds a workspace write lock and provides DB access.
///
/// Acquired via `FromRequest` for write operations (POST, PUT, PATCH, DELETE).
/// The lock is released automatically when the permit is dropped.
///
/// Use `checkout()` to get a mutable reference to the underlying connection.
pub struct WorkspaceWritePermit {
    conn: superposition_types::DBConnection,
    lease: Option<WorkspaceLockLease>,
}

impl WorkspaceWritePermit {
    /// Returns a mutable reference to the database connection.
    pub fn checkout(&mut self) -> &mut superposition_types::DBConnection {
        &mut self.conn
    }

    fn release_lock(&mut self) {
        let Some(lease) = self.lease.take() else {
            return;
        };
        match release_workspace_lease(&mut self.conn, lease) {
            Ok(()) => log::debug!("released workspace lock"),
            Err(e) => log::error!("failed to release workspace lock: {}", e),
        }
    }
}

impl Drop for WorkspaceWritePermit {
    fn drop(&mut self) {
        self.release_lock();
    }
}

impl FromRequest for WorkspaceWritePermit {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let method = req.method().to_string();
        let path = req.path().to_string();
        let app_state = match req.app_data::<Data<AppState>>() {
            Some(state) => state,
            None => {
                log::error!("WorkspaceWritePermit: Unable to get app_data from request");
                return ready(Err(error::ErrorInternalServerError(
                    "Internal server error",
                )));
            }
        };
        let organisation_id = match req.extensions().get::<OrganisationId>().cloned() {
            Some(id) => id,
            None => {
                log::error!("WorkspaceWritePermit: Organisation Id not found");
                return ready(Err(error::ErrorInternalServerError(
                    "Organisation Id not found",
                )));
            }
        };
        let workspace_id = match req.extensions().get::<WorkspaceId>().cloned() {
            Some(id) => id,
            None => {
                log::error!("WorkspaceWritePermit: Workspace Id not found");
                return ready(Err(error::ErrorInternalServerError(
                    "Workspace Id not found",
                )));
            }
        };
        let user = req.extensions().get::<User>().cloned().unwrap_or_default();

        let lock_request = WorkspaceLockRequest {
            operation: format!("{} {}", method, path),
            locked_by: user.get_email(),
            ttl: app_state.workspace_lock_default_ttl,
        };

        let result = (|| {
            let mut conn = checkout_connection(&app_state.db_pool).map_err(|e| {
                log::info!("WorkspaceWritePermit: Unable to get db connection from pool, error: {e}");
                error::ErrorInternalServerError("")
            })?;
            let lease = acquire_workspace_lease(
                &mut conn,
                &organisation_id.0,
                &workspace_id.0,
                lock_request,
            )?;

            Ok(WorkspaceWritePermit {
                conn,
                lease: Some(lease),
            })
        })();

        ready(result)
    }
}

pub struct CustomHeaders {
    pub config_tags: Option<String>,
    pub idempotency_key: Option<String>,
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
            idempotency_key: header_val
                .get("idempotency-key")
                .and_then(|v| v.to_str().ok())
                .map(|v| v.trim().to_string())
                .filter(|v| !v.is_empty()),
        };
        ready(Ok(val))
    }
}
