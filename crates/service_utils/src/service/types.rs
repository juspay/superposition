use std::{
    collections::{HashMap, HashSet},
    future::{Ready, ready},
    str::FromStr,
    sync::{Arc, Mutex},
};

use tokio::sync::watch;

use actix_web::{Error, FromRequest, HttpMessage, HttpResponseBuilder, error, web::Data};
use chrono::{DateTime, Utc};
use derive_more::{Deref, DerefMut};
use diesel::r2d2::{ConnectionManager, PooledConnection};
use diesel::{Connection, PgConnection};
use reqwest::header::HeaderValue;
use secrecy::SecretString;
use snowflake::SnowflakeIdGenerator;
use superposition_types::database::models::Workspace;

use crate::db::PgSchemaConnectionPool;

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
    pub http_client: reqwest::Client,
    pub master_encryption_key: Option<EncryptionKey>,
    pub sse_broadcaster: Mutex<HashMap<String, watch::Sender<()>>>,
}

impl AppState {
    pub fn get_sse_sender(&self, schema_name: &str) -> watch::Sender<()> {
        let mut map = self.sse_broadcaster.lock().expect("sse_broadcaster lock poisoned");
        map.entry(schema_name.to_string())
            .or_insert_with(|| watch::channel(()).0)
            .clone()
    }

    pub fn subscribe_sse(&self, schema_name: &str) -> watch::Receiver<()> {
        self.get_sse_sender(schema_name).subscribe()
    }
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

        let result = match app_state.db_pool.get() {
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
        };

        ready(result)
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
