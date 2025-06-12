use std::sync::Mutex;
use std::{
    collections::{HashMap, HashSet},
    future::{ready, Ready},
    str::FromStr,
    sync::Arc,
};

use actix_web::{error, web::Data, Error, FromRequest, HttpMessage};
use derive_more::{Deref, DerefMut};
use diesel::r2d2::{ConnectionManager, PooledConnection};
use diesel::{Connection, PgConnection};
use jsonschema::JSONSchema;
use snowflake::SnowflakeIdGenerator;

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
    XAuditId,
    LastModified,
}

pub struct AppState {
    pub cac_host: String,
    pub app_env: AppEnv,
    pub cac_version: String,
    pub db_pool: PgSchemaConnectionPool,
    pub meta_schema: JSONSchema,
    pub experimentation_flags: ExperimentationFlags,
    pub snowflake_generator: Arc<Mutex<SnowflakeIdGenerator>>,
    pub tenant_middleware_exclusion_list: HashSet<String>,
    pub service_prefix: String,
    pub superposition_token: String,
    #[cfg(feature = "high-performance-mode")]
    pub redis: fred::clients::RedisPool,
    pub http_client: reqwest::Client,
    pub encrypted_keys: HashMap<String, String>,
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
