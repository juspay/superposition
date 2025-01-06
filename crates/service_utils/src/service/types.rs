use std::sync::Mutex;
use std::{
    collections::HashSet,
    future::{ready, Ready},
    str::FromStr,
    sync::Arc,
};

use actix_web::{error, web::Data, Error, FromRequest, HttpMessage};
use aws_sdk_kms::Client;
use derive_more::{Deref, DerefMut};
use diesel::r2d2::{ConnectionManager, PooledConnection};
use diesel::PgConnection;
use jsonschema::JSONSchema;
use serde_json::json;
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
    pub enable_tenant_and_scope: bool,
    pub tenant_middleware_exclusion_list: HashSet<String>,
    pub service_prefix: String,
    pub superposition_token: String,
    #[cfg(feature = "high-performance-mode")]
    pub redis: fred::clients::RedisPool,
    pub http_client: reqwest::Client,
    pub kms_client: Option<Client>,
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

#[derive(Copy, Clone, Debug, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum AppScope {
    CAC,
    EXPERIMENTATION,
    SUPERPOSITION,
}
impl FromRequest for AppScope {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let scope = req.extensions().get::<AppScope>().cloned();
        let result = match scope {
            Some(v) => Ok(v),
            None => Err(error::ErrorInternalServerError("app scope not set")),
        };
        ready(result)
    }
}

#[derive(Deref, DerefMut, Clone, Debug)]
pub struct AppExecutionNamespace(pub String);
impl AppExecutionNamespace {
    pub fn from_request_sync(req: &actix_web::HttpRequest) -> Result<Self, Error> {
        let app_state = match req.app_data::<Data<AppState>>() {
            Some(val) => val,
            None => {
                log::error!("get_app_execution_namespace: AppState not set");
                return Err(error::ErrorInternalServerError(""));
            }
        };

        let tenant = req.extensions().get::<Tenant>().cloned();
        let scope = req.extensions().get::<AppScope>().cloned();

        match (
            app_state.enable_tenant_and_scope,
            app_state.app_env,
            tenant,
            scope,
        ) {
            (false, _, _, _) => Ok(AppExecutionNamespace("cac_v1".to_string())),
            (true, _, Some(t), Some(_)) => Ok(AppExecutionNamespace(t.0)),
            (true, _, None, _) => {
                log::error!(
                    "get_app_execution_namespace: Tenant not set in request extensions"
                );
                Err(error::ErrorInternalServerError(""))
            }
            (true, _, _, None) => {
                log::error!(
                    "get_app_execution_namespace: AppScope not set in request extensions"
                );
                Err(error::ErrorInternalServerError(""))
            }
        }
    }
}

impl FromRequest for AppExecutionNamespace {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        ready(AppExecutionNamespace::from_request_sync(req))
    }
}

#[derive(Deref, DerefMut, Clone, Debug)]
pub struct Tenant(pub String);
impl FromRequest for Tenant {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let tenant = req.extensions().get::<Tenant>().cloned();
        let result = match tenant {
            Some(v) => Ok(v),
            None => {
                let app_state = match req.app_data::<Data<AppState>>() {
                    Some(val) => val,
                    None => {
                        log::error!("app state not set");
                        return ready(Err(error::ErrorInternalServerError(json!({
                            "message": "an unknown error occurred with the app. Please contact an admin"
                        }))));
                    }
                };
                if app_state.enable_tenant_and_scope {
                    Err(error::ErrorInternalServerError(json!({
                        "message": "tenant was not set. Please ensure you are passing in the x-tenant header"
                    })))
                } else {
                    Ok(Tenant("public".into()))
                }
            }
        };
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
            Ok(conn) => Ok(DbConnection(conn)),
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
        let organisation = req.extensions().get::<OrganisationId>().cloned();
        let result = match organisation {
            Some(v) => Ok(v),
            None => {
                let app_state = match req.app_data::<Data<AppState>>() {
                    Some(val) => val,
                    None => {
                        log::error!("app state not set");
                        return ready(Err(error::ErrorInternalServerError(json!({
                            "message": "an unknown error occurred with the app. Please contact an admin"
                        }))));
                    }
                };
                if app_state.enable_tenant_and_scope {
                    Err(error::ErrorInternalServerError(json!({
                        "message": "x-org-id was not set. Please ensure you are passing in the x-tenant header"
                    })))
                } else {
                    Ok(OrganisationId::default())
                }
            }
        };
        ready(result)
    }
}
