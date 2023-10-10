use crate::db::pgschema_manager::{PgSchemaConnection, PgSchemaManager};
use derive_more::{Deref, DerefMut};
use jsonschema::JSONSchema;
use serde_json::json;

use std::{
    collections::HashSet,
    future::{ready, Ready},
    str::FromStr,
};

use actix_web::{error, web::Data, Error, FromRequest, HttpMessage};

use snowflake::SnowflakeIdGenerator;
use std::sync::Mutex;

pub struct ExperimentationFlags {
    pub allow_same_keys_overlapping_ctx: bool,
    pub allow_diff_keys_overlapping_ctx: bool,
    pub allow_same_keys_non_overlapping_ctx: bool,
}

#[derive(Copy, Clone, Debug)]
pub enum AppEnv {
    PROD,
    SANDBOX,
    DEV,
}

pub struct AppState {
    pub cac_host: String,
    pub app_env: AppEnv,
    pub tenants: HashSet<String>,
    pub cac_version: String,
    pub admin_token: String,
    pub db_pool: PgSchemaManager,
    pub default_config_validation_schema: JSONSchema,
    pub meta_schema: JSONSchema,
    pub experimentation_flags: ExperimentationFlags,
    pub snowflake_generator: Mutex<SnowflakeIdGenerator>,
    pub enable_tenant_and_scope: bool,
    pub tenant_middleware_exclusion_list: HashSet<String>,
}

impl FromStr for AppEnv {
    type Err = String;
    fn from_str(val: &str) -> Result<AppEnv, Self::Err> {
        match val {
            "PROD" => Ok(AppEnv::PROD),
            "SANDBOX" => Ok(AppEnv::SANDBOX),
            "DEV" => Ok(AppEnv::DEV),
            _ => Err("invalid app env!!".to_string()),
        }
    }
}

#[derive(Copy, Clone, Debug, strum_macros::Display)]
#[strum(serialize_all = "lowercase")]
pub enum AppScope {
    CAC,
    EXPERIMENTATION,
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
            (true, _, Some(t), Some(s)) => Ok(AppExecutionNamespace(format!(
                "{}_{}",
                t.as_str(),
                s.to_string()
            ))),
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
                    Ok(Tenant("mjos".into()))
                }
            }
        };
        ready(result)
    }
}

#[derive(Deref, DerefMut)]
pub struct DbConnection(pub PgSchemaConnection);
impl FromRequest for DbConnection {
    type Error = Error;
    type Future = Ready<Result<DbConnection, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let namespace = match AppExecutionNamespace::from_request_sync(req) {
            Ok(val) => val.as_str().to_string(),
            Err(e) => {
                return ready(Err(e));
            }
        };

        let app_state = match req.app_data::<Data<AppState>>() {
            Some(state) => state,
            None => {
                log::info!(
                    "DbConnection-FromRequest: Unable to get app_data from request"
                );
                return ready(Err(error::ErrorInternalServerError("")));
            }
        };

        let result = match app_state.db_pool.get_conn(namespace) {
            Ok(conn) => Ok(DbConnection(conn)),
            Err(e) => {
                log::info!("Unable to get db connection from pool, error: {e}");
                Err(error::ErrorInternalServerError(""))
            }
        };

        ready(result)
    }
}
