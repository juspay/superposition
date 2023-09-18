use diesel::{
    r2d2::{ConnectionManager, Pool, PooledConnection},
    PgConnection,
};
use jsonschema::JSONSchema;

use std::{collections::HashSet, future::{ready, Ready}};

use actix_web::{
    error, web::Data, Error, FromRequest, HttpMessage,
};

use snowflake::SnowflakeIdGenerator;
use std::sync::Mutex;

use dashboard_auth::types::Tenant;

pub struct ExperimentationFlags {
    pub allow_same_keys_overlapping_ctx: bool,
    pub allow_diff_keys_overlapping_ctx: bool,
    pub allow_same_keys_non_overlapping_ctx: bool,
}

pub struct AppState {
    pub cac_host: String,
    pub prod: bool,
    pub tenants: HashSet<String>,
    pub cac_version: String,
    pub admin_token: String,
    pub db_pool: Pool<ConnectionManager<PgConnection>>,
    pub default_config_validation_schema: JSONSchema,
    pub meta_schema: JSONSchema,
    pub experimentation_flags: ExperimentationFlags,
    pub snowflake_generator: Mutex<SnowflakeIdGenerator>,
    pub enable_tenant_and_scope: bool,
}

#[derive(Copy, Clone, Debug)]
pub enum AppScope {
    CAC,
    EXPERIMENTATION,
}

impl ToString for AppScope {
    fn to_string(&self) -> String {
        match self {
            AppScope::CAC => String::from("cac"),
            AppScope::EXPERIMENTATION => String::from("experimentation"),
        }
    }
}

pub type DBConnection = PooledConnection<ConnectionManager<PgConnection>>;

pub struct DbConnection(pub PooledConnection<ConnectionManager<PgConnection>>);
impl FromRequest for DbConnection {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let app_state = match req.app_data::<Data<AppState>>() {
            Some(state) => state,
            None => {
                log::info!("Unable to get app_data from request");
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

pub struct TenantNScope(pub String);
impl FromRequest for TenantNScope {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        payload: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let app_state = match req.app_data::<Data<AppState>>() {
            Some(state) => state,
            None => {
                log::info!("Unable to get app_date from request");
                return ready(Err(error::ErrorInternalServerError("")));
            }
        };

        if !app_state.enable_tenant_and_scope {
            let default_tenant = match app_state.prod {
                true => "cac_v1".to_string(),
                false => "public".to_string()
            };

            return ready(Ok(TenantNScope(default_tenant)));
        }

        // extract headers for tenant id
        let tenant = Tenant::from_request(req, payload)
            .into_inner()
            .map_or(None, |value: Tenant| Some(value.as_str().to_string()));

        let scope = req
            .extensions()
            .get::<AppScope>()
            .and_then(|app_scope| Some(app_scope.to_string()));

        let tenant_n_scope:Result<Self, Self::Error> = match (tenant, scope) {
            (Some(t), Some(s)) if app_state.tenants.contains(&t) => Ok(TenantNScope(format!("{t}_{s}"))),
            (Some(_), Some(_)) => Err(error::ErrorBadRequest("invalid x-tenant value")),
            (None, _) => Err(error::ErrorBadRequest("x-tenant not set")),
            (_, None) => {
                log::error!("AppScope not set for the request!");
                Err(error::ErrorInternalServerError("something went wrong"))
            }
        };

        ready(tenant_n_scope)
    }
}