use diesel::{
    r2d2::{PooledConnection, ConnectionManager, Pool},
    PgConnection,
};
use jsonschema::JSONSchema;

use std::future::{ready, Ready};

use actix_web::{error, web::Data, Error, FromRequest};

use snowflake::SnowflakeIdGenerator;
use std::sync::Mutex;

pub struct ExperimentationFlags {
    pub allow_same_keys_overlapping_ctx : bool,
    pub allow_diff_keys_overlapping_ctx : bool,
    pub allow_same_keys_non_overlapping_ctx: bool,
}

pub struct AppState {
    pub cac_host: String,
    pub db_pool: Pool<ConnectionManager<PgConnection>>,
    pub default_config_validation_schema: JSONSchema,
    pub admin_token: String,
    pub experimentation_flags: ExperimentationFlags,
    pub snowflake_generator: Mutex<SnowflakeIdGenerator>,
}

#[derive(Clone)]
pub struct AuthenticationInfo(pub String);
impl FromRequest for AuthenticationInfo {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let opt_token = req
            .headers()
            .get("Authorization")
            .and_then(|h| h.to_str().ok())
            .and_then(|h| {
                if h.starts_with("Bearer") {
                    Some(h)
                } else {
                    None
                }
            })
            .and_then(|h| {
                h.split(' ')
                    .collect::<Vec<_>>()
                    .get(1)
                    .map(|token| token.to_string())
            });
        dbg!(format!("Token is \"{:?}\"", opt_token));
        let opt_admin_token = req
            .app_data()
            .map(|d: &Data<AppState>| d.admin_token.as_str());

        let result = match (opt_token, opt_admin_token) {
            (_, None) => {
                log::info!("ERROR: ADMIN TOKEN NOT FOUND!!!!");
                Err(error::ErrorInternalServerError(""))
            }
            (None, _) => Err(error::ErrorUnauthorized("Bearer token required.")),
            (Some(token), Some(admin_token)) if token != admin_token => {
                Err(error::ErrorUnauthorized(""))
            }
            (Some(_token), Some(_admin_token)) => {
                let email = "cac.admin@juspay.in";
                let auth_info = AuthenticationInfo(email.to_string());
                Ok(auth_info)
            }
        };
        ready(result)
    }
}

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
