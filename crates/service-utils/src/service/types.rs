use diesel::{
    r2d2::{ConnectionManager, Pool, PooledConnection},
    PgConnection,
};
use jsonschema::JSONSchema;

use std::future::{ready, Ready};

use actix_web::{error, web::Data, Error, FromRequest};

use snowflake::SnowflakeIdGenerator;
use std::sync::Mutex;

pub struct ExperimentationFlags {
    pub allow_same_keys_overlapping_ctx: bool,
    pub allow_diff_keys_overlapping_ctx: bool,
    pub allow_same_keys_non_overlapping_ctx: bool,
}

pub struct AppState {
    pub cac_host: String,
    pub cac_version: String,
    pub admin_token: String,
    pub db_pool: Pool<ConnectionManager<PgConnection>>,
    pub default_config_validation_schema: JSONSchema,
    pub meta_schema: JSONSchema,
    pub experimentation_flags: ExperimentationFlags,
    pub snowflake_generator: Mutex<SnowflakeIdGenerator>,
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
