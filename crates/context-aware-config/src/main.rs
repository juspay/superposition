mod api;
mod db;
mod helpers;

use dotenv;
use std::{env, io::Result};

use actix_web::{
    middleware::Logger, web::get, web::scope, web::Data, App, HttpResponse, HttpServer,
};
use snowflake::SnowflakeIdGenerator;
use std::sync::Mutex;

use service_utils::{
    db::utils::get_pool,
    service::types::{AppState, ExperimentationFlags},
    helpers::get_from_env_unsafe
};

use api::*;
use helpers::get_default_config_validation_schema;

#[actix_web::main]
async fn main() -> Result<()> {
    dotenv::dotenv().ok();
    env_logger::init();
    let pool = get_pool().await;
    let admin_token = env::var("ADMIN_TOKEN").expect("Admin token is not set!");

    /****** EXPERIMENTATION PLATFORM ENVs *********/

    let allow_same_keys_overlapping_ctx: bool = get_from_env_unsafe("ALLOW_SAME_KEYS_OVERLAPPING_CTX").expect("ALLOW_SAME_KEYS_OVERLAPPING_CTX not set");
    let allow_diff_keys_overlapping_ctx: bool = get_from_env_unsafe("ALLOW_DIFF_KEYS_OVERLAPPING_CTX").expect("ALLOW_DIFF_KEYS_OVERLAPPING_CTX not set");
    let allow_same_keys_non_overlapping_ctx: bool = get_from_env_unsafe("ALLOW_SAME_KEYS_NON_OVERLAPPING_CTX").expect("ALLOW_SAME_KEYS_NON_OVERLAPPING_CTX not set");

    /****** EXPERIMENTATION PLATFORM ENVs *********/

    HttpServer::new(move || {
        let logger: Logger = Logger::default();
        App::new()
            .app_data(Data::new(AppState {
                db_pool: pool.clone(),
                default_config_validation_schema: get_default_config_validation_schema(),
		        admin_token: admin_token.to_owned(),

                experimentation_flags: ExperimentationFlags {
                    allow_same_keys_overlapping_ctx:
                        allow_same_keys_overlapping_ctx.to_owned(),
                    allow_diff_keys_overlapping_ctx:
                        allow_diff_keys_overlapping_ctx.to_owned(),
                    allow_same_keys_non_overlapping_ctx:
                        allow_same_keys_non_overlapping_ctx.to_owned(),
                },

                snowflake_generator: Mutex::new(SnowflakeIdGenerator::new(1, 1)),
            }))
            .wrap(logger)
            .route(
                "/health",
                get().to(|| async { HttpResponse::Ok().body("Health is good :D") }),
            )
            /***************************** V1 Routes *****************************/
            .service(scope("/context").service(context::endpoints()))
            .service(scope("/dimension").service(dimension::endpoints()))
            .service(scope("/default-config").service(default_config::endpoints()))
            .service(scope("/config").service(config::endpoints()))
    })
    .bind(("0.0.0.0", 8080))?
    .workers(5)
    .run()
    .await
}
