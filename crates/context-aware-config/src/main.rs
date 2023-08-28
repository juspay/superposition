mod api;
mod db;
mod helpers;
mod logger;

use dotenv;
use logger::{init_log_subscriber, CustomRootSpanBuilder};
use std::{env, io::Result};
use tracing::{span, Level};

use actix_web::{web::get, web::scope, web::Data, App, HttpResponse, HttpServer};
use snowflake::SnowflakeIdGenerator;
use std::sync::Mutex;
use tracing_actix_web::TracingLogger;

use service_utils::{
    db::utils::get_pool,
    helpers::get_from_env_unsafe,
    service::types::{AppState, ExperimentationFlags},
};

use api::*;
use helpers::get_default_config_validation_schema;

use experimentation_platform::api::*;

#[actix_web::main]
async fn main() -> Result<()> {
    dotenv::dotenv().ok();
    init_log_subscriber();
    let cac_span = span!(Level::INFO, "app", service = "context-aware-config");
    let _span_entered = cac_span.enter();
    let pool = get_pool().await;
    let admin_token = env::var("ADMIN_TOKEN").expect("Admin token is not set!");
    let cac_host: String = get_from_env_unsafe("CAC_HOST").expect("CAC host is not set");
    let cac_version: String = get_from_env_unsafe("CONTEXT_AWARE_CONFIG_VERSION")
        .expect("CONTEXT_AWARE_CONFIG_VERSION is not set");

    /****** EXPERIMENTATION PLATFORM ENVs *********/

    let allow_same_keys_overlapping_ctx: bool =
        get_from_env_unsafe("ALLOW_SAME_KEYS_OVERLAPPING_CTX")
            .expect("ALLOW_SAME_KEYS_OVERLAPPING_CTX not set");
    let allow_diff_keys_overlapping_ctx: bool =
        get_from_env_unsafe("ALLOW_DIFF_KEYS_OVERLAPPING_CTX")
            .expect("ALLOW_DIFF_KEYS_OVERLAPPING_CTX not set");
    let allow_same_keys_non_overlapping_ctx: bool =
        get_from_env_unsafe("ALLOW_SAME_KEYS_NON_OVERLAPPING_CTX")
            .expect("ALLOW_SAME_KEYS_NON_OVERLAPPING_CTX not set");

    /****** EXPERIMENTATION PLATFORM ENVs *********/

    HttpServer::new(move || {
        App::new()
            .wrap(logger::GoldenSignalFactory)
            .wrap(TracingLogger::<CustomRootSpanBuilder>::new())
            .app_data(Data::new(AppState {
                db_pool: pool.clone(),
                default_config_validation_schema: get_default_config_validation_schema(),
                admin_token: admin_token.to_owned(),
                cac_host: cac_host.to_owned(),
                cac_version: cac_version.to_owned(),

                experimentation_flags: ExperimentationFlags {
                    allow_same_keys_overlapping_ctx: allow_same_keys_overlapping_ctx
                        .to_owned(),
                    allow_diff_keys_overlapping_ctx: allow_diff_keys_overlapping_ctx
                        .to_owned(),
                    allow_same_keys_non_overlapping_ctx:
                        allow_same_keys_non_overlapping_ctx.to_owned(),
                },

                snowflake_generator: Mutex::new(SnowflakeIdGenerator::new(1, 1)),
            }))
            .wrap(
                actix_web::middleware::DefaultHeaders::new()
                    .add(("X-SERVER-VERSION", cac_version.to_string())),
            )
            .route(
                "/health",
                get().to(|| async { HttpResponse::Ok().body("Health is good :D") }),
            )
            /***************************** V1 Routes *****************************/
            .service(scope("/context").service(context::endpoints()))
            .service(scope("/dimension").service(dimension::endpoints()))
            .service(scope("/default-config").service(default_config::endpoints()))
            .service(scope("/config").service(config::endpoints()))
            .service(experiments::endpoints())
    })
    .bind(("0.0.0.0", 8080))?
    .workers(5)
    .run()
    .await
}
