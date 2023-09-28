mod api;
mod db;
mod helpers;
mod logger;
mod middlewares;

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
    helpers::{get_from_env_unsafe, get_pod_info},
    service::types::{AppState, ExperimentationFlags},
};

use crate::middlewares::audit_response_header::{AuditHeader, TableName};

use api::*;
use helpers::{get_default_config_validation_schema, get_meta_schema};

use experimentation_platform::api::*;

#[actix_web::main]
async fn main() -> Result<()> {
    dotenv::dotenv().ok();
    init_log_subscriber();
    let (pod_identifier, deployment_id) = get_pod_info();
    let cac_span = span!(
        Level::INFO,
        "app",
        service = "context-aware-config",
        pod_id = pod_identifier,
        deployment_id = deployment_id
    );
    let _span_entered = cac_span.enter();
    let pool = get_pool().await;
    let admin_token = env::var("ADMIN_TOKEN").expect("Admin token is not set!");
    let cac_host: String = get_from_env_unsafe("CAC_HOST").expect("CAC host is not set");
    let cac_version: String = get_from_env_unsafe("CONTEXT_AWARE_CONFIG_VERSION")
        .expect("CONTEXT_AWARE_CONFIG_VERSION is not set");

    let string_to_int = |s: &String| -> i32 {
        s.chars()
            .map(|i| (i as i32) & rand::random::<i32>())
            .fold(0, i32::wrapping_add)
    };
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
            .wrap(middlewares::cors())
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

                snowflake_generator: Mutex::new(SnowflakeIdGenerator::new(
                    string_to_int(&deployment_id),
                    string_to_int(&pod_identifier),
                )),
                meta_schema: get_meta_schema(),
            }))
            .wrap(
                actix_web::middleware::DefaultHeaders::new()
                    .add(("X-SERVER-VERSION", cac_version.to_string()))
                    .add(("X-DEPLOYMENT-ID", deployment_id.clone()))
                    .add(("X-POD-ID", pod_identifier.clone())),
            )
            .route(
                "/health",
                get().to(|| async { HttpResponse::Ok().body("Health is good :D") }),
            )
            /***************************** V1 Routes *****************************/
            .service(scope("/context").service(context::endpoints()))
            .service(scope("/dimension").service(dimension::endpoints()))
            .service(scope("/default-config").service(default_config::endpoints()))
            .service(
                scope("/config")
                    .wrap(AuditHeader::new(TableName::Contexts))
                    .service(config::endpoints()),
            )
            .service(scope("/audit").service(audit_log::endpoints()))
            .service(external::endpoints(experiments::endpoints(scope("/experiments"))))
    })
    .bind(("0.0.0.0", 8080))?
    .workers(5)
    .run()
    .await
}