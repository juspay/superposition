mod api;
mod db;
mod helpers;
mod logger;
mod middlewares;

use crate::middlewares::audit_response_header::{AuditHeader, TableName};
use actix_web::{web::get, web::scope, web::Data, App, HttpResponse, HttpServer};
use api::*;
use dashboard_auth::middleware::DashboardAuth;
use dotenv;
use experimentation_platform::api::*;
use helpers::{get_default_config_validation_schema, get_meta_schema};
use logger::{init_log_subscriber, CustomRootSpanBuilder};
use std::{env, io::Result, collections::HashSet};
use tracing::{span, Level};

use snowflake::SnowflakeIdGenerator;
use std::{sync::Mutex, time::Duration};
use tracing_actix_web::TracingLogger;

use service_utils::{
    db::utils::get_pool,
    helpers::{get_from_env_unsafe, get_pod_info},
    middlewares::app_scope::AppExecutionScope,
    service::types::{AppScope, AppState, ExperimentationFlags},
};

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

    let prod: bool = get_from_env_unsafe("PROD").expect("PROD is not set");
    let enable_tenant_and_scope: bool = get_from_env_unsafe("ENABLE_TENANT_AND_SCOPE")
        .expect("ENABLE_TENANT_AND_SCOPE is not set");
    let tenants: HashSet<String> = get_from_env_unsafe::<String>("TENANTS")
        .expect("TENANTS is not set")
        .split(",")
        .map(|tenant| tenant.to_string())
        .collect::<HashSet<String>>();

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
            .wrap(DashboardAuth::default())
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
                prod: prod.to_owned(),
                enable_tenant_and_scope: enable_tenant_and_scope.to_owned(),
                tenants: tenants.to_owned(),
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
            .service(
                scope("/context")
                    .wrap(AppExecutionScope::new(AppScope::CAC))
                    .service(context::endpoints()),
            )
            .service(
                scope("/dimension")
                    .wrap(AppExecutionScope::new(AppScope::CAC))
                    .service(dimension::endpoints()),
            )
            .service(
                scope("/default-config")
                    .wrap(AppExecutionScope::new(AppScope::CAC))
                    .service(default_config::endpoints()),
            )
            .service(
                scope("/config")
                    .wrap(AuditHeader::new(TableName::Contexts))
                    .wrap(AppExecutionScope::new(AppScope::CAC))
                    .service(config::endpoints()),
            )
            .service(
                scope("/audit")
                .wrap(AppExecutionScope::new(AppScope::CAC))
                .service(audit_log::endpoints())
            )
            .service(
                external::endpoints(
                    experiments::endpoints(
                        scope("/experiments")
                    )
                )
                .wrap(AppExecutionScope::new(AppScope::EXPERIMENTATION))
            )
    })
    .bind(("0.0.0.0", 8080))?
    .workers(5)
    .keep_alive(Duration::from_secs(
        get_from_env_unsafe("ACTIX_KEEP_ALIVE").unwrap_or(120),
    ))
    .run()
    .await
}