mod api;
mod auth;
mod db;
mod helpers;
mod logger;
mod middlewares;

use crate::middlewares::audit_response_header::{AuditHeader, TableName};
use actix_web::{web, web::get, web::scope, web::Data, App, HttpResponse, HttpServer};
use api::*;
use dashboard_auth::{
    middleware::DashboardAuth,
    types::{AuthenticatedRoute, AuthenticatedRouteList},
};
use dotenv;
use experimentation_platform::api::*;
use helpers::{get_default_config_validation_schema, get_meta_schema};
use logger::{init_log_subscriber, CustomRootSpanBuilder};
use std::{collections::HashSet, env, io::Result};
use tracing::{span, Level};

use snowflake::SnowflakeIdGenerator;
use std::{sync::Mutex, time::Duration};
use tracing_actix_web::TracingLogger;

use actix_files::Files;
use frontend::app::*;
use leptos::*;
use leptos_actix::{generate_route_list, LeptosRoutes};
use service_utils::{
    db::pgschema_manager::PgSchemaManager,
    db::utils::init_pool_manager,
    helpers::{get_from_env_or_default, get_from_env_unsafe, get_pod_info},
    middlewares::{
        app_scope::AppExecutionScopeMiddlewareFactory, tenant::TenantMiddlewareFactory,
    },
    service::types::{AppEnv, AppScope, AppState, ExperimentationFlags},
};

#[actix_web::get("favicon.ico")]
async fn favicon(
    leptos_options: actix_web::web::Data<leptos::LeptosOptions>,
) -> actix_web::Result<actix_files::NamedFile> {
    let leptos_options = leptos_options.into_inner();
    let site_root = &leptos_options.site_root;
    Ok(actix_files::NamedFile::open(format!(
        "{site_root}/favicon.ico"
    ))?)
}

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
    let admin_token = env::var("ADMIN_TOKEN").expect("Admin token is not set!");
    let cac_host: String = get_from_env_unsafe("CAC_HOST").expect("CAC host is not set");
    let cac_version: String = get_from_env_unsafe("CONTEXT_AWARE_CONFIG_VERSION")
        .expect("CONTEXT_AWARE_CONFIG_VERSION is not set");
    let max_pool_size = get_from_env_or_default("MAX_DB_CONNECTION_POOL_SIZE", 3);

    let app_env: AppEnv = get_from_env_unsafe("APP_ENV").expect("APP_ENV is not set");
    let enable_tenant_and_scope: bool = get_from_env_unsafe("ENABLE_TENANT_AND_SCOPE")
        .expect("ENABLE_TENANT_AND_SCOPE is not set");
    let tenants: HashSet<String> = get_from_env_unsafe::<String>("TENANTS")
        .expect("TENANTS is not set")
        .split(",")
        .map(|tenant| tenant.to_string())
        .collect::<HashSet<String>>();
    let tenant_middleware_exclusion_list =
        get_from_env_unsafe::<String>("TENANT_MIDDLEWARE_EXCLUSION_LIST")
            .expect("TENANT_MIDDLEWARE_EXCLUSION_LIST is not set")
            .split(",")
            .map(String::from)
            .collect::<HashSet<String>>();

    let string_to_int = |s: &String| -> i32 {
        s.chars()
            .map(|i| (i as i32) & rand::random::<i32>())
            .fold(0, i32::wrapping_add)
    };

    let schema_manager: PgSchemaManager = init_pool_manager(
        tenants.clone(),
        enable_tenant_and_scope,
        app_env,
        max_pool_size,
    )
    .await;

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

    /* Frontend configurations */
    let conf = get_configuration(Some("Cargo.toml")).await.unwrap();
    // Generate the list of routes in your Leptos App
    let routes = generate_route_list(|| view! { <App/> });

    HttpServer::new(move || {
        let leptos_options = &conf.leptos_options;
        let site_root = &leptos_options.site_root;
        App::new()
            .wrap(DashboardAuth::default(authenticated_routes()))
            .wrap(TenantMiddlewareFactory)
            .wrap(middlewares::cors())
            .wrap(logger::GoldenSignalFactory)
            .wrap(TracingLogger::<CustomRootSpanBuilder>::new())
            .app_data(Data::new(AppState {
                db_pool: schema_manager.clone(),
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
                app_env: app_env.to_owned(),
                enable_tenant_and_scope: enable_tenant_and_scope.to_owned(),
                tenants: tenants.to_owned(),
                tenant_middleware_exclusion_list: tenant_middleware_exclusion_list
                    .to_owned(),
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
                    .wrap(AppExecutionScopeMiddlewareFactory::new(AppScope::CAC))
                    .service(context::endpoints()),
            )
            .service(
                scope("/dimension")
                    .wrap(AppExecutionScopeMiddlewareFactory::new(AppScope::CAC))
                    .service(dimension::endpoints()),
            )
            .service(
                scope("/default-config")
                    .wrap(AppExecutionScopeMiddlewareFactory::new(AppScope::CAC))
                    .service(default_config::endpoints()),
            )
            .service(
                scope("/config")
                    .wrap(AuditHeader::new(TableName::Contexts))
                    .wrap(AppExecutionScopeMiddlewareFactory::new(AppScope::CAC))
                    .service(config::endpoints()),
            )
            .service(
                scope("/audit")
                    .wrap(AppExecutionScopeMiddlewareFactory::new(AppScope::CAC))
                    .service(audit_log::endpoints()),
            )
            .service(
                external::endpoints(experiments::endpoints(scope("/experiments"))).wrap(
                    AppExecutionScopeMiddlewareFactory::new(AppScope::EXPERIMENTATION),
                ),
            )
            .route("/api/{tail:.*}", leptos_actix::handle_server_fns())
            // serve other assets from the `assets` directory
            .service(Files::new("/assets", format!("{site_root}/assets")))
            // serve JS/WASM/CSS from `pkg`
            .service(Files::new("/pkg", format!("{site_root}/pkg")))
            // serve the favicon from /favicon.ico
            .service(web::redirect("/", "/admin"))
            .service(favicon)
            .leptos_routes(
                leptos_options.to_owned(),
                routes.to_owned(),
                || view! { <App/> },
            )
            .app_data(Data::new(leptos_options.to_owned()))
    })
    .bind(("0.0.0.0", 8080))?
    .workers(5)
    .keep_alive(Duration::from_secs(
        get_from_env_unsafe("ACTIX_KEEP_ALIVE").unwrap_or(120),
    ))
    .run()
    .await
}

#[actix_web::get("/style.css")]
async fn css() -> impl actix_web::Responder {
    actix_files::NamedFile::open_async("./style/output.css").await
}

fn authenticated_routes() -> AuthenticatedRouteList {
    let mut route_vector: Vec<(&str, AuthenticatedRoute)> = Vec::new();
    route_vector.append(&mut auth::contexts::authenticated_routes());
    route_vector.append(&mut auth::default_config::authenticated_routes());
    route_vector.append(&mut auth::dimension::authenticated_routes());
    route_vector.append(&mut auth::experiments::authenticated_routes());
    AuthenticatedRouteList::from(route_vector)
}
