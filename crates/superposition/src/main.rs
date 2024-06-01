use actix_web::dev::Service;
use actix_web::HttpMessage;
use actix_web::{web, web::get, web::scope, web::Data, App, HttpResponse, HttpServer};
use context_aware_config::api::*;
use context_aware_config::helpers::{
    get_default_config_validation_schema, get_meta_schema,
};
use experimentation_platform::api::*;
use std::{collections::HashSet, io::Result};
use superposition_types::User;

use snowflake::SnowflakeIdGenerator;
use std::{sync::Mutex, time::Duration};

use actix_files::Files;
use frontend::app::*;
use frontend::types::Envs as UIEnvs;
use leptos::*;
use leptos_actix::{generate_route_list, LeptosRoutes};
use service_utils::{
    db::pgschema_manager::PgSchemaManager,
    db::utils::init_pool_manager,
    helpers::{get_from_env_or_default, get_from_env_unsafe},
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
    env_logger::init();
    let service_prefix: String =
        get_from_env_unsafe("SERVICE_PREFIX").expect("SERVICE_PREFIX is not set");

    /*
        Reading from a env returns a String at best we cannot obtain a &'static str from it,
        which seems logical as it not known at compiletime, and there is no straightforward way to do this.

        Leptos' Router component base prop type is &'static str, since service_prefix is of String type
        we cannot give this as base value.

        This can be solved, if somehow we can tell rust that this String is going to live for entirety of the process,
        here comes Box::leak() to our rescue, which keeps the value in the memory for the entire process lifetime,
        this also enables to borrow the String value as &'static str .
    */
    let service_prefix_str: &'static str = Box::leak(service_prefix.into_boxed_str());
    let base = match service_prefix_str {
        "" | "/" => "".to_owned(),
        prefix => "/".to_owned() + prefix,
    };

    let cac_host: String = get_from_env_unsafe("CAC_HOST").expect("CAC host is not set");
    let cac_port: u16 = get_from_env_unsafe("PORT").unwrap_or(8080);
    let cac_version: String = get_from_env_unsafe("CONTEXT_AWARE_CONFIG_VERSION")
        .expect("CONTEXT_AWARE_CONFIG_VERSION is not set");
    let max_pool_size = get_from_env_or_default("MAX_DB_CONNECTION_POOL_SIZE", 2);

    let api_host: String =
        get_from_env_unsafe("API_HOSTNAME").expect("API_HOSTNAME is not set");
    let app_env: AppEnv = get_from_env_unsafe("APP_ENV").expect("APP_ENV is not set");
    let enable_tenant_and_scope: bool = get_from_env_unsafe("ENABLE_TENANT_AND_SCOPE")
        .expect("ENABLE_TENANT_AND_SCOPE is not set");
    let tenants: HashSet<String> = get_from_env_unsafe::<String>("TENANTS")
        .expect("TENANTS is not set")
        .split(',')
        .map(|tenant| tenant.to_string())
        .collect::<HashSet<String>>();
    let tenant_middleware_exclusion_list =
        get_from_env_unsafe::<String>("TENANT_MIDDLEWARE_EXCLUSION_LIST")
            .expect("TENANT_MIDDLEWARE_EXCLUSION_LIST is not set")
            .split(',')
            .map(String::from)
            .collect::<HashSet<String>>();

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
    let ui_redirect_path = match tenants.iter().next() {
        Some(tenant) => format!("{}/admin/{}/default-config", base, tenant),
        None => String::from("/admin"),
    };

    let ui_envs = UIEnvs {
        service_prefix: service_prefix_str,
        tenants: tenants.clone().into_iter().collect::<Vec<String>>(),
        host: api_host.clone(),
    };

    let routes_ui_envs = ui_envs.clone();

    let conf = get_configuration(Some("Cargo.toml")).await.unwrap();
    // Generate the list of routes in your Leptos App
    let routes = generate_route_list(move || {
        view! { <App app_envs=routes_ui_envs.clone()/> }
    });

    HttpServer::new(move || {
        let leptos_options = &conf.leptos_options;
        let site_root = &leptos_options.site_root;
        let leptos_envs = ui_envs.clone();
        let cac_host = cac_host.to_owned() + base.as_str();
        App::new()
            .wrap_fn(|req, srv| {
                let user = User::default();
                req.extensions_mut().insert::<User>(user);
                srv.call(req)
            })
            .wrap(TenantMiddlewareFactory)
            .app_data(Data::new(AppState {
                db_pool: schema_manager.clone(),
                default_config_validation_schema: get_default_config_validation_schema(),
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

                snowflake_generator: Mutex::new(SnowflakeIdGenerator::new(1,1)),
                meta_schema: get_meta_schema(),
                app_env: app_env.to_owned(),
                enable_tenant_and_scope: enable_tenant_and_scope.to_owned(),
                tenants: tenants.to_owned(),
                tenant_middleware_exclusion_list: tenant_middleware_exclusion_list
                    .to_owned(),
                service_prefix: service_prefix_str.to_owned(),
            }))
            .wrap(
                actix_web::middleware::DefaultHeaders::new()
                    .add(("X-SERVER-VERSION", cac_version.to_string()))
                    .add(("Cache-Control", "no-store".to_string()))
            )
            .service(web::redirect("/", ui_redirect_path.to_string()))
            .service(web::redirect("/admin", ui_redirect_path.to_string()))
            .service(web::redirect("/admin/{tenant}/", "default-config"))
            .leptos_routes(
                leptos_options.to_owned(),
                routes.to_owned(),
                move || view! { <App app_envs=leptos_envs.clone()/> },
            )
            .service(
                scope(&base)
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
                            .wrap(AppExecutionScopeMiddlewareFactory::new(AppScope::CAC))
                            .service(config::endpoints()),
                    )
                    .service(
                        scope("/audit")
                            .wrap(AppExecutionScopeMiddlewareFactory::new(AppScope::CAC))
                            .service(audit_log::endpoints()),
                    )
                    .service(
                        scope("/function")
                            .wrap(AppExecutionScopeMiddlewareFactory::new(AppScope::CAC))
                            .service(functions::endpoints()),
                    )
                    .service(
                        experiments::endpoints(scope("/experiments")).wrap(
                            AppExecutionScopeMiddlewareFactory::new(AppScope::EXPERIMENTATION),
                        ),
                    )
                    /***************************** UI Routes ******************************/
                    .route("/fxn/{tail:.*}", leptos_actix::handle_server_fns())
                    // serve JS/WASM/CSS from `pkg`
                    .service(Files::new("/pkg", format!("{site_root}/pkg")))
                    // serve other assets from the `assets` directory
                    .service(Files::new("/assets", site_root.to_string()))
                    // serve the favicon from /favicon.ico
            )
            .route(
                "/health",
                get().to(|| async { HttpResponse::Ok().body("Health is good :D") }),
            )
            .app_data(Data::new(leptos_options.to_owned()))
    })
    .bind(("0.0.0.0", cac_port))?
    .workers(5)
    .keep_alive(Duration::from_secs(
        get_from_env_unsafe("ACTIX_KEEP_ALIVE").unwrap_or(120),
    ))
    .run()
    .await
}
