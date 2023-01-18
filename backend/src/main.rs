mod api;
mod models;
mod db;
mod messages;
mod handlers;
mod utils;

use api::primary::{
    global_config::{
        get_global_config_key,
        get_global_config,
        post_config_key_value,
    },

    dimensions::{
        get_dimensions,
        get_dimension_key,
        post_dimension
    },
    overrides::{
        post_override,
        delete_override,
        get_override,
    },
    contexts::{
        post_context,
        delete_context,
        get_context
    },
    context_overrides::{
        post_ctx_override,
        delete_ctx_override,
        get_ctx_override,
    }
};

use api::derived::{
    config::get_config,
    context_override::add_new_context_override
};

// use crate::utils::validations::just_for_test;

use dotenv;
use std::env;
use std::io::Result;

use db::utils::{get_pool, AppState, DbActor};
use actix::SyncArbiter;
use actix_web::{HttpServer, App, web::scope, middleware::Logger,web::Data};


#[actix_web::main]
async fn main() -> Result<()> {
    // just_for_test();
    dotenv::dotenv().ok();
    std::env::set_var("RUST_LOG", "debug");
    std::env::set_var("RUST_BACKTRACE", "1");
    env_logger::init();
    let db_url: String = env::var("DATABASE_URL").expect("DATABASE_URL must be set in environment");
    let pool = get_pool(&db_url);
    let db_addr = SyncArbiter::start(5, move || DbActor(pool.clone()));
    HttpServer::new(move || {
        let logger: Logger = Logger::default();
        App::new()
        .app_data(Data::new(AppState {db: db_addr.clone()}))
        .wrap(logger)

/***************************** Primary api routes *****************************/
        .service(
            scope("/global_config")
                .service(get_global_config)
                .service(get_global_config_key)
                .service(post_config_key_value)
        )
        .service(
            scope("/dimensions")
                .service(get_dimensions)
                .service(get_dimension_key)
                .service(post_dimension)
        )
        .service(
            scope("/context_overrides")
                .service(post_ctx_override)
                .service(delete_ctx_override)
                .service(get_ctx_override)
        )
        .service(
            scope("/override")
                .service(post_override)
                .service(delete_override)
                .service(get_override)
        ).service(
            scope("/context")
                .service(post_context)
                .service(delete_context)
                .service(get_context)

        )

/***************************** Derived api routes *****************************/
        .service(
            scope("/config")
                .service(get_config)
        )
        .service(
            scope("add_context_overrides")
                .service(add_new_context_override)
        )
    })
    .bind(("0.0.0.0", 8080))?
    .workers(5)
    .run()
    .await
}
