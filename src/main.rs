mod v1;

use dotenv;
use std::{env, io::Result};

use actix_web::{
    middleware::Logger, web::get, web::scope, web::Data, App, HttpResponse, HttpServer,
};

use service_utils::{
    db::utils::get_pool,
    service::types::AppState,
};

use v1::{api::*, helpers::get_default_config_validation_schema};

#[actix_web::main]
async fn main() -> Result<()> {
    dotenv::dotenv().ok();
    env_logger::init();
    let pool = get_pool().await;
    let admin_token = env::var("ADMIN_TOKEN").expect("Admin token is not set!");
    HttpServer::new(move || {
        let logger: Logger = Logger::default();
        App::new()
            .app_data(Data::new(AppState {
                db_pool: pool.clone(),
                default_config_validation_schema: get_default_config_validation_schema(),
		admin_token: admin_token.to_owned()
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
