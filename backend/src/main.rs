mod api;
mod model;
use api::global_config::{
    get_key,
    get_config,
    post_config_key_value,
};

use actix_web::{HttpServer, App, web::scope, middleware::Logger};

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    std::env::set_var("RUST_LOG", "debug");
    std::env::set_var("RUST_BACKTRACE", "1");
    env_logger::init();
    HttpServer::new(move || {
        let logger: Logger = Logger::default();
        App::new()
        .wrap(logger)
        .service(
            scope("/global_config")
                .service(get_config)
                .service(get_key)
                .service(post_config_key_value)
        )
    })
    .bind(("127.0.0.1", 8000))?
    .workers(5)
    .run()
    .await
}
