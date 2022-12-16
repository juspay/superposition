use actix_web::{
    get, middleware::Logger, App, HttpRequest, HttpResponse, HttpServer, Responder,
};

#[get("/")]
async fn hello() -> impl Responder {
    HttpResponse::Ok().body("Hello world!")
}

#[get("/{name}")]
async fn greet_user(req: HttpRequest) -> impl Responder {
    let name = req.match_info().get("name").unwrap_or("World");
    format!("Hello {}!", &name)
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {


    std::env::set_var("RUST_LOG", "debug");
    std::env::set_var("RUST_BACKTRACE", "1");

    HttpServer::new(|| {
        let logger = Logger::default();
        App::new().wrap(logger).service(hello).service(greet_user)
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
