use actix_web::{
    get, rt,
    web::{get, Data, Path},
    App, HttpResponse, HttpServer,
};

use serde_json::json;
use superposition_client as exp;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let client_configuration = exp::Config {
        tenant: "tenant".to_string(),
        hostname: "http://localhost:8080".to_string(),
        poll_frequency: 10,
    };
    let client = std::sync::Arc::new(exp::Client::new(client_configuration));
    rt::spawn(client.clone().run_polling_updates());
    HttpServer::new(move || {
        App::new()
            .app_data(Data::new(client.clone()))
            .route(
                "/health",
                get().to(|| async { HttpResponse::Ok().body("Health is good :D") }),
            )
            .service(get_variants)
    })
    .bind(("127.0.0.1", 8083))?
    .run()
    .await
}

#[get("/variants/{client_id}/{platform}/{toss}")]
async fn get_variants(
    state: Data<exp::Client>,
    path: Path<(String, String, i8)>,
) -> HttpResponse {
    let (client_id, platform, toss) = path.into_inner();
    println!("client state on the server = {:?}", state);
    let contexts = json!({
        "clientId": client_id,
        "os": platform
    });
    let variant = state.get_applicable_variant(&contexts, toss).await;
    println!("variant value: {:?}", variant);
    HttpResponse::Ok().body("check your console")
}
