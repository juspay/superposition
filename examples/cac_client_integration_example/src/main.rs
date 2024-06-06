use actix_web::{
    get, rt,
    web::{get, Query},
    App, HttpRequest, HttpResponse, HttpServer,
};

use cac_client as cac;
use serde_json::{Map, Value};
use std::time::Duration;

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    rt::spawn(
        cac::CLIENT_FACTORY
            .create_client(
                "dev".to_string(),
                Duration::new(10, 0),
                "http://localhost:8080".into(),
            )
            .await
            .expect(format!("{}: Failed to acquire cac_client", "dev").as_str())
            .clone()
            .run_polling_updates(),
    );
    HttpServer::new(move || {
        App::new()
            .route(
                "/health",
                get().to(|| async { HttpResponse::Ok().body("Health is good :D") }),
            )
            .service(get_last_modified)
            .service(get_full_config)
            .service(get_resolved_config)
            .service(get_default_config)
    })
    .bind(("127.0.0.1", 8084))?
    .run()
    .await
}

#[get("/last-modified")]
async fn get_last_modified() -> HttpResponse {
    let client = cac::CLIENT_FACTORY
        .get_client("dev".into())
        .await
        .expect("No client found for dev tenant");
    println!(
        "Last modified time of configs: {:?}",
        client.get_last_modified().await
    );
    HttpResponse::Ok().body("check your console")
}

#[get("/full-config")]
async fn get_full_config(request: HttpRequest) -> HttpResponse {
    let client = cac::CLIENT_FACTORY
        .get_client("dev".into())
        .await
        .expect("No client found for dev tenant");
    let query_params = Query::<Map<String, Value>>::from_query(request.query_string())
        .map(Query::into_inner)
        .unwrap_or(Map::new());
    let prefix = query_params
        .get("prefix")
        .and_then(|item| item.as_str())
        .and_then(|item| {
            Some(item.split(',').map(str::to_string).collect::<Vec<String>>())
        });
    println!(
        "full config with filters: {:?}",
        client
            .get_full_config_state_with_filter(Some(query_params), prefix)
            .await
    );
    HttpResponse::Ok().body("check your console")
}

#[get("/resolved-config")]
async fn get_resolved_config(request: HttpRequest) -> HttpResponse {
    let client = cac::CLIENT_FACTORY
        .get_client("dev".into())
        .await
        .expect("No client found for dev tenant");
    let query_params = Query::<Map<String, Value>>::from_query(request.query_string())
        .map(Query::into_inner)
        .unwrap_or(Map::new());
    println!(
        "resolved config with filters: {:?}",
        client
            .get_resolved_config(query_params, None, cac_client::MergeStrategy::MERGE)
            .await
    );
    HttpResponse::Ok().body("check your console")
}

#[get("/default-config")]
async fn get_default_config() -> HttpResponse {
    let client = cac::CLIENT_FACTORY
        .get_client("dev".into())
        .await
        .expect("No client found for dev tenant");
    println!(
        "default config: {:?}",
        client.get_default_config(None).await
    );
    HttpResponse::Ok().body("check your console")
}
