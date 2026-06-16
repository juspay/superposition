//! Separate HttpServer that exposes /metrics on SUPERPOSITION_METRICS_PORT.

use std::{net::SocketAddr, sync::Arc};

use actix_web::{App, HttpResponse, HttpServer, dev::Server, web};
use prometheus::{Encoder, Registry, TextEncoder};

/// Spawn an HttpServer on `bind` whose only route is `GET /metrics`. Returns
/// the actix `Server` handle so the caller can `await` it concurrently with
/// the main app.
pub fn spawn_metrics_server(
    registry: Arc<Registry>,
    bind: SocketAddr,
) -> std::io::Result<Server> {
    let registry_data = web::Data::new(registry);
    Ok(HttpServer::new(move || {
        App::new()
            .app_data(registry_data.clone())
            .route("/metrics", web::get().to(scrape))
    })
    .workers(1)
    .bind(bind)?
    .run())
}

async fn scrape(registry: web::Data<Arc<Registry>>) -> HttpResponse {
    let encoder = TextEncoder::new();
    let metric_families = registry.gather();
    let mut buf = Vec::new();
    if let Err(e) = encoder.encode(&metric_families, &mut buf) {
        return HttpResponse::InternalServerError().body(format!("encode error: {e}"));
    }
    HttpResponse::Ok()
        .content_type(encoder.format_type())
        .body(buf)
}

#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::{App, http::StatusCode, test};

    #[actix_web::test]
    async fn scrape_endpoint_returns_text_plain() {
        let registry = Arc::new(Registry::new());
        let app = test::init_service(
            App::new()
                .app_data(web::Data::new(registry.clone()))
                .route("/metrics", web::get().to(scrape)),
        )
        .await;
        let req = test::TestRequest::get().uri("/metrics").to_request();
        let resp = test::call_service(&app, req).await;
        assert_eq!(resp.status(), StatusCode::OK);
        let ct = resp
            .headers()
            .get("content-type")
            .unwrap()
            .to_str()
            .unwrap();
        assert!(ct.starts_with("text/plain"), "got {ct}");
    }
}
