//! Health probe endpoints mounted on the main app port.
//!
//! Paths are added to `tenant_middleware_exclusion_list` so they bypass auth.

use actix_web::{HttpResponse, web};

pub const HEALTHZ: &str = "/healthz";
pub const LIVEZ: &str = "/livez";
pub const READYZ: &str = "/readyz";

/// Registers `/healthz`, `/livez`, `/readyz` directly on the App's
/// `ServiceConfig`. Use as:
///
/// ```ignore
/// App::new().configure(observability::configure_health_endpoints)
/// ```
///
/// Registering at App root via `.configure` (rather than via a
/// `web::scope("")` returned from a helper) is intentional: an empty-prefix
/// scope matches every request path and would return 404 for any path that
/// is not one of the three routes inside it, shadowing later services.
pub fn configure_health_endpoints(cfg: &mut web::ServiceConfig) {
    cfg.route(HEALTHZ, web::get().to(healthz))
        .route(LIVEZ, web::get().to(livez))
        .route(READYZ, web::get().to(readyz));
}

/// Paths to add to the auth exclusion list.
pub fn health_endpoint_paths() -> &'static [&'static str] {
    &[HEALTHZ, LIVEZ, READYZ]
}

async fn healthz() -> HttpResponse {
    HttpResponse::Ok().content_type("text/plain; charset=utf-8").body("ok")
}

async fn livez() -> HttpResponse {
    HttpResponse::Ok().content_type("text/plain; charset=utf-8").body("ok")
}

async fn readyz() -> HttpResponse {
    // v1: same as livez. Future: check DB pool, Redis, dependencies.
    HttpResponse::Ok().content_type("text/plain; charset=utf-8").body("ok")
}

#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::{App, http::StatusCode, test};

    #[actix_web::test]
    async fn each_endpoint_returns_200_ok() {
        let app =
            test::init_service(App::new().configure(configure_health_endpoints)).await;
        for path in health_endpoint_paths() {
            let req = test::TestRequest::get().uri(path).to_request();
            let resp = test::call_service(&app, req).await;
            assert_eq!(resp.status(), StatusCode::OK, "GET {path}");
        }
    }

    #[actix_web::test]
    async fn paths_list_matches_routes() {
        let paths = health_endpoint_paths();
        assert_eq!(paths, &[HEALTHZ, LIVEZ, READYZ]);
    }

    /// Regression: ensure adding the health endpoints does NOT shadow other
    /// routes registered on the same App. The prior `web::scope("")` shape
    /// matched every path and returned 404 for non-health paths.
    #[actix_web::test]
    async fn does_not_shadow_other_routes() {
        let app = test::init_service(
            App::new()
                .configure(configure_health_endpoints)
                .route("/other", web::get().to(|| async { HttpResponse::Ok().body("other") })),
        )
        .await;

        let req = test::TestRequest::get().uri("/other").to_request();
        let resp = test::call_service(&app, req).await;
        assert_eq!(resp.status(), StatusCode::OK);

        let req = test::TestRequest::get().uri("/healthz").to_request();
        let resp = test::call_service(&app, req).await;
        assert_eq!(resp.status(), StatusCode::OK);
    }
}
