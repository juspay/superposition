//! Health probe endpoints mounted on the main app port.
//!
//! Paths are added to `tenant_middleware_exclusion_list` so they bypass auth.

use actix_web::{HttpResponse, Scope, web};

pub const HEALTHZ: &str = "/healthz";
pub const LIVEZ: &str = "/livez";
pub const READYZ: &str = "/readyz";

/// Returns the Actix scope to mount on the main app:
/// `App::new().service(observability::health_endpoints())`.
pub fn health_endpoints() -> Scope {
    web::scope("")
        .route(HEALTHZ, web::get().to(healthz))
        .route(LIVEZ, web::get().to(livez))
        .route(READYZ, web::get().to(readyz))
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
        let app = test::init_service(App::new().service(health_endpoints())).await;
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
}
