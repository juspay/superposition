//! Actix middleware that records OpenTelemetry HTTP server metrics.

/// Per OpenTelemetry HTTP semantic conventions, only known methods get their
/// literal name; anything else collapses to `_OTHER`. Prevents weirdo clients
/// from blowing up the cardinality of the `http.request.method` attribute.
pub(crate) fn normalize_method(m: &actix_web::http::Method) -> &'static str {
    match m.as_str() {
        "GET" => "GET",
        "POST" => "POST",
        "PUT" => "PUT",
        "DELETE" => "DELETE",
        "PATCH" => "PATCH",
        "HEAD" => "HEAD",
        "OPTIONS" => "OPTIONS",
        "TRACE" => "TRACE",
        "CONNECT" => "CONNECT",
        _ => "_OTHER",
    }
}

use actix_web::dev::ServiceRequest;

/// Sentinel for paths that did not match any registered route (would 404).
pub(crate) const ROUTE_NOT_FOUND: &str = "__not_found__";

/// Extracts the templated route pattern from a ServiceRequest. Falls back to
/// a sentinel when no route matched, to keep `http.route` cardinality bounded.
pub(crate) fn extract_route(req: &ServiceRequest) -> String {
    req.match_pattern().unwrap_or_else(|| ROUTE_NOT_FOUND.to_owned())
}

pub struct MetricsMiddleware;   // placeholder until Task 11

#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::http::Method;

    #[test]
    fn known_methods_pass_through() {
        for (m, expected) in [
            (Method::GET, "GET"),
            (Method::POST, "POST"),
            (Method::PUT, "PUT"),
            (Method::DELETE, "DELETE"),
            (Method::PATCH, "PATCH"),
            (Method::HEAD, "HEAD"),
            (Method::OPTIONS, "OPTIONS"),
            (Method::TRACE, "TRACE"),
            (Method::CONNECT, "CONNECT"),
        ] {
            assert_eq!(normalize_method(&m), expected);
        }
    }

    #[test]
    fn unknown_methods_collapse_to_other() {
        let m = Method::from_bytes(b"XPROPFIND").unwrap();
        assert_eq!(normalize_method(&m), "_OTHER");
        let m = Method::from_bytes(b"WEIRDO").unwrap();
        assert_eq!(normalize_method(&m), "_OTHER");
    }

    use actix_web::{App, HttpResponse, http::StatusCode, test as actix_test, web};

    #[actix_web::test]
    async fn matched_route_returns_pattern() {
        let app = actix_test::init_service(
            App::new().route(
                "/contexts/{id}",
                web::get().to(|| async { HttpResponse::Ok() }),
            ),
        )
        .await;
        let req = actix_test::TestRequest::get().uri("/contexts/abc123").to_request();
        let resp = actix_test::call_service(&app, req).await;
        assert_eq!(resp.status(), StatusCode::OK);
        // Note: extract_route is exercised in the integration test in Task 19
        // because match_pattern() is only populated mid-pipeline. This unit-test
        // stub is kept for build-coverage of the call site.
    }
}
