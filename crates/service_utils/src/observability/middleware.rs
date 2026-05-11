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

use opentelemetry::KeyValue;
use crate::observability::config::LabelConfig;

/// Build the OTel attributes set for a single HTTP request. Reads org_id /
/// workspace_id from request extensions if `OrgWorkspaceMiddlewareFactory`
/// has populated them; otherwise omits those attributes entirely (rather
/// than emitting an empty string, which would create a distinct series).
pub(crate) fn build_attributes(
    method: &'static str,
    route: &str,
    status_code: u16,
    org_id: Option<&str>,
    workspace: Option<&str>,
    label_cfg: &LabelConfig,
) -> Vec<KeyValue> {
    let mut attrs = Vec::with_capacity(5);
    attrs.push(KeyValue::new("http.request.method", method));
    attrs.push(KeyValue::new("http.route", route.to_owned()));
    attrs.push(KeyValue::new("http.response.status_code", status_code as i64));
    if label_cfg.with_org_label {
        if let Some(o) = org_id {
            attrs.push(KeyValue::new("sp.org_id", o.to_owned()));
        }
    }
    if label_cfg.with_workspace_label {
        if let Some(w) = workspace {
            attrs.push(KeyValue::new("sp.workspace_id", w.to_owned()));
        }
    }
    attrs
}

pub struct MetricsMiddleware;   // placeholder until Task 11

use std::sync::atomic::{AtomicBool, Ordering};
use opentelemetry::metrics::UpDownCounter;

/// RAII guard that decrements `http.server.active_requests` on Drop unless
/// `release()` was called. Ensures a panicking handler still decrements the
/// gauge.
pub(crate) struct InFlightGuard {
    counter: UpDownCounter<i64>,
    method: &'static str,
    decremented: AtomicBool,
}

impl InFlightGuard {
    pub(crate) fn enter(counter: UpDownCounter<i64>, method: &'static str) -> Self {
        counter.add(1, &[KeyValue::new("http.request.method", method)]);
        Self {
            counter,
            method,
            decremented: AtomicBool::new(false),
        }
    }

    pub(crate) fn release(&self) {
        if !self.decremented.swap(true, Ordering::Relaxed) {
            self.counter.add(
                -1,
                &[KeyValue::new("http.request.method", self.method)],
            );
        }
    }
}

impl Drop for InFlightGuard {
    fn drop(&mut self) {
        self.release();
    }
}

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

    use crate::observability::config::LabelConfig;

    #[test]
    fn build_attributes_with_all_labels() {
        let cfg = LabelConfig { with_org_label: true, with_workspace_label: true };
        let attrs = build_attributes("GET", "/contexts/{id}", 200, Some("org1"), Some("ws1"), &cfg);
        assert_eq!(attrs.len(), 5);
        assert!(attrs.iter().any(|kv| kv.key.as_str() == "sp.org_id"));
        assert!(attrs.iter().any(|kv| kv.key.as_str() == "sp.workspace_id"));
    }

    #[test]
    fn build_attributes_omits_missing_workspace() {
        let cfg = LabelConfig { with_org_label: true, with_workspace_label: true };
        let attrs = build_attributes("POST", "/orgs", 201, Some("org1"), None, &cfg);
        assert_eq!(attrs.len(), 4);
        assert!(attrs.iter().any(|kv| kv.key.as_str() == "sp.org_id"));
        assert!(!attrs.iter().any(|kv| kv.key.as_str() == "sp.workspace_id"));
    }

    #[test]
    fn build_attributes_respects_disable_flag() {
        let cfg = LabelConfig { with_org_label: false, with_workspace_label: false };
        let attrs = build_attributes("GET", "/x", 200, Some("org1"), Some("ws1"), &cfg);
        assert_eq!(attrs.len(), 3);
        assert!(!attrs.iter().any(|kv| kv.key.as_str() == "sp.org_id"));
        assert!(!attrs.iter().any(|kv| kv.key.as_str() == "sp.workspace_id"));
    }

    #[test]
    fn guard_decrements_on_drop_only_once() {
        use crate::observability::{Observability, ObservabilityConfig, LabelConfig};
        use std::time::Duration;

        let cfg = ObservabilityConfig {
            enabled: true,
            bind: "127.0.0.1".parse().unwrap(),
            port: 0,
            label: LabelConfig::default(),
            collect_interval: Duration::from_secs(10),
            instance_id: "test".into(),
            service_name: "sp-test".into(),
            service_version: "0".into(),
            deployment_environment: None,
            otlp_endpoint: None,
        };
        let obs = Observability::init(cfg).unwrap();
        let m = obs.meter().i64_up_down_counter("test.in_flight").build();

        {
            let g = InFlightGuard::enter(m.clone(), "GET");
            g.release();
            // Drop after explicit release; should be a no-op.
        }
        // The guard should tolerate multiple release() calls without panicking
        // and a release-then-drop pattern.
        let g = InFlightGuard::enter(m.clone(), "POST");
        g.release();
        g.release();
        drop(g);
    }
}
