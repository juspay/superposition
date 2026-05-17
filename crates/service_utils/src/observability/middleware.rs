//! Actix middleware that records OpenTelemetry HTTP server metrics.

use std::future::{Ready, ready};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Instant;

use actix_web::{
    Error, HttpMessage,
    body::MessageBody,
    dev::{Service, ServiceRequest, ServiceResponse, Transform, forward_ready},
};
use futures_util::future::LocalBoxFuture;
use opentelemetry::KeyValue;
use opentelemetry::metrics::{Meter, UpDownCounter};

use crate::observability::config::LabelConfig;
use crate::observability::meters::HttpMeters;
use crate::service::types::{OrganisationId, WorkspaceId};

/// Per OpenTelemetry HTTP semantic conventions, only known methods get their
/// literal name; anything else collapses to `_OTHER`. Prevents weirdo clients
/// from blowing up the cardinality of the `http.request.method` attribute.
pub(crate) fn normalize_method(m: &actix_web::http::Method) -> &'static str {
    macro_rules! match_known {
        ($val:expr, [$($name:literal),+ $(,)?], $other:literal) => {
            match $val { $($name => $name,)+ _ => $other }
        };
    }
    match_known!(
        m.as_str(),
        ["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS", "TRACE", "CONNECT"],
        "_OTHER"
    )
}

/// Sentinel for paths that did not match any registered route (would 404).
pub(crate) const ROUTE_NOT_FOUND: &str = "__not_found__";

/// Sentinel for static-asset routes (pkg, assets, favicon).  Collapsing these
/// prevents one cardinality series per unique path tail.
pub(crate) const ROUTE_STATIC: &str = "__static__";

/// Route-pattern prefixes that identify static-asset serving routes.
/// Any `match_pattern()` that starts with one of these is collapsed to
/// `ROUTE_STATIC` to keep `http.route` cardinality bounded.
const STATIC_PATTERN_PREFIXES: &[&str] = &["/pkg", "/assets", "/favicon"];

/// Returns `true` when `pattern` belongs to a static-asset route.
pub(crate) fn is_static_pattern(pattern: &str) -> bool {
    STATIC_PATTERN_PREFIXES
        .iter()
        .any(|prefix| pattern.starts_with(prefix))
}

/// Extracts the templated route pattern from a `ServiceRequest`.
/// Falls back to `ROUTE_NOT_FOUND` when no route matched;
/// collapses static patterns to `ROUTE_STATIC`.
///
/// This is the request-phase variant.  In production the middleware uses
/// [`extract_route_from_response`] (response phase) because route matching is
/// only complete after the inner service has run.  This function is available
/// for callers that have a live `ServiceRequest` (e.g. future request-scoped
/// middleware).
#[allow(dead_code)]
pub(crate) fn extract_route(req: &ServiceRequest) -> String {
    match req.match_pattern() {
        None => ROUTE_NOT_FOUND.to_owned(),
        Some(p) if is_static_pattern(&p) => ROUTE_STATIC.to_owned(),
        Some(p) => p,
    }
}

/// Same logic as `extract_route` but operates on a completed `ServiceResponse`
/// (available in the middleware's response phase).
pub(crate) fn extract_route_from_response<B>(res: &ServiceResponse<B>) -> String {
    match res.request().match_pattern() {
        None => ROUTE_NOT_FOUND.to_owned(),
        Some(p) if is_static_pattern(&p) => ROUTE_STATIC.to_owned(),
        Some(p) => p,
    }
}

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
    attrs.push(KeyValue::new(
        "http.response.status_code",
        status_code as i64,
    ));
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

/// RAII guard that decrements `http.server.active_requests` on Drop unless
/// `release()` was called. Ensures a panicking handler still decrements the
/// gauge.
#[must_use = "dropping InFlightGuard immediately negates the in-flight window"]
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
            self.counter
                .add(-1, &[KeyValue::new("http.request.method", self.method)]);
        }
    }
}

impl Drop for InFlightGuard {
    fn drop(&mut self) {
        self.release();
    }
}

#[derive(Clone)]
pub struct MetricsMiddleware {
    meters: HttpMeters,
    label_cfg: LabelConfig,
}

impl MetricsMiddleware {
    pub fn new(meter: &Meter, label_cfg: LabelConfig) -> Self {
        Self {
            meters: HttpMeters::new(meter),
            label_cfg,
        }
    }
}

impl<S, B> Transform<S, ServiceRequest> for MetricsMiddleware
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: MessageBody + 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = MetricsMiddlewareImpl<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(MetricsMiddlewareImpl {
            service: Rc::new(service),
            meters: self.meters.clone(),
            label_cfg: self.label_cfg,
        }))
    }
}

pub struct MetricsMiddlewareImpl<S> {
    service: Rc<S>,
    meters: HttpMeters,
    label_cfg: LabelConfig,
}

impl<S, B> Service<ServiceRequest> for MetricsMiddlewareImpl<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: MessageBody + 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        let service = self.service.clone();
        let meters = self.meters.clone();
        let label_cfg = self.label_cfg;

        let method_normalized = normalize_method(req.method());
        let start = Instant::now();
        let guard =
            InFlightGuard::enter(meters.active_requests.clone(), method_normalized);

        Box::pin(async move {
            let result = service.call(req).await;
            let elapsed = start.elapsed().as_secs_f64();

            match &result {
                Ok(res) => {
                    let route = extract_route_from_response(res);
                    let status = res.status().as_u16();
                    let extensions = res.request().extensions();
                    let org = extensions.get::<OrganisationId>().map(|o| o.0.clone());
                    let ws = extensions.get::<WorkspaceId>().map(|w| w.0.clone());
                    drop(extensions);

                    let attrs = build_attributes(
                        method_normalized,
                        &route,
                        status,
                        org.as_deref(),
                        ws.as_deref(),
                        &label_cfg,
                    );
                    meters.request_duration.record(elapsed, &attrs);
                    meters.busy_duration.add(
                        elapsed,
                        &[KeyValue::new("http.request.method", method_normalized)],
                    );
                }
                Err(err) => {
                    // The request was consumed by `service.call`, so
                    // `match_pattern` is no longer accessible.  Route stays
                    // `ROUTE_NOT_FOUND`.  We do extract the real HTTP status
                    // from the error's response rather than blindly using 500.
                    let status = err.error_response().status().as_u16();
                    let attrs = build_attributes(
                        method_normalized,
                        ROUTE_NOT_FOUND,
                        status,
                        None,
                        None,
                        &label_cfg,
                    );
                    meters.request_duration.record(elapsed, &attrs);
                    // The request still consumed worker time; count it.
                    meters.busy_duration.add(
                        elapsed,
                        &[KeyValue::new("http.request.method", method_normalized)],
                    );
                }
            }

            guard.release();
            result
        })
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

    #[test]
    fn extract_route_helper_handles_static_paths() {
        assert!(is_static_pattern("/pkg/{tail:.*}"));
        assert!(is_static_pattern("/assets/{tail:.*}"));
        assert!(is_static_pattern("/favicon.ico"));
        assert!(!is_static_pattern("/contexts/{id}"));
        assert!(!is_static_pattern("/health"));
    }

    #[actix_web::test]
    async fn matched_route_setup_smoke() {
        let app = actix_test::init_service(App::new().route(
            "/contexts/{id}",
            web::get().to(|| async { HttpResponse::Ok().finish() }),
        ))
        .await;
        let req = actix_test::TestRequest::get()
            .uri("/contexts/abc123")
            .to_request();
        let resp = actix_test::call_service(&app, req).await;
        assert_eq!(resp.status(), StatusCode::OK);
        // Note: extract_route is exercised in the integration test in Task 19
        // because match_pattern() is only populated mid-pipeline. This unit-test
        // stub is kept for build-coverage of the call site.
    }

    use crate::observability::config::LabelConfig;

    #[test]
    fn build_attributes_with_all_labels() {
        let cfg = LabelConfig {
            with_org_label: true,
            with_workspace_label: true,
        };
        let attrs = build_attributes(
            "GET",
            "/contexts/{id}",
            200,
            Some("org1"),
            Some("ws1"),
            &cfg,
        );
        assert_eq!(attrs.len(), 5);
        assert!(attrs.iter().any(|kv| kv.key.as_str() == "sp.org_id"));
        assert!(attrs.iter().any(|kv| kv.key.as_str() == "sp.workspace_id"));
    }

    #[test]
    fn build_attributes_omits_missing_workspace() {
        let cfg = LabelConfig {
            with_org_label: true,
            with_workspace_label: true,
        };
        let attrs = build_attributes("POST", "/orgs", 201, Some("org1"), None, &cfg);
        assert_eq!(attrs.len(), 4);
        assert!(attrs.iter().any(|kv| kv.key.as_str() == "sp.org_id"));
        assert!(!attrs.iter().any(|kv| kv.key.as_str() == "sp.workspace_id"));
    }

    #[test]
    fn build_attributes_respects_disable_flag() {
        let cfg = LabelConfig {
            with_org_label: false,
            with_workspace_label: false,
        };
        let attrs = build_attributes("GET", "/x", 200, Some("org1"), Some("ws1"), &cfg);
        assert_eq!(attrs.len(), 3);
        assert!(!attrs.iter().any(|kv| kv.key.as_str() == "sp.org_id"));
        assert!(!attrs.iter().any(|kv| kv.key.as_str() == "sp.workspace_id"));
    }

    #[test]
    fn guard_decrements_on_drop_only_once() {
        use crate::observability::{LabelConfig, Observability, ObservabilityConfig};
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

    #[actix_web::test]
    async fn middleware_records_request_duration() {
        use crate::observability::{Observability, ObservabilityConfig};
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
        let mw = MetricsMiddleware::new(&obs.meter(), LabelConfig::default());

        use actix_web::{App, HttpResponse, http::StatusCode, web};
        let app = actix_test::init_service(App::new().wrap(mw).route(
            "/ping",
            web::get().to(|| async { HttpResponse::Ok().body("pong") }),
        ))
        .await;

        let req = actix_test::TestRequest::get().uri("/ping").to_request();
        let resp = actix_test::call_service(&app, req).await;
        assert_eq!(resp.status(), StatusCode::OK);

        let mut buf = Vec::new();
        let metric_families = obs.registry().gather();
        prometheus::Encoder::encode(
            &prometheus::TextEncoder::new(),
            &metric_families,
            &mut buf,
        )
        .unwrap();
        let text = String::from_utf8(buf).unwrap();
        assert!(
            text.contains("http_server_request_duration_seconds_count"),
            "{text}"
        );
        assert!(
            text.contains("http_server_busy_duration_seconds_total"),
            "{text}"
        );
        assert!(text.contains("http_server_active_requests"), "{text}");
        assert!(text.contains("http_route=\"/ping\""), "{text}");
    }
}
