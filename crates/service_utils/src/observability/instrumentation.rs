//! Wiring for `opentelemetry-instrumentation-actix-web`'s `RequestMetrics`.
//!
//! Customizes:
//! - **Route formatting** — collapses unmatched requests to a bounded
//!   `__not_found__` sentinel and static-asset routes (`/pkg`, `/assets`,
//!   `/favicon`) to `__static__`. Keeps `http.route` cardinality bounded.
//! - **Attribute extraction** — replaces the upstream default (which emits
//!   `server.address`, `server.port`, `url.scheme`, `network.protocol.version`)
//!   with a minimal set: `http.request.method` (semconv-normalized — unknown
//!   methods collapse to `_OTHER`), `http.route`, and the optional tenant
//!   labels `sp.org_id` / `sp.workspace_id`.
//!
//! The tenant labels are controlled by a process-wide [`LabelConfig`]
//! installed via [`set_label_config`]. The crate's attribute hook is a `fn`
//! pointer (not a closure), so the toggle has to live in a global. Until
//! `set_label_config` is called, defaults are used.
//!
//! Note: `http.response.status_code` and `error.type` are appended by the
//! upstream middleware *after* the response is observed, not here.

use std::borrow::Cow;
use std::sync::OnceLock;

use actix_web::HttpMessage;
use actix_web::dev::ServiceRequest;
use opentelemetry::KeyValue;
use opentelemetry_instrumentation_actix_web::{
    RequestMetrics, RequestMetricsBuilder, RouteFormatter,
};
use opentelemetry_sdk::metrics::SdkMeterProvider;

use crate::observability::config::LabelConfig;
use crate::service::types::{OrganisationId, WorkspaceId};

/// `http.route` value emitted for requests that did not match any registered
/// route. Bounds the cardinality of probing / scanning traffic.
pub const ROUTE_NOT_FOUND: &str = "__not_found__";

/// `http.route` value emitted for static-asset routes. Without this every
/// unique asset path would create a distinct `http.route` series.
pub const ROUTE_STATIC: &str = "__static__";

const STATIC_PATTERN_PREFIXES: &[&str] = &["/pkg", "/assets", "/favicon"];

fn is_static_pattern(p: &str) -> bool {
    STATIC_PATTERN_PREFIXES
        .iter()
        .any(|prefix| p.starts_with(prefix))
}

#[derive(Debug)]
struct CardinalityBoundedFormatter;

impl RouteFormatter for CardinalityBoundedFormatter {
    fn format(&self, path: &str) -> String {
        // The crate's middleware falls back to the literal `"default"` when
        // actix has no `match_pattern` (i.e. a 404 path).
        if path == "default" {
            return ROUTE_NOT_FOUND.to_string();
        }
        if is_static_pattern(path) {
            return ROUTE_STATIC.to_string();
        }
        path.to_string()
    }
}

static GLOBAL_LABEL_CONFIG: OnceLock<LabelConfig> = OnceLock::new();

/// Install the process-wide label configuration consulted by the metrics
/// middleware. Idempotent — the first call wins; subsequent calls are
/// no-ops. Until set, [`LabelConfig::default()`] is used.
///
/// A global is required because the upstream crate's attribute hook is a
/// `fn` pointer that cannot capture state.
pub fn set_label_config(cfg: LabelConfig) {
    let _ = GLOBAL_LABEL_CONFIG.set(cfg);
}

fn label_config() -> LabelConfig {
    GLOBAL_LABEL_CONFIG.get().copied().unwrap_or_default()
}

/// Normalize HTTP method per OTel semantic conventions: known methods pass
/// through, anything else collapses to `_OTHER` to prevent rogue clients
/// from inflating `http.request.method` cardinality.
fn normalize_method(m: &actix_web::http::Method) -> &'static str {
    macro_rules! match_known {
        ($val:expr, [$($name:literal),+ $(,)?], $other:literal) => {
            match $val { $($name => $name,)+ _ => $other }
        };
    }
    match_known!(
        m.as_str(),
        [
            "GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS", "TRACE",
            "CONNECT"
        ],
        "_OTHER"
    )
}

fn metrics_attrs_from_req(
    req: &ServiceRequest,
    http_route: Cow<'static, str>,
) -> Vec<KeyValue> {
    let lc = label_config();
    let mut attrs = Vec::with_capacity(4);
    attrs.push(KeyValue::new(
        "http.request.method",
        normalize_method(req.method()),
    ));
    attrs.push(KeyValue::new("http.route", http_route));

    if lc.with_org_label {
        if let Some(org) = req.extensions().get::<OrganisationId>() {
            attrs.push(KeyValue::new("sp.org_id", org.0.clone()));
        }
    }
    if lc.with_workspace_label {
        if let Some(ws) = req.extensions().get::<WorkspaceId>() {
            attrs.push(KeyValue::new("sp.workspace_id", ws.0.clone()));
        }
    }

    attrs
}

/// Build the `RequestMetrics` Actix middleware wired with our cardinality-
/// bounded route formatter and minimal attribute extractor. Binds explicitly
/// to the supplied `SdkMeterProvider` (rather than reading the OTel global)
/// so multiple `Observability` instances — e.g. parallel integration tests
/// or a no-op fallback when metrics are disabled — stay isolated.
pub fn build_request_metrics_middleware(provider: &SdkMeterProvider) -> RequestMetrics {
    RequestMetricsBuilder::new()
        .with_route_formatter(CardinalityBoundedFormatter)
        .with_metric_attrs_from_req(metrics_attrs_from_req)
        .with_meter_provider(provider.clone())
        .build()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formatter_maps_unmatched_to_not_found_sentinel() {
        assert_eq!(
            CardinalityBoundedFormatter.format("default"),
            ROUTE_NOT_FOUND
        );
    }

    #[test]
    fn formatter_collapses_static_asset_prefixes() {
        for path in ["/pkg/foo.wasm", "/assets/style.css", "/favicon.ico"] {
            assert_eq!(CardinalityBoundedFormatter.format(path), ROUTE_STATIC);
        }
    }

    #[test]
    fn formatter_passes_through_matched_templates() {
        assert_eq!(
            CardinalityBoundedFormatter.format("/users/{id}"),
            "/users/{id}"
        );
    }

    #[test]
    fn normalize_method_collapses_unknown() {
        let m = actix_web::http::Method::from_bytes(b"XPROPFIND").unwrap();
        assert_eq!(normalize_method(&m), "_OTHER");
    }

    #[test]
    fn normalize_method_passes_known_through() {
        assert_eq!(normalize_method(&actix_web::http::Method::GET), "GET");
        assert_eq!(normalize_method(&actix_web::http::Method::POST), "POST");
    }
}
