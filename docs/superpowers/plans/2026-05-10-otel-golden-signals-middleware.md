# OpenTelemetry Golden-Signals Middleware Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an Actix middleware and supporting subsystem to `crates/service_utils` that emits Google SRE golden signals (latency, traffic, errors, saturation) for every HTTP route on the main API, exposed via Prometheus scrape on a dedicated port and optional OTLP push, using OpenTelemetry.

**Architecture:** New `service_utils::observability` module owns: (a) `init()` that builds an OTel `MeterProvider` with a Prometheus exporter and an optional OTLP exporter, (b) an Actix `MetricsMiddleware` that records `http.server.request.duration` (histogram), `http.server.busy.duration` (counter), and `http.server.active_requests` (UpDownCounter) for every request, (c) saturation collectors for r2d2 DB pool, fred Redis pool, and (cfg-gated) tokio-metrics, and (d) a separate `HttpServer` on `SUPERPOSITION_METRICS_PORT` exposing `/metrics`. Health endpoints `/healthz`/`/livez`/`/readyz` mount on the main port and bypass auth via the existing `tenant_middleware_exclusion_list`.

**Tech Stack:** Rust, Actix-web 4, OpenTelemetry SDK 0.27 (`opentelemetry`, `opentelemetry_sdk`, `opentelemetry-prometheus`, `opentelemetry-otlp`, `opentelemetry-semantic-conventions`), `prometheus` 0.13, `tokio-metrics` 0.3 (under `cfg(tokio_unstable)`), Diesel/r2d2, fred (Redis client).

**Spec:** [`docs/superpowers/specs/2026-05-10-otel-golden-signals-middleware-design.md`](../specs/2026-05-10-otel-golden-signals-middleware-design.md)

**Notes for the implementer:**

- The OpenTelemetry Rust SDK has had API churn between minor versions. The exact import paths and builder method names below are written against `opentelemetry` 0.27. If you pin a different version in Task 1, expect to adjust 1–3 import paths or method names per call site. The plan uses the **stable** APIs only (no unstable/preview features).
- After Task 1 (deps), run `cargo check -p service_utils` after every code-touching task to catch wiring issues early — even on tasks that don't add tests yet.
- File commit boundary: each task ends with one commit. If a step within a task fails, fix and continue within the same task before committing.
- All paths in this plan are absolute under the repo root: `/Users/natarajankannan/src/superposition/`.

---

## Task 1: Add workspace dependencies

**Files:**
- Modify: `Cargo.toml` (root)

- [ ] **Step 1: Add OpenTelemetry deps to `[workspace.dependencies]`**

Add the following block to the `[workspace.dependencies]` section of the root `Cargo.toml` (alphabetical order, near the existing entries like `prometheus`-adjacent / `opentelemetry`-adjacent slots):

```toml
opentelemetry = { version = "0.27", default-features = false, features = ["metrics"] }
opentelemetry_sdk = { version = "0.27", default-features = false, features = ["metrics", "rt-tokio"] }
opentelemetry-prometheus = { version = "0.27", default-features = false }
opentelemetry-otlp = { version = "0.27", default-features = false, features = ["metrics", "http-proto", "reqwest-client"] }
opentelemetry-semantic-conventions = { version = "0.27" }
prometheus = { version = "0.13", default-features = false }
tokio-metrics = { version = "0.3", default-features = false, features = ["rt"] }
humantime = "2.1"
```

- [ ] **Step 2: Verify the workspace still resolves**

Run: `cargo metadata --format-version 1 > /dev/null`
Expected: exit code 0, no errors. (This forces `cargo` to re-resolve the workspace without compiling.)

- [ ] **Step 3: Commit**

```bash
git add Cargo.toml
git commit -m "build: add opentelemetry deps to workspace

Adds opentelemetry, opentelemetry_sdk, opentelemetry-prometheus,
opentelemetry-otlp, opentelemetry-semantic-conventions, prometheus,
tokio-metrics, and humantime as workspace dependencies. Enabled in
service_utils in a follow-up commit.
"
```

---

## Task 2: Add `.cargo/config.toml` for `tokio_unstable`

**Files:**
- Create: `.cargo/config.toml`

- [ ] **Step 1: Create the file**

```toml
# Required by tokio-metrics for runtime instrumentation. Affects all
# crates in the workspace; only the saturation::tokio_runtime module
# consumes the additional APIs that this flag unlocks.
[build]
rustflags = ["--cfg", "tokio_unstable"]
```

- [ ] **Step 2: Verify the workspace still builds**

Run: `cargo check --workspace`
Expected: exit code 0. (Build may take a while on first run; that's fine.)

- [ ] **Step 3: Commit**

```bash
git add .cargo/config.toml
git commit -m "build: enable tokio_unstable workspace-wide

Required by tokio-metrics for runtime instrumentation introduced in
the upcoming observability subsystem. tokio_unstable only adds APIs;
no behavioural change for existing code.
"
```

---

## Task 3: Enable observability deps in `service_utils`

**Files:**
- Modify: `crates/service_utils/Cargo.toml`

- [ ] **Step 1: Add the dependency lines**

Append to the `[dependencies]` block of `crates/service_utils/Cargo.toml` (after the existing entries, preserving alphabetical-ish order):

```toml
opentelemetry = { workspace = true }
opentelemetry_sdk = { workspace = true }
opentelemetry-prometheus = { workspace = true }
opentelemetry-otlp = { workspace = true }
opentelemetry-semantic-conventions = { workspace = true }
prometheus = { workspace = true }
tokio-metrics = { workspace = true }
humantime = { workspace = true }
```

(`fred` already has the `metrics` feature enabled at line 22 — no change needed there.)

- [ ] **Step 2: Verify the crate still compiles**

Run: `cargo check -p service_utils`
Expected: exit code 0.

- [ ] **Step 3: Commit**

```bash
git add crates/service_utils/Cargo.toml
git commit -m "build(service_utils): enable opentelemetry deps"
```

---

## Task 4: Module skeleton in `service_utils`

**Files:**
- Modify: `crates/service_utils/src/lib.rs`
- Create: `crates/service_utils/src/observability.rs`

- [ ] **Step 1: Add the `pub mod` line**

Edit `crates/service_utils/src/lib.rs`, adding a new line in alphabetical position:

```rust
pub mod aws;
pub mod db;
pub mod encryption;
pub mod extensions;
pub mod helpers;
pub mod middlewares;
pub mod observability;   // <-- NEW LINE, between middlewares and redis
pub mod redis;
pub mod registry;
pub mod service;
```

- [ ] **Step 2: Create `observability.rs` with public surface stubs**

```rust
//! HTTP golden-signals metrics exposition via OpenTelemetry.
//!
//! See `docs/superpowers/specs/2026-05-10-otel-golden-signals-middleware-design.md`.

mod config;
mod health;
mod meters;
mod metrics_server;
mod middleware;
mod saturation;

pub use config::{LabelConfig, ObservabilityConfig};
pub use health::{health_endpoint_paths, health_endpoints};
pub use metrics_server::spawn_metrics_server;
pub use middleware::MetricsMiddleware;
pub use saturation::{register_observers, SaturationDeps};

use std::sync::Arc;

use opentelemetry::metrics::Meter;
use opentelemetry_sdk::metrics::SdkMeterProvider;
use prometheus::Registry;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ObservabilityError {
    #[error("prometheus exporter init failed: {0}")]
    PrometheusInit(String),
    #[error("otlp exporter init failed: {0}")]
    OtlpInit(String),
    #[error("config error: {0}")]
    Config(String),
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

pub struct Observability {
    provider: SdkMeterProvider,
    registry: Arc<Registry>,
    meter: Meter,
}

impl Observability {
    pub fn meter(&self) -> Meter {
        self.meter.clone()
    }

    pub fn registry(&self) -> Arc<Registry> {
        self.registry.clone()
    }

    pub fn shutdown(self) -> Result<(), ObservabilityError> {
        self.provider
            .shutdown()
            .map_err(|e| ObservabilityError::PrometheusInit(e.to_string()))
    }

    pub fn init(_cfg: ObservabilityConfig) -> Result<Self, ObservabilityError> {
        // Real implementation lands in Task 7.
        unimplemented!("Observability::init implemented in Task 7")
    }
}
```

(`thiserror` is already used elsewhere in the workspace; if `service_utils/Cargo.toml` does not yet depend on it, add `thiserror = "1"` to the `[dependencies]` block. Quick check: `grep thiserror crates/service_utils/Cargo.toml`.)

- [ ] **Step 3: Verify it compiles**

Run: `cargo check -p service_utils`
Expected: exit code 0. The stub modules listed in the `mod` declarations don't exist yet, so compile may fail — proceed to step 4 if so.

- [ ] **Step 4: Create empty stub files for child modules so this task ends compilable**

Create each of:

- `crates/service_utils/src/observability/config.rs`:
  ```rust
  //! Stub — real implementation in Task 5.
  pub struct ObservabilityConfig;
  pub struct LabelConfig;
  ```
- `crates/service_utils/src/observability/meters.rs`:
  ```rust
  //! Stub — real implementation in Task 7.
  ```
- `crates/service_utils/src/observability/middleware.rs`:
  ```rust
  //! Stub — real implementation in Task 11.
  pub struct MetricsMiddleware;
  ```
- `crates/service_utils/src/observability/metrics_server.rs`:
  ```rust
  //! Stub — real implementation in Task 13.
  use std::{net::SocketAddr, sync::Arc};
  use prometheus::Registry;
  pub fn spawn_metrics_server(
      _registry: Arc<Registry>,
      _bind: SocketAddr,
  ) -> std::io::Result<actix_web::dev::Server> {
      unimplemented!("Task 13")
  }
  ```
- `crates/service_utils/src/observability/health.rs`:
  ```rust
  //! Stub — real implementation in Task 12.
  pub fn health_endpoints() -> actix_web::Scope {
      actix_web::web::scope("")
  }
  pub fn health_endpoint_paths() -> &'static [&'static str] {
      &[]
  }
  ```
- `crates/service_utils/src/observability/saturation.rs`:
  ```rust
  //! Stub — real implementation in Task 14.
  use opentelemetry::metrics::Meter;
  pub struct SaturationDeps;
  pub fn register_observers(
      _meter: &Meter,
      _deps: SaturationDeps,
  ) -> Result<(), super::ObservabilityError> {
      Ok(())
  }
  ```

- [ ] **Step 5: Verify it compiles**

Run: `cargo check -p service_utils`
Expected: exit code 0.

- [ ] **Step 6: Commit**

```bash
git add crates/service_utils/src/lib.rs crates/service_utils/src/observability.rs crates/service_utils/src/observability/
git commit -m "feat(observability): module skeleton

Adds the empty module structure that subsequent commits flesh out:
- observability.rs: public surface, Observability handle, errors
- observability/{config,meters,middleware,metrics_server,health,saturation}.rs: stubs

No behaviour change. The Observability::init() body is unimplemented!()
until Task 7.
"
```

---

## Task 5: `ObservabilityConfig` from env

**Files:**
- Modify: `crates/service_utils/src/observability/config.rs`

- [ ] **Step 1: Write the failing test**

Replace the contents of `crates/service_utils/src/observability/config.rs` with:

```rust
//! Configuration for the observability subsystem, parsed from env vars.

use std::{net::IpAddr, str::FromStr, time::Duration};

#[derive(Debug, Clone)]
pub struct ObservabilityConfig {
    pub enabled: bool,
    pub bind: IpAddr,
    pub port: u16,
    pub label: LabelConfig,
    pub collect_interval: Duration,
    pub instance_id: String,
    pub service_name: String,
    pub service_version: String,
    pub deployment_environment: Option<String>,
    pub otlp_endpoint: Option<String>,
}

#[derive(Debug, Clone, Copy)]
pub struct LabelConfig {
    pub with_org_label: bool,
    pub with_workspace_label: bool,
}

impl Default for LabelConfig {
    fn default() -> Self {
        Self { with_org_label: true, with_workspace_label: true }
    }
}

impl ObservabilityConfig {
    pub fn from_env() -> Result<Self, String> {
        fn env_bool(key: &str, default: bool) -> Result<bool, String> {
            match std::env::var(key) {
                Ok(v) => v.parse::<bool>().map_err(|_| format!("{key} must be true or false")),
                Err(_) => Ok(default),
            }
        }
        fn env_str(key: &str, default: &str) -> String {
            std::env::var(key).unwrap_or_else(|_| default.to_owned())
        }
        fn env_opt(key: &str) -> Option<String> {
            std::env::var(key).ok().filter(|s| !s.is_empty())
        }

        let enabled = env_bool("SUPERPOSITION_METRICS_ENABLED", true)?;
        let bind = IpAddr::from_str(&env_str("SUPERPOSITION_METRICS_BIND", "0.0.0.0"))
            .map_err(|e| format!("SUPERPOSITION_METRICS_BIND: {e}"))?;
        let port: u16 = env_str("SUPERPOSITION_METRICS_PORT", "9091")
            .parse()
            .map_err(|e| format!("SUPERPOSITION_METRICS_PORT: {e}"))?;
        let with_org_label = env_bool("SUPERPOSITION_METRICS_LABEL_ORG", true)?;
        let with_workspace_label = env_bool("SUPERPOSITION_METRICS_LABEL_WORKSPACE", true)?;
        let collect_interval =
            humantime::parse_duration(&env_str("SUPERPOSITION_METRICS_COLLECT_INTERVAL", "10s"))
                .map_err(|e| format!("SUPERPOSITION_METRICS_COLLECT_INTERVAL: {e}"))?;
        let instance_id = env_opt("SUPERPOSITION_INSTANCE_ID")
            .or_else(|| hostname_or_none())
            .unwrap_or_else(|| "unknown".to_owned());
        let service_name = env_str("OTEL_SERVICE_NAME", "superposition");
        let service_version = env!("CARGO_PKG_VERSION").to_owned();
        let deployment_environment = env_opt("APP_ENV").or_else(|| env_opt("DEPLOYMENT_ENV"));
        let otlp_endpoint = env_opt("OTEL_EXPORTER_OTLP_ENDPOINT");

        Ok(Self {
            enabled,
            bind,
            port,
            label: LabelConfig { with_org_label, with_workspace_label },
            collect_interval,
            instance_id,
            service_name,
            service_version,
            deployment_environment,
            otlp_endpoint,
        })
    }
}

fn hostname_or_none() -> Option<String> {
    // Avoid pulling in a hostname crate; read /etc/hostname on Linux/macOS.
    std::fs::read_to_string("/etc/hostname")
        .ok()
        .map(|s| s.trim().to_owned())
        .filter(|s| !s.is_empty())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests run sequentially via `serial_test` to avoid env races.
    /// We use a simple lock + a env-snapshot helper instead of adding a new dep.
    fn with_env<F: FnOnce()>(vars: &[(&str, Option<&str>)], f: F) {
        use std::sync::Mutex;
        static LOCK: Mutex<()> = Mutex::new(());
        let _guard = LOCK.lock().unwrap();
        let prev: Vec<_> =
            vars.iter().map(|(k, _)| (k.to_string(), std::env::var(k).ok())).collect();
        for (k, v) in vars {
            match v {
                Some(v) => std::env::set_var(k, v),
                None => std::env::remove_var(k),
            }
        }
        f();
        for (k, v) in prev {
            match v {
                Some(v) => std::env::set_var(&k, &v),
                None => std::env::remove_var(&k),
            }
        }
    }

    #[test]
    fn defaults_when_unset() {
        with_env(
            &[
                ("SUPERPOSITION_METRICS_ENABLED", None),
                ("SUPERPOSITION_METRICS_PORT", None),
                ("SUPERPOSITION_METRICS_BIND", None),
                ("SUPERPOSITION_METRICS_LABEL_ORG", None),
                ("SUPERPOSITION_METRICS_LABEL_WORKSPACE", None),
                ("SUPERPOSITION_METRICS_COLLECT_INTERVAL", None),
                ("OTEL_EXPORTER_OTLP_ENDPOINT", None),
                ("OTEL_SERVICE_NAME", None),
            ],
            || {
                let cfg = ObservabilityConfig::from_env().unwrap();
                assert!(cfg.enabled);
                assert_eq!(cfg.port, 9091);
                assert_eq!(cfg.bind.to_string(), "0.0.0.0");
                assert!(cfg.label.with_org_label);
                assert!(cfg.label.with_workspace_label);
                assert_eq!(cfg.collect_interval, Duration::from_secs(10));
                assert_eq!(cfg.service_name, "superposition");
                assert_eq!(cfg.otlp_endpoint, None);
            },
        );
    }

    #[test]
    fn explicit_overrides() {
        with_env(
            &[
                ("SUPERPOSITION_METRICS_ENABLED", Some("false")),
                ("SUPERPOSITION_METRICS_PORT", Some("9999")),
                ("SUPERPOSITION_METRICS_BIND", Some("127.0.0.1")),
                ("SUPERPOSITION_METRICS_LABEL_WORKSPACE", Some("false")),
                ("SUPERPOSITION_METRICS_COLLECT_INTERVAL", Some("30s")),
                ("OTEL_EXPORTER_OTLP_ENDPOINT", Some("http://collector:4318")),
                ("OTEL_SERVICE_NAME", Some("sp-test")),
            ],
            || {
                let cfg = ObservabilityConfig::from_env().unwrap();
                assert!(!cfg.enabled);
                assert_eq!(cfg.port, 9999);
                assert_eq!(cfg.bind.to_string(), "127.0.0.1");
                assert!(cfg.label.with_org_label); // default still true
                assert!(!cfg.label.with_workspace_label);
                assert_eq!(cfg.collect_interval, Duration::from_secs(30));
                assert_eq!(cfg.otlp_endpoint.as_deref(), Some("http://collector:4318"));
                assert_eq!(cfg.service_name, "sp-test");
            },
        );
    }

    #[test]
    fn malformed_port_errors() {
        with_env(
            &[("SUPERPOSITION_METRICS_PORT", Some("not-a-number"))],
            || {
                let err = ObservabilityConfig::from_env().unwrap_err();
                assert!(err.contains("SUPERPOSITION_METRICS_PORT"));
            },
        );
    }
}
```

- [ ] **Step 2: Run the tests**

Run: `cargo test -p service_utils observability::config -- --test-threads=1`
Expected: 3 tests pass.

(`--test-threads=1` is required because the tests mutate process env vars; the in-test mutex covers same-binary races but doctests/other tests in parallel could interleave.)

- [ ] **Step 3: Commit**

```bash
git add crates/service_utils/src/observability/config.rs
git commit -m "feat(observability): config from env

Reads SUPERPOSITION_METRICS_* and OTEL_* env vars into a typed
ObservabilityConfig. Defaults: enabled, port 9091, bind 0.0.0.0,
both org/workspace labels on, 10s collect interval.
"
```

---

## Task 6: HTTP method normalization helper

**Files:**
- Modify: `crates/service_utils/src/observability/middleware.rs`

- [ ] **Step 1: Replace stub with TDD scaffold**

```rust
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
}
```

- [ ] **Step 2: Run the tests**

Run: `cargo test -p service_utils observability::middleware`
Expected: 2 tests pass.

- [ ] **Step 3: Commit**

```bash
git add crates/service_utils/src/observability/middleware.rs
git commit -m "feat(observability): http method normalization

Per OTel semconv: collapse unknown methods to _OTHER to bound the
cardinality of the http.request.method label.
"
```

---

## Task 7: `Observability::init()` with Prometheus exporter

**Files:**
- Modify: `crates/service_utils/src/observability.rs`
- Modify: `crates/service_utils/src/observability/meters.rs`

- [ ] **Step 1: Define `HttpMeters` struct**

Replace `crates/service_utils/src/observability/meters.rs`:

```rust
//! Typed handles for the metric instruments emitted by the HTTP middleware.

use opentelemetry::metrics::{Counter, Histogram, Meter, UpDownCounter};

/// Histogram + counter + gauge for HTTP server golden signals. Built once at
/// startup and cloned cheaply; instruments are `Arc<>`-backed internally.
#[derive(Clone)]
pub struct HttpMeters {
    pub request_duration: Histogram<f64>,
    pub busy_duration: Counter<f64>,
    pub active_requests: UpDownCounter<i64>,
}

impl HttpMeters {
    pub fn new(meter: &Meter) -> Self {
        let request_duration = meter
            .f64_histogram("http.server.request.duration")
            .with_unit("s")
            .with_description("Duration of HTTP server requests, in seconds.")
            .with_boundaries(vec![
                0.005, 0.025, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0,
            ])
            .build();

        let busy_duration = meter
            .f64_counter("http.server.busy.duration")
            .with_unit("s")
            .with_description(
                "Cumulative seconds spent serving HTTP requests; \
                 rate() over a window gives time-averaged request concurrency.",
            )
            .build();

        let active_requests = meter
            .i64_up_down_counter("http.server.active_requests")
            .with_description("Number of HTTP server requests currently in flight.")
            .build();

        Self { request_duration, busy_duration, active_requests }
    }
}
```

- [ ] **Step 2: Implement `Observability::init`**

Replace the body of `Observability::init` in `crates/service_utils/src/observability.rs`:

```rust
impl Observability {
    pub fn init(cfg: ObservabilityConfig) -> Result<Self, ObservabilityError> {
        use opentelemetry::KeyValue;
        use opentelemetry_sdk::Resource;
        use opentelemetry_sdk::metrics::SdkMeterProvider;

        let registry = Arc::new(prometheus::Registry::new());

        let exporter = opentelemetry_prometheus::exporter()
            .with_registry((*registry).clone())
            .build()
            .map_err(|e| ObservabilityError::PrometheusInit(e.to_string()))?;

        let mut resource_attrs = vec![
            KeyValue::new("service.name", cfg.service_name.clone()),
            KeyValue::new("service.version", cfg.service_version.clone()),
            KeyValue::new("service.instance.id", cfg.instance_id.clone()),
        ];
        if let Some(env) = &cfg.deployment_environment {
            resource_attrs.push(KeyValue::new("deployment.environment", env.clone()));
        }

        let mut builder = SdkMeterProvider::builder()
            .with_reader(exporter)
            .with_resource(Resource::new(resource_attrs));

        if let Some(endpoint) = &cfg.otlp_endpoint {
            builder = with_otlp_reader(builder, endpoint, cfg.collect_interval)?;
        }

        let provider = builder.build();
        opentelemetry::global::set_meter_provider(provider.clone());
        let meter = provider.meter("superposition");

        Ok(Self { provider, registry, meter })
    }
}

#[cfg(not(test))]
fn with_otlp_reader(
    builder: opentelemetry_sdk::metrics::MeterProviderBuilder,
    endpoint: &str,
    interval: std::time::Duration,
) -> Result<opentelemetry_sdk::metrics::MeterProviderBuilder, ObservabilityError> {
    use opentelemetry_otlp::{MetricExporter, WithExportConfig};
    use opentelemetry_sdk::metrics::PeriodicReader;
    use opentelemetry_sdk::runtime;

    let exporter = MetricExporter::builder()
        .with_http()
        .with_endpoint(endpoint.to_owned())
        .build()
        .map_err(|e| ObservabilityError::OtlpInit(e.to_string()))?;

    let reader = PeriodicReader::builder(exporter, runtime::Tokio)
        .with_interval(interval)
        .build();

    Ok(builder.with_reader(reader))
}

#[cfg(test)]
fn with_otlp_reader(
    builder: opentelemetry_sdk::metrics::MeterProviderBuilder,
    _endpoint: &str,
    _interval: std::time::Duration,
) -> Result<opentelemetry_sdk::metrics::MeterProviderBuilder, ObservabilityError> {
    // OTLP exporter requires a tokio runtime; we don't spin one up in unit tests.
    Ok(builder)
}
```

- [ ] **Step 3: Add a smoke test**

Append to `crates/service_utils/src/observability.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    fn test_cfg() -> ObservabilityConfig {
        ObservabilityConfig {
            enabled: true,
            bind: "127.0.0.1".parse().unwrap(),
            port: 0,
            label: LabelConfig::default(),
            collect_interval: std::time::Duration::from_secs(10),
            instance_id: "test".into(),
            service_name: "sp-test".into(),
            service_version: "0.0.0-test".into(),
            deployment_environment: None,
            otlp_endpoint: None,
        }
    }

    #[test]
    fn init_builds_meter_and_registry() {
        let obs = Observability::init(test_cfg()).expect("init failed");
        let _meter = obs.meter();
        let registry = obs.registry();
        assert_eq!(registry.gather().len(), 0, "no metrics emitted yet");
    }

    #[test]
    fn meter_can_record_a_histogram_and_register_it_in_registry() {
        let obs = Observability::init(test_cfg()).unwrap();
        let meter = obs.meter();
        let h = meter.f64_histogram("test.duration").with_unit("s").build();
        h.record(0.123, &[]);

        let mut buf = Vec::new();
        let encoder = prometheus::TextEncoder::new();
        let metric_families = obs.registry().gather();
        prometheus::Encoder::encode(&encoder, &metric_families, &mut buf).unwrap();
        let text = String::from_utf8(buf).unwrap();
        assert!(
            text.contains("test_duration"),
            "expected test_duration in exposition, got:\n{text}"
        );
    }
}
```

- [ ] **Step 4: Run the tests**

Run: `cargo test -p service_utils observability::tests`
Expected: 2 tests pass.

If you get a compile error about `with_boundaries` not existing, the OpenTelemetry SDK version you pinned uses the older `with_explicit_buckets` name. Adjust the call in `meters.rs` accordingly. Same for `MetricExporter::builder().with_http()` — older versions used `new_exporter().http()`.

- [ ] **Step 5: Commit**

```bash
git add crates/service_utils/src/observability.rs crates/service_utils/src/observability/meters.rs
git commit -m "feat(observability): MeterProvider with prometheus exporter

Builds an SdkMeterProvider wired to an opentelemetry-prometheus
exporter that writes into a per-process prometheus::Registry. OTLP
push exporter is plumbed but only activates when
OTEL_EXPORTER_OTLP_ENDPOINT is set.
"
```

---

## Task 8: Route template extraction helper

**Files:**
- Modify: `crates/service_utils/src/observability/middleware.rs`

- [ ] **Step 1: Add helper + tests**

Append to `crates/service_utils/src/observability/middleware.rs`:

```rust
use actix_web::dev::ServiceRequest;

/// Sentinel for paths that did not match any registered route (would 404).
pub(crate) const ROUTE_NOT_FOUND: &str = "__not_found__";

/// Extracts the templated route pattern from a ServiceRequest. Falls back to
/// a sentinel when no route matched, to keep `http.route` cardinality bounded.
pub(crate) fn extract_route(req: &ServiceRequest) -> String {
    req.match_pattern().unwrap_or_else(|| ROUTE_NOT_FOUND.to_owned())
}
```

Add tests inside the existing `#[cfg(test)] mod tests` block:

```rust
    use actix_web::{App, HttpResponse, http::StatusCode, test, web};

    #[actix_web::test]
    async fn matched_route_returns_pattern() {
        let app = test::init_service(
            App::new().route(
                "/contexts/{id}",
                web::get().to(|| async { HttpResponse::Ok() }),
            ),
        )
        .await;
        let req = test::TestRequest::get().uri("/contexts/abc123").to_request();
        let resp = test::call_service(&app, req).await;
        assert_eq!(resp.status(), StatusCode::OK);
        // Note: extract_route is exercised in the integration test in Task 15
        // because match_pattern() is only populated mid-pipeline. This unit-test
        // stub is kept for build-coverage of the call site.
    }
```

- [ ] **Step 2: Run the tests**

Run: `cargo test -p service_utils observability::middleware`
Expected: previous 2 tests + 1 new still pass.

- [ ] **Step 3: Commit**

```bash
git add crates/service_utils/src/observability/middleware.rs
git commit -m "feat(observability): route template extraction helper"
```

---

## Task 9: Label extraction with org/workspace from extensions

**Files:**
- Modify: `crates/service_utils/src/observability/middleware.rs`

- [ ] **Step 1: Confirm extension types**

Run: `grep -rn "OrgId\|WorkspaceName\|insert::<.*Workspace" /Users/natarajankannan/src/superposition/crates/service_utils/src/middlewares/ 2>/dev/null | head -10`
Expected: shows the actual type names that `OrgWorkspaceMiddlewareFactory` inserts into request extensions. The likely candidates are `OrgId(String)` and `WorkspaceName(String)` or similar newtypes from `superposition_types`.

If the grep shows different type names, use those in the code below in place of the placeholders.

- [ ] **Step 2: Add label-build helper + tests**

Append to `crates/service_utils/src/observability/middleware.rs`:

```rust
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
```

Add to the test block:

```rust
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
```

- [ ] **Step 3: Run the tests**

Run: `cargo test -p service_utils observability::middleware`
Expected: all middleware tests pass.

- [ ] **Step 4: Commit**

```bash
git add crates/service_utils/src/observability/middleware.rs
git commit -m "feat(observability): build OTel attributes for HTTP metrics"
```

---

## Task 10: `InFlightGuard` (panic-safe active-requests decrement)

**Files:**
- Modify: `crates/service_utils/src/observability/middleware.rs`

- [ ] **Step 1: Add the guard + tests**

Append to `middleware.rs`:

```rust
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
```

Add a test (this requires a real meter; we get one from `Observability::init`):

```rust
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
        // Hard to introspect the counter value from outside, but we can call
        // release multiple times and ensure no panic.
        let g = InFlightGuard::enter(m.clone(), "POST");
        g.release();
        g.release();
        drop(g);
    }
```

- [ ] **Step 2: Run the tests**

Run: `cargo test -p service_utils observability::middleware`
Expected: all middleware tests pass.

- [ ] **Step 3: Commit**

```bash
git add crates/service_utils/src/observability/middleware.rs
git commit -m "feat(observability): RAII guard for active_requests gauge

Drop-based decrement ensures the gauge stays balanced even when a
handler panics or the future is cancelled.
"
```

---

## Task 11: Full `MetricsMiddleware` (Transform + Service)

**Files:**
- Modify: `crates/service_utils/src/observability/middleware.rs`

- [ ] **Step 1: Implement the full middleware**

Replace the placeholder `pub struct MetricsMiddleware;` with the full implementation. Append/replace at the bottom of `middleware.rs` (keep all helpers and tests above intact):

```rust
use std::future::{Ready, ready};
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll};
use std::time::Instant;

use actix_web::{
    Error, HttpMessage,
    body::MessageBody,
    dev::{Service, ServiceResponse, Transform, forward_ready},
};
use futures_util::future::LocalBoxFuture;
use opentelemetry::metrics::Meter;

use crate::observability::config::LabelConfig;
use crate::observability::meters::HttpMeters;

#[derive(Clone)]
pub struct MetricsMiddleware {
    meters: HttpMeters,
    label_cfg: LabelConfig,
}

impl MetricsMiddleware {
    pub fn new(meter: &Meter, label_cfg: LabelConfig) -> Self {
        Self { meters: HttpMeters::new(meter), label_cfg }
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
        let guard = InFlightGuard::enter(meters.active_requests.clone(), method_normalized);

        Box::pin(async move {
            let result = service.call(req).await;
            let elapsed = start.elapsed().as_secs_f64();

            match &result {
                Ok(res) => {
                    let route = extract_route_from_response(res);
                    let status = res.status().as_u16();
                    let org = res
                        .request()
                        .extensions()
                        .get::<OrgIdExt>()
                        .map(|o| o.0.clone());
                    let ws = res
                        .request()
                        .extensions()
                        .get::<WorkspaceNameExt>()
                        .map(|w| w.0.clone());

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
                Err(_) => {
                    // The error converts to a response upstream; record under 500
                    // with `error.type=unhandled`. Route is unknown here.
                    let attrs = build_attributes(
                        method_normalized,
                        ROUTE_NOT_FOUND,
                        500,
                        None,
                        None,
                        &label_cfg,
                    );
                    meters.request_duration.record(elapsed, &attrs);
                }
            }

            guard.release();
            result
        })
    }
}

fn extract_route_from_response<B>(res: &ServiceResponse<B>) -> String {
    res.request()
        .match_pattern()
        .unwrap_or_else(|| ROUTE_NOT_FOUND.to_owned())
}

/// Newtype wrappers used to read org/workspace from request extensions.
/// Replace these with the real types inserted by `OrgWorkspaceMiddlewareFactory`
/// (verified in Task 9 Step 1) — typically something like
/// `superposition_types::OrgId(pub String)` and `WorkspaceName(pub String)`.
#[derive(Clone)]
pub(crate) struct OrgIdExt(pub String);
#[derive(Clone)]
pub(crate) struct WorkspaceNameExt(pub String);
```

**Important — replace `OrgIdExt` / `WorkspaceNameExt` with the real extension types** that `OrgWorkspaceMiddlewareFactory` inserts (verified at Task 9 Step 1). If they live in `superposition_types`, just import them and use them directly. The newtype shims above only exist as a fallback to keep this task buildable in isolation.

- [ ] **Step 2: Add an end-to-end test for the middleware**

Append to the test module:

```rust
    use crate::observability::{Observability, ObservabilityConfig};
    use actix_web::App;
    use std::time::Duration;

    fn obs_for_test() -> Observability {
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
        Observability::init(cfg).unwrap()
    }

    #[actix_web::test]
    async fn middleware_records_request_duration() {
        let obs = obs_for_test();
        let mw = MetricsMiddleware::new(&obs.meter(), LabelConfig::default());
        let app = test::init_service(
            App::new().wrap(mw).route(
                "/ping",
                web::get().to(|| async { HttpResponse::Ok().body("pong") }),
            ),
        )
        .await;

        let req = test::TestRequest::get().uri("/ping").to_request();
        let resp = test::call_service(&app, req).await;
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
        assert!(text.contains("http_server_request_duration_seconds_count"), "{text}");
        assert!(text.contains("http_server_busy_duration_seconds_total"), "{text}");
        assert!(text.contains("http_server_active_requests"), "{text}");
        assert!(text.contains("http_route=\"/ping\""), "{text}");
    }
```

- [ ] **Step 3: Run the tests**

Run: `cargo test -p service_utils observability::middleware`
Expected: all middleware tests pass.

- [ ] **Step 4: Commit**

```bash
git add crates/service_utils/src/observability/middleware.rs
git commit -m "feat(observability): MetricsMiddleware records HTTP signals

Wraps every request with timing + active_requests gauge + busy_duration
counter. Uses match_pattern() to template routes, OrgWorkspaceMiddleware
extensions for tenant labels, and an InFlightGuard for panic safety.
"
```

---

## Task 12: Health endpoints

**Files:**
- Modify: `crates/service_utils/src/observability/health.rs`

- [ ] **Step 1: Replace stub with real handlers**

```rust
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

    #[test]
    fn paths_list_matches_routes() {
        let paths = health_endpoint_paths();
        assert_eq!(paths, &[HEALTHZ, LIVEZ, READYZ]);
    }
}
```

- [ ] **Step 2: Run the tests**

Run: `cargo test -p service_utils observability::health`
Expected: 2 tests pass.

- [ ] **Step 3: Commit**

```bash
git add crates/service_utils/src/observability/health.rs
git commit -m "feat(observability): /healthz /livez /readyz handlers"
```

---

## Task 13: Metrics server (separate `HttpServer` on `SUPERPOSITION_METRICS_PORT`)

**Files:**
- Modify: `crates/service_utils/src/observability/metrics_server.rs`

- [ ] **Step 1: Replace stub with real implementation**

```rust
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
        return HttpResponse::InternalServerError()
            .body(format!("encode error: {e}"));
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
```

- [ ] **Step 2: Run the tests**

Run: `cargo test -p service_utils observability::metrics_server`
Expected: 1 test passes.

- [ ] **Step 3: Commit**

```bash
git add crates/service_utils/src/observability/metrics_server.rs
git commit -m "feat(observability): /metrics server on dedicated port"
```

---

## Task 14: DB pool saturation (r2d2 ObservableGauge callbacks)

**Files:**
- Create: `crates/service_utils/src/observability/saturation/db_pool.rs`
- Modify: `crates/service_utils/src/observability/saturation.rs`

- [ ] **Step 1: Confirm pool type**

Run: `grep -rn "type DbPool\|r2d2::Pool<\|PgPool" /Users/natarajankannan/src/superposition/crates/service_utils/src/db/ /Users/natarajankannan/src/superposition/crates/service_utils/src/service/ 2>/dev/null | head -10`
Expected: shows the concrete `r2d2::Pool<…>` alias used across the codebase. The `DbPoolHandle` type alias below should be set to that exact type (commonly `r2d2::Pool<diesel::r2d2::ConnectionManager<diesel::PgConnection>>`).

- [ ] **Step 2: Create the db_pool module**

```rust
//! ObservableGauge callbacks for the r2d2 connection pool. Purely passive —
//! no instrumentation at `pool.get()` call sites.

use opentelemetry::{KeyValue, metrics::Meter};

/// Concrete pool type used across the codebase. Update if it differs.
pub type DbPoolHandle = std::sync::Arc<
    r2d2::Pool<diesel::r2d2::ConnectionManager<diesel::PgConnection>>,
>;

pub fn register(meter: &Meter, pool: DbPoolHandle, pool_name: &'static str) {
    let pool_for_usage = pool.clone();
    let usage_pool_name = KeyValue::new("pool.name", pool_name);
    meter
        .u64_observable_gauge("db.client.connections.usage")
        .with_description("Number of DB connections in idle/used state.")
        .with_callback(move |observer| {
            let s = pool_for_usage.state();
            let used = s.connections.saturating_sub(s.idle_connections);
            observer.observe(
                s.idle_connections as u64,
                &[
                    KeyValue::new("state", "idle"),
                    usage_pool_name.clone(),
                ],
            );
            observer.observe(
                used as u64,
                &[
                    KeyValue::new("state", "used"),
                    usage_pool_name.clone(),
                ],
            );
        })
        .build();

    let pool_for_max = pool.clone();
    let max_pool_name = KeyValue::new("pool.name", pool_name);
    meter
        .u64_observable_gauge("db.client.connections.max")
        .with_description("Configured maximum size of the DB connection pool.")
        .with_callback(move |observer| {
            observer.observe(pool_for_max.max_size() as u64, &[max_pool_name.clone()]);
        })
        .build();
}
```

- [ ] **Step 3: Wire it via the saturation entry point**

Replace `crates/service_utils/src/observability/saturation.rs`:

```rust
//! Saturation collectors: DB pool, Redis pool, Tokio runtime.
//!
//! Most metrics are observable-gauge callbacks (no background tasks).
//! Only `tokio_runtime` requires a polling loop.

mod db_pool;
mod redis_pool;
mod tokio_runtime;

use opentelemetry::metrics::Meter;

pub use db_pool::DbPoolHandle;

/// Optional dependencies the saturation subsystem can observe.
#[derive(Default, Clone)]
pub struct SaturationDeps {
    pub db_pool: Option<DbPoolHandle>,
    pub redis_client: Option<redis_pool::RedisHandle>,
    pub tokio_collect_interval: std::time::Duration,
}

pub fn register_observers(
    meter: &Meter,
    deps: SaturationDeps,
) -> Result<(), super::ObservabilityError> {
    if let Some(pool) = deps.db_pool {
        db_pool::register(meter, pool, "primary");
    }
    if let Some(client) = deps.redis_client {
        redis_pool::register(meter, client, "primary");
    }

    #[cfg(tokio_unstable)]
    if deps.tokio_collect_interval > std::time::Duration::ZERO {
        tokio_runtime::spawn(meter, deps.tokio_collect_interval);
    }

    Ok(())
}
```

- [ ] **Step 4: Verify it compiles**

Run: `cargo check -p service_utils`
Expected: exit code 0. Failures here are usually:
- The `DbPoolHandle` alias doesn't match the codebase's actual pool type → adjust to whatever Step 1 found.
- `redis_pool` module doesn't exist yet → that's Task 15. Create an empty stub now: `crates/service_utils/src/observability/saturation/redis_pool.rs` with:
  ```rust
  //! Stub — implemented in Task 15.
  use opentelemetry::metrics::Meter;
  pub type RedisHandle = std::sync::Arc<()>;
  pub fn register(_meter: &Meter, _client: RedisHandle, _pool_name: &'static str) {}
  ```
- Same for `tokio_runtime` (Task 16). Stub: `crates/service_utils/src/observability/saturation/tokio_runtime.rs` with:
  ```rust
  //! Stub — implemented in Task 16.
  #[cfg(tokio_unstable)]
  pub fn spawn(_meter: &opentelemetry::metrics::Meter, _interval: std::time::Duration) {}
  ```

- [ ] **Step 5: Add a smoke test**

Append to `db_pool.rs`:

```rust
#[cfg(test)]
mod tests {
    // Constructing a real r2d2 pool requires a database. We assert the function
    // signature compiles and that calling `register` does not panic with a
    // synthetic in-memory pool; this is exercised via the integration test in
    // Task 18 instead.
}
```

- [ ] **Step 6: Commit**

```bash
git add crates/service_utils/src/observability/saturation.rs crates/service_utils/src/observability/saturation/
git commit -m "feat(observability): db pool saturation gauges

ObservableGauge callbacks read r2d2::Pool::state() at scrape time.
Emits db.client.connections.usage{state} and db.client.connections.max.
"
```

---

## Task 15: Redis pool saturation (fred metrics)

**Files:**
- Modify: `crates/service_utils/src/observability/saturation/redis_pool.rs`

- [ ] **Step 1: Find fred client type and metrics surface**

Run: `grep -rn "fred::\|RedisClient\|Pool<RedisClient>" /Users/natarajankannan/src/superposition/crates/service_utils/src/redis* 2>/dev/null | head -10`
Expected: shows the concrete fred client type (likely `fred::clients::RedisClient` or `fred::clients::RedisPool`).

Also run: `cargo doc -p fred --no-deps --open` *(or browse https://docs.rs/fred/latest/fred/)* and locate the metrics API. fred 9.x exposes per-client `read_latency_metrics()` / `write_latency_metrics()` and connection counters via `Server` / `Stats` types. Pin the names you actually find.

- [ ] **Step 2: Replace stub**

```rust
//! Saturation gauges for the Redis client pool (fred crate).
//!
//! fred's `metrics` feature exposes per-client / per-pool stats. The
//! callbacks below are intentionally tolerant: if a stat is unavailable
//! in the version we use, the metric is simply not emitted (a TODO is
//! left at the call site).

use std::sync::Arc;

use opentelemetry::{KeyValue, metrics::Meter};

/// Wraps whatever fred client/pool type the rest of `service_utils` uses.
/// Update the inner type to match `crate::redis`'s public surface.
pub type RedisHandle = Arc<dyn RedisStats + Send + Sync>;

/// Tiny abstraction so the metrics module doesn't have to know fred's
/// concrete types. Implement on the wrapper that `crate::redis` already
/// hands around.
pub trait RedisStats {
    fn idle_connections(&self) -> Option<u64>;
    fn used_connections(&self) -> Option<u64>;
    fn commands_in_flight(&self) -> Option<u64>;
}

pub fn register(meter: &Meter, client: RedisHandle, pool_name: &'static str) {
    let usage_label = KeyValue::new("pool.name", pool_name);

    let c = client.clone();
    let label = usage_label.clone();
    meter
        .u64_observable_gauge("redis.client.connections.usage")
        .with_description("Number of Redis connections in idle/used state.")
        .with_callback(move |observer| {
            if let Some(idle) = c.idle_connections() {
                observer.observe(
                    idle,
                    &[KeyValue::new("state", "idle"), label.clone()],
                );
            }
            if let Some(used) = c.used_connections() {
                observer.observe(
                    used,
                    &[KeyValue::new("state", "used"), label.clone()],
                );
            }
        })
        .build();

    let c = client.clone();
    let label = usage_label.clone();
    meter
        .u64_observable_gauge("redis.client.commands.in_flight")
        .with_description("Number of Redis commands currently in flight.")
        .with_callback(move |observer| {
            if let Some(n) = c.commands_in_flight() {
                observer.observe(n, &[label.clone()]);
            }
        })
        .build();
}
```

- [ ] **Step 3: Wire `RedisStats` to whatever fred client you use**

Find the wrapper in `crate::redis` (the internal Redis surface), and `impl RedisStats for YourWrapper` using the fred metrics surface confirmed in Step 1. If a particular field is unavailable in your fred version, leave the impl returning `None` and a `// TODO(observability): expose <field> when fred …` comment.

This step is intentionally light on prescriptive code because the exact fred API depends on the pinned version. The contract is just three `Option<u64>` getters; all three returning `None` is acceptable for v1 — the metrics simply won't have data.

- [ ] **Step 4: Verify it compiles**

Run: `cargo check -p service_utils`
Expected: exit code 0.

- [ ] **Step 5: Commit**

```bash
git add crates/service_utils/src/observability/saturation/redis_pool.rs crates/service_utils/src/redis*
git commit -m "feat(observability): redis pool saturation gauges

ObservableGauge callbacks read fred client/pool stats via a thin
RedisStats trait. Tolerant to missing fields — a None return simply
omits the metric.
"
```

---

## Task 16: Tokio runtime saturation (`cfg(tokio_unstable)`)

**Files:**
- Modify: `crates/service_utils/src/observability/saturation/tokio_runtime.rs`

- [ ] **Step 1: Replace stub**

```rust
//! Tokio runtime saturation, gated on `cfg(tokio_unstable)`.
//!
//! Unlike DB/Redis, `tokio_metrics::RuntimeMonitor` is delta-based: each
//! `.intervals()` call returns stats since the last call. So we run a
//! background task that samples every `interval` and stores derived values
//! in atomics that observable-gauge callbacks read.

#[cfg(not(tokio_unstable))]
pub fn spawn(_meter: &opentelemetry::metrics::Meter, _interval: std::time::Duration) {}

#[cfg(tokio_unstable)]
mod inner {
    use std::sync::Arc;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::Duration;

    use opentelemetry::metrics::Meter;
    use tokio_metrics::RuntimeMonitor;

    #[derive(Default)]
    struct Snapshot {
        workers: AtomicU64,
        global_queue_depth: AtomicU64,
        busy_ratio_milli: AtomicU64, // busy_ratio * 1000, stored as integer
    }

    pub fn spawn(meter: &Meter, interval: Duration) {
        let handle = match tokio::runtime::Handle::try_current() {
            Ok(h) => h,
            Err(_) => return, // not running on a tokio runtime; no-op
        };
        let snap = Arc::new(Snapshot::default());

        // Background sampler.
        let snap_for_task = snap.clone();
        tokio::spawn(async move {
            let monitor = RuntimeMonitor::new(&handle);
            let mut intervals = monitor.intervals();
            loop {
                if let Some(m) = intervals.next() {
                    snap_for_task
                        .workers
                        .store(m.workers_count as u64, Ordering::Relaxed);
                    snap_for_task
                        .global_queue_depth
                        .store(m.global_queue_depth as u64, Ordering::Relaxed);
                    let busy = m.total_busy_duration.as_secs_f64();
                    let total = (m.total_polls_count as f64).max(1.0)
                        * interval.as_secs_f64()
                        * (m.workers_count as f64).max(1.0);
                    let ratio = (busy / total).clamp(0.0, 1.0);
                    snap_for_task
                        .busy_ratio_milli
                        .store((ratio * 1000.0) as u64, Ordering::Relaxed);
                }
                tokio::time::sleep(interval).await;
            }
        });

        // ObservableGauges read from the snapshot atomics.
        let s = snap.clone();
        meter
            .u64_observable_gauge("runtime.tokio.workers")
            .with_callback(move |observer| {
                observer.observe(s.workers.load(Ordering::Relaxed), &[]);
            })
            .build();

        let s = snap.clone();
        meter
            .u64_observable_gauge("runtime.tokio.global_queue.depth")
            .with_callback(move |observer| {
                observer.observe(s.global_queue_depth.load(Ordering::Relaxed), &[]);
            })
            .build();

        let s = snap.clone();
        meter
            .f64_observable_gauge("runtime.tokio.workers.busy_ratio")
            .with_callback(move |observer| {
                let milli = s.busy_ratio_milli.load(Ordering::Relaxed);
                observer.observe(milli as f64 / 1000.0, &[]);
            })
            .build();
    }
}

#[cfg(tokio_unstable)]
pub use inner::spawn;
```

- [ ] **Step 2: Verify it compiles both with and without the cfg**

Run: `cargo check -p service_utils`
Expected: exit code 0 (the workspace `.cargo/config.toml` enables `tokio_unstable`, so the `inner` branch compiles).

Run: `RUSTFLAGS="" cargo check -p service_utils`
Expected: exit code 0 (the `not(tokio_unstable)` no-op stub compiles when the flag is off).

If `tokio_metrics::RuntimeMonitor::intervals()` field names differ from those used above (`workers_count`, `global_queue_depth`, `total_busy_duration`, `total_polls_count`), adjust to match. The fundamental shape — `.intervals()` returning a delta iterator — is stable across recent versions.

- [ ] **Step 3: Commit**

```bash
git add crates/service_utils/src/observability/saturation/tokio_runtime.rs
git commit -m "feat(observability): tokio runtime saturation gauges

Background sampler updates atomic snapshots; observable gauges read
from the snapshot. Gated on cfg(tokio_unstable); compiles to a no-op
when the flag is disabled.
"
```

---

## Task 17: Add health paths to `tenant_middleware_exclusion_list`

**Files:**
- Modify: `crates/superposition/src/app_state.rs`

- [ ] **Step 1: Make health paths always-excluded**

Replace lines 101–107 of `crates/superposition/src/app_state.rs` (the `tenant_middleware_exclusion_list` field assignment). Use `Read` first to confirm the surrounding context, then `Edit`:

```rust
        tenant_middleware_exclusion_list: {
            let mut set = get_from_env_unsafe::<String>(
                "TENANT_MIDDLEWARE_EXCLUSION_LIST",
            )
            .expect("TENANT_MIDDLEWARE_EXCLUSION_LIST is not set")
            .split(',')
            .map(String::from)
            .collect::<HashSet<_>>();
            // Always exclude observability health endpoints from auth checks.
            set.extend(
                service_utils::observability::health_endpoint_paths()
                    .iter()
                    .map(|s| s.to_string()),
            );
            set
        },
```

- [ ] **Step 2: Verify it compiles**

Run: `cargo check -p superposition`
Expected: exit code 0.

- [ ] **Step 3: Commit**

```bash
git add crates/superposition/src/app_state.rs
git commit -m "feat: exclude /healthz /livez /readyz from auth checks

Adds the observability health paths to tenant_middleware_exclusion_list
so probes do not trigger auth flow. Operators no longer need to remember
to put these in TENANT_MIDDLEWARE_EXCLUSION_LIST.
"
```

---

## Task 18: Wire observability into `main.rs`

**Files:**
- Modify: `crates/superposition/src/main.rs`

- [ ] **Step 1: Add imports + early init**

At the top of `main()` in `crates/superposition/src/main.rs` (after tracing init, before app_state construction), add:

```rust
    use service_utils::observability::{
        self, MetricsMiddleware, Observability, ObservabilityConfig, SaturationDeps,
    };

    let obs_cfg = ObservabilityConfig::from_env()
        .expect("invalid observability env config");
    let obs_enabled = obs_cfg.enabled;
    let observability = if obs_enabled {
        Some(Observability::init(obs_cfg.clone()).expect("observability init failed"))
    } else {
        None
    };
    let metrics_meter = observability.as_ref().map(|o| o.meter());
    let metrics_label_cfg = obs_cfg.label;
```

- [ ] **Step 2: Register saturation observers**

After `app_state` is built (so the DB pool is available) and inside the tokio runtime context, add:

```rust
    if let (Some(obs), Some(pool)) = (observability.as_ref(), Some(app_state.db_pool.clone())) {
        observability::register_observers(
            &obs.meter(),
            SaturationDeps {
                db_pool: Some(pool),
                redis_client: app_state.redis_client.clone().map(Into::into),
                tokio_collect_interval: obs_cfg.collect_interval,
            },
        )
        .expect("saturation observer registration failed");
    }
```

(Adjust `app_state.db_pool` and `app_state.redis_client` to whatever fields actually exist on `AppState`. If the redis client isn't easily Arc-wrapped, pass `None` and leave the metric unattached for now.)

- [ ] **Step 3: Spawn the metrics server**

After the saturation registration, add:

```rust
    let metrics_server_handle = if let Some(obs) = observability.as_ref() {
        let bind: std::net::SocketAddr = format!("{}:{}", obs_cfg.bind, obs_cfg.port)
            .parse()
            .expect("invalid metrics bind addr");
        Some(observability::spawn_metrics_server(obs.registry(), bind)?)
    } else {
        None
    };
```

- [ ] **Step 4: Add the middleware to the App builder**

In the `HttpServer::new(move || App::new()…)` closure, add the middleware as the *outermost* `.wrap()` (i.e., the *last* `.wrap()` in the chain — Actix runs the last-wrapped middleware first):

```rust
            .service(observability::health_endpoints())
            // ... existing .service() and .wrap() calls (auth_z, auth_n, ...
            //     RequestResponseLogger, TracingLogger) ...
            .wrap(actix_web::middleware::Condition::new(
                obs_enabled,
                metrics_meter
                    .as_ref()
                    .map(|m| MetricsMiddleware::new(m, metrics_label_cfg))
                    .unwrap_or_else(|| MetricsMiddleware::new(
                        // construct a no-op meter for the disabled case
                        &opentelemetry::global::meter("noop"),
                        metrics_label_cfg,
                    )),
            ))
            .wrap(TracingLogger::<CustomRootSpanBuilder>::new())
```

The `Condition` wrapper makes the middleware a no-op when `SUPERPOSITION_METRICS_ENABLED=false`. Match the `&meter` borrow shape that `MetricsMiddleware::new` expects.

- [ ] **Step 5: Run both servers concurrently**

Replace the final `.run().await` with a `try_join!` over the main and metrics servers. Example:

```rust
    let main_server = HttpServer::new(/* ... */)
        .bind(("0.0.0.0", cac_port))?
        .workers(get_from_env_or_default("ACTIX_WORKER_COUNT", 5))
        .keep_alive(Duration::from_secs(
            get_from_env_unsafe("ACTIX_KEEP_ALIVE").unwrap_or(120),
        ))
        .run();

    match metrics_server_handle {
        Some(metrics) => {
            futures_util::try_join!(main_server, metrics)?;
        }
        None => {
            main_server.await?;
        }
    }
```

(`futures_util` is already a workspace dep — see root `Cargo.toml`.)

- [ ] **Step 6: Build the binary**

Run: `cargo build -p superposition`
Expected: exit code 0. Compilation errors here are the hardest part of the wiring; iterate on imports and types until clean.

- [ ] **Step 7: Smoke-test locally**

Start the binary against the local docker-compose dev stack:

```bash
make run    # or whatever the makefile target is
```

In another shell:

```bash
curl -s -i http://localhost:8080/healthz
curl -s http://localhost:9091/metrics | head -50
```

Expected:

- `/healthz` returns `200 OK` with body `ok`.
- `/metrics` returns Prometheus exposition that includes lines starting with `# HELP http_server_request_duration_seconds`, `http_server_active_requests`, `http_server_busy_duration_seconds_total`, and (after issuing a few API requests) `http_server_request_duration_seconds_bucket{...}` lines with `http_route` labels.

Stop the binary.

- [ ] **Step 8: Commit**

```bash
git add crates/superposition/src/main.rs
git commit -m "feat: wire observability into main binary

- Init Observability early (Prometheus exporter + optional OTLP push)
- Spawn metrics server on SUPERPOSITION_METRICS_PORT
- Register DB/Redis/Tokio saturation observers
- Wrap App with MetricsMiddleware (gated by SUPERPOSITION_METRICS_ENABLED)
- Mount /healthz /livez /readyz on the main app
- try_join! both servers so the process exits if either dies
"
```

---

## Task 19: Integration test — full pipeline through `/metrics`

**Files:**
- Create: `crates/service_utils/tests/observability_integration.rs`

- [ ] **Step 1: Write the test**

```rust
//! End-to-end test: an Actix app wrapped with MetricsMiddleware serves several
//! routes; we then issue requests and parse the Prometheus scrape output to
//! assert on the metrics that should appear.

use actix_web::{App, HttpResponse, http::StatusCode, test, web};
use prometheus::Encoder;
use service_utils::observability::{
    LabelConfig, MetricsMiddleware, Observability, ObservabilityConfig,
};

fn cfg() -> ObservabilityConfig {
    ObservabilityConfig {
        enabled: true,
        bind: "127.0.0.1".parse().unwrap(),
        port: 0,
        label: LabelConfig::default(),
        collect_interval: std::time::Duration::from_secs(10),
        instance_id: "it".into(),
        service_name: "sp-it".into(),
        service_version: "0".into(),
        deployment_environment: None,
        otlp_endpoint: None,
    }
}

fn scrape(obs: &Observability) -> String {
    let metric_families = obs.registry().gather();
    let mut buf = Vec::new();
    prometheus::TextEncoder::new()
        .encode(&metric_families, &mut buf)
        .unwrap();
    String::from_utf8(buf).unwrap()
}

#[actix_web::test]
async fn metrics_appear_after_requests() {
    let obs = Observability::init(cfg()).unwrap();
    let mw = MetricsMiddleware::new(&obs.meter(), LabelConfig::default());
    let app = test::init_service(
        App::new()
            .wrap(mw)
            .route("/ping", web::get().to(|| async { HttpResponse::Ok() }))
            .route(
                "/echo/{name}",
                web::post().to(|p: web::Path<String>| async move {
                    HttpResponse::Created().body(p.into_inner())
                }),
            )
            .route(
                "/boom",
                web::get().to(|| async { HttpResponse::InternalServerError() }),
            ),
    )
    .await;

    for _ in 0..3 {
        let req = test::TestRequest::get().uri("/ping").to_request();
        let resp = test::call_service(&app, req).await;
        assert_eq!(resp.status(), StatusCode::OK);
    }
    let req = test::TestRequest::post().uri("/echo/world").to_request();
    let resp = test::call_service(&app, req).await;
    assert_eq!(resp.status(), StatusCode::CREATED);

    let req = test::TestRequest::get().uri("/boom").to_request();
    let resp = test::call_service(&app, req).await;
    assert_eq!(resp.status(), StatusCode::INTERNAL_SERVER_ERROR);

    let req = test::TestRequest::get().uri("/no-such-route").to_request();
    let resp = test::call_service(&app, req).await;
    assert_eq!(resp.status(), StatusCode::NOT_FOUND);

    let body = scrape(&obs);

    // Request duration histogram exists with expected labels for /ping (3 hits).
    let ping_count_line = body
        .lines()
        .find(|l| {
            l.starts_with("http_server_request_duration_seconds_count{")
                && l.contains("http_route=\"/ping\"")
                && l.contains("http_request_method=\"GET\"")
                && l.contains("http_response_status_code=\"200\"")
        })
        .unwrap_or_else(|| panic!("no /ping count line in:\n{body}"));
    let ping_count: f64 = ping_count_line
        .rsplit_once(' ')
        .unwrap()
        .1
        .trim()
        .parse()
        .unwrap();
    assert_eq!(ping_count as u64, 3);

    // 5xx series for /boom appears.
    assert!(
        body.lines().any(|l| {
            l.starts_with("http_server_request_duration_seconds_count{")
                && l.contains("http_route=\"/boom\"")
                && l.contains("http_response_status_code=\"500\"")
        }),
        "no /boom 500 series in:\n{body}"
    );

    // Unmatched path uses the sentinel.
    assert!(
        body.lines().any(|l| {
            l.starts_with("http_server_request_duration_seconds_count{")
                && l.contains("http_route=\"__not_found__\"")
        }),
        "no __not_found__ series in:\n{body}"
    );

    // busy_duration_total > 0
    let busy = body
        .lines()
        .find(|l| l.starts_with("http_server_busy_duration_seconds_total{"))
        .unwrap_or_else(|| panic!("no busy_duration line in:\n{body}"));
    let busy_value: f64 = busy.rsplit_once(' ').unwrap().1.trim().parse().unwrap();
    assert!(busy_value > 0.0, "expected busy_duration > 0, got {busy_value}");

    // active_requests returns to 0 after all requests complete.
    let active_lines: Vec<_> = body
        .lines()
        .filter(|l| l.starts_with("http_server_active_requests{"))
        .collect();
    for line in &active_lines {
        let v: f64 = line.rsplit_once(' ').unwrap().1.trim().parse().unwrap();
        assert_eq!(v, 0.0, "active_requests not zero: {line}");
    }
}
```

- [ ] **Step 2: Run the test**

Run: `cargo test -p service_utils --test observability_integration`
Expected: 1 test passes.

- [ ] **Step 3: Commit**

```bash
git add crates/service_utils/tests/observability_integration.rs
git commit -m "test(observability): end-to-end integration

Wraps a small App with MetricsMiddleware, issues requests of various
shapes (200, 201, 500, 404), and asserts on the parsed Prometheus
exposition: per-route counts, the 5xx series, the __not_found__
sentinel, busy_duration > 0, and active_requests returning to 0.
"
```

---

## Task 20: Cardinality regression test

**Files:**
- Modify: `crates/service_utils/tests/observability_integration.rs`

- [ ] **Step 1: Add the test**

Append to the integration test file:

```rust
#[actix_web::test]
async fn cardinality_stays_within_budget() {
    let obs = Observability::init(cfg()).unwrap();
    let mw = MetricsMiddleware::new(&obs.meter(), LabelConfig::default());
    let app = test::init_service(
        App::new()
            .wrap(mw)
            .route("/a", web::get().to(|| async { HttpResponse::Ok() }))
            .route("/b", web::get().to(|| async { HttpResponse::Ok() }))
            .route("/c", web::post().to(|| async { HttpResponse::Created() })),
    )
    .await;

    for _ in 0..10 {
        for path in &["/a", "/b"] {
            let req = test::TestRequest::get().uri(path).to_request();
            let _ = test::call_service(&app, req).await;
        }
        let req = test::TestRequest::post().uri("/c").to_request();
        let _ = test::call_service(&app, req).await;
    }

    let body = scrape(&obs);
    let series = body
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .count();

    // Budget for this scenario: 3 routes × 1 method each × 1 status × ~12
    // (10 buckets + sum + count) = ~36 series for the histogram, plus 3 for
    // busy_duration, plus 1 for active_requests, plus a few from `target_info`
    // that the prometheus exporter emits. Headroom: 200.
    assert!(series <= 200, "cardinality regression: {series} series\n{body}");
}
```

- [ ] **Step 2: Run the test**

Run: `cargo test -p service_utils --test observability_integration`
Expected: 2 tests pass.

- [ ] **Step 3: Commit**

```bash
git add crates/service_utils/tests/observability_integration.rs
git commit -m "test(observability): cardinality regression budget

Asserts that a 3-route × 1-method × 1-status scenario produces no more
than 200 series, catching accidental high-cardinality labels in review.
"
```

---

## Task 21: Update README + makefile note

**Files:**
- Modify: `README.md`
- Modify: `makefile`

- [ ] **Step 1: Add a section to README**

Find the build/development section and append:

````markdown
### Metrics & observability

The HTTP API exposes Prometheus metrics on `SUPERPOSITION_METRICS_PORT` (default `9091`):

```
curl http://localhost:9091/metrics
```

Health endpoints live on the main port: `GET /healthz`, `/livez`, `/readyz`.

For full details (labels, cardinality, OTLP push), see
[`docs/superpowers/specs/2026-05-10-otel-golden-signals-middleware-design.md`](docs/superpowers/specs/2026-05-10-otel-golden-signals-middleware-design.md).

**Note on `tokio_unstable`.** The workspace's `.cargo/config.toml` enables
`--cfg tokio_unstable` so `tokio-metrics` can collect runtime saturation. This
flag only adds APIs; no behavioural change for existing code. Contributors who
build outside `cargo` (e.g., custom IDE invocations) should pass the same flag,
or accept that the `runtime.tokio.*` metrics will be absent.
````

- [ ] **Step 2: Sanity-check the makefile**

Read the makefile's `build`/`run` targets. If they invoke `cargo` plainly, no change is needed (the `.cargo/config.toml` is picked up automatically). If they set `RUSTFLAGS=` explicitly anywhere, ensure `--cfg tokio_unstable` is preserved.

- [ ] **Step 3: Commit**

```bash
git add README.md makefile
git commit -m "docs: note metrics endpoints and tokio_unstable build flag"
```

---

## Task 22: Final smoke-test pass

**Files:** none (verification only)

- [ ] **Step 1: Full test suite**

Run: `cargo test --workspace`
Expected: all tests pass (including the integration test from Task 19).

- [ ] **Step 2: Build with metrics disabled**

Run:

```bash
SUPERPOSITION_METRICS_ENABLED=false cargo build -p superposition
```

Expected: builds cleanly.

- [ ] **Step 3: Build without `tokio_unstable`**

Run:

```bash
RUSTFLAGS="" cargo check -p service_utils
```

Expected: builds cleanly (the `not(tokio_unstable)` no-op stub is exercised).

- [ ] **Step 4: Live smoke**

Bring up the dev stack and verify metrics flow into Prometheus / VictoriaMetrics:

```bash
docker compose -f grafana/docker-compose.yaml up -d
make run    # or whatever the makefile target is
```

Add a scrape target to `grafana/prometheus.yml` for `host.docker.internal:9091` (Mac) or the host IP (Linux), reload Prometheus (`docker compose restart prometheus`), and verify in the Prometheus UI's Targets page that the new target is `UP`. Query `http_server_request_duration_seconds_count` and confirm series with `http_route` labels appear after issuing a few requests.

Stop the dev stack.

- [ ] **Step 5: Commit (if any docs changed during smoke)**

If the Prometheus scrape config got a new entry, commit it:

```bash
git add grafana/prometheus.yml
git commit -m "chore(grafana): scrape superposition metrics endpoint"
```

Otherwise, no commit.

---

## Notes for self-review (already incorporated)

- **Spec coverage.** Every section of the spec maps to a task: §5 architecture → Tasks 4, 18; §6 module structure → Tasks 4, 11, 12, 13, 14, 15, 16; §7 dependencies → Tasks 1, 3; §8 metric definitions → Tasks 7, 11, 14, 15, 16; §9 middleware mechanics → Tasks 6, 8, 9, 10, 11; §10 saturation collectors → Tasks 14, 15, 16; §11 configuration → Task 5; §12 testing strategy → Tasks 19, 20; §13 rollout — handled at deployment time, not in code (env var defaults); §14 future work — explicitly out of scope.
- **Type consistency.** `HttpMeters` (Task 7), `ObservabilityConfig`/`LabelConfig` (Task 5), `Observability` (Tasks 4, 7), and the helper functions in `middleware.rs` (Tasks 6–11) all use consistent names across tasks.
- **Build-flag duality.** Task 16 explicitly tests both with and without `tokio_unstable`. Task 22 retests this at the end as a regression check.
- **Auth bypass mechanism.** Task 17 wires the health paths into the existing `tenant_middleware_exclusion_list` machinery (verified in Task spec §5.2 against `crates/service_utils/src/middlewares/auth_n.rs:44–60`), not the incorrect "register before auth_n" pattern that was in an earlier draft of the spec.
