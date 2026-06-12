# OpenTelemetry Golden-Signals Middleware

- **Date:** 2026-05-10 (original), updated post-implementation to match shipped code
- **Status:** Shipped (PR #998); this document reflects the as-built design
- **Owner:** Natarajan Kannan
- **Target crate:** `service_utils`
- **Reference TSDB:** VictoriaMetrics (single-node `vmsingle`); design is TSDB-agnostic

## 0. Changes from initial design

The original 2026-05-10 design was edited inline after the PR shipped so the body reads as truth. The original prose is recoverable from git history (`git log -p` this file). Headlines:

| Area | Change |
|---|---|
| HTTP middleware | Hand-written `MetricsMiddleware` replaced with `opentelemetry-instrumentation-actix-web 0.24` (`RequestMetrics`); we plug in a custom `RouteFormatter` + `metric_attrs_from_req` to preserve our cardinality story. |
| HTTP saturation | `http.server.busy.duration` Counter dropped; equivalent signal is `rate(http_server_request_duration_seconds_sum[...])`. |
| Tokio runtime | `tokio_unstable` flag, `tokio-metrics` dep, and `.cargo/config.toml` removed; tokio 1.50 stable APIs (`Handle::metrics()`) suffice. `busy_ratio` Gauge replaced with `runtime.tokio.workers.busy.time` Counter. |
| Health endpoints | `/healthz`, `/livez`, `/readyz` dropped — existing `GET /health` covers the up-check role; k8s-conventional split deferred. |
| Deps | OTel family bumped to `0.32`, `prometheus` to `0.14`; `opentelemetry-semantic-conventions` and `tokio-metrics` removed. |

## 1. Background

Superposition's Actix-web HTTP API has structured tracing via `tracing-actix-web` and a `RequestResponseLogger` middleware that emits a single `info!(latency = …, "GoldenSignal")` log line per request. There is no Prometheus/OpenTelemetry client, no `/metrics` endpoint, and no per-process gauge for in-flight work, DB pool state, or runtime saturation. The repository ships a `grafana/` directory with Prometheus + Grafana docker-compose and a Python `custom-exporter`, but no application metrics flow through it.

This design adds first-class metrics exposition for the four [Google SRE golden signals](https://sre.google/sre-book/monitoring-distributed-systems/) — **latency, traffic, errors, saturation** — using OpenTelemetry, with VictoriaMetrics as the reference scrape target. Instrumentation is applied via Actix middleware so any existing or future API endpoint is covered automatically.

## 2. Goals

1. Expose Prometheus-format metrics for every HTTP route on the main API, covering latency / traffic / errors / saturation, without per-handler code changes.
2. Emit OpenTelemetry semantic-convention metric names so any OTel-native backend (VictoriaMetrics, Prometheus, Grafana Mimir, SigNoz, OpenObserve, Datadog, Honeycomb, …) can ingest them.
3. Support both **pull** (Prometheus scrape on a dedicated port) and **push** (OTLP HTTP/gRPC) exposition; users choose at deployment time via standard OTel env vars.
4. Capture saturation signals beyond HTTP — DB connection pool, Redis connection pool, Tokio runtime — so a real "is this process overloaded?" view is possible.
5. Keep the per-request overhead low (single-digit microseconds) and keep cardinality bounded by design.
6. Provide a clean configuration surface so operators can disable high-cardinality labels (notably `workspace_id`) without code changes.

## 3. Non-goals

- **Trace correlation via exemplars.** Linking percentile spikes to specific traces requires `tracing-opentelemetry` to be wired through the existing tracing setup. Out of scope for this spec; a follow-up.
- **OTLP traces export.** The same SDK init code is structured to host trace export later, but this spec covers metrics only.
- **Per-tenant separate histograms.** Considered (option D in §11.1) and deferred until the global histogram's cardinality budget proves tight in production.
- **Grafana dashboards.** A separate PR will commit JSON dashboards under `grafana/dashboards/` covering the four golden-signal panels.
- **Alert rules.** A separate PR will commit VM/Prometheus alert rule YAML.
- **Removing the existing `info!(… "GoldenSignal")` log.** Stays for now; downstream tooling may consume it. Marked for removal once dashboards have migrated.
- **Instrumenting non-HTTP work** (background jobs, DB query timing per query). Out of scope for v1.

## 4. Decisions summary

| Decision | Choice | Rationale |
|---|---|---|
| Client library | OpenTelemetry SDK + Prometheus exporter | TSDB-agnostic; future-proof for OSS users; future-proofs unified traces+metrics. |
| HTTP middleware | `opentelemetry-instrumentation-actix-web 0.24` (`RequestMetrics`) with custom `RouteFormatter` + `metric_attrs_from_req` | Upstream OTel-contrib crate; gets us semconv-compliant `http.server.request.duration` for free. Cardinality controls (route sentinels, method normalization, optional org/workspace labels) plug in via the crate's hooks. |
| TSDB (reference) | VictoriaMetrics (`vmsingle`) | Cheap to operate; Prom-compatible; cluster path exists if needed. |
| Exposition transport | Prometheus scrape on dedicated port + optional OTLP push | Pull-by-default for self-hosted users; OTLP path unlocks every OTel-native backend. |
| Labels on HTTP metrics | `route × method × status × org × workspace` | Tenant-level slicing in metrics; org / workspace labels are independently env-disable-able for users with very high workspace counts. |
| Saturation signals | HTTP active requests + Tokio runtime busy time + DB pool + Redis pool | Multiple independent signals avoid single-metric blind spots. Time-averaged HTTP concurrency is derivable from `rate(http_server_request_duration_seconds_sum[...])` — no separate Counter needed. Host-level (CPU/mem/FD) stays with `node-exporter`. |
| Where `/metrics` lives | Separate listener on `SUPERPOSITION_METRICS_PORT` (default `9091`) | Network-policy isolation; scrape requests don't pollute the app's own metrics; no auth interaction. |
| Module location | `crates/service_utils/src/observability.rs` (+ `observability/` for submodules, no `mod.rs`) | Matches existing convention for cross-cutting concerns; modern Rust 2018+ module layout. |
| Body-size histograms | Suppressed via SDK View (`Aggregation::Drop`) | The upstream crate emits `http.server.request.body.size` and `http.server.response.body.size` unconditionally; we have no operator demand for them and they add cardinality. The View suppresses them at the SDK layer without forking the crate. |

## 5. Architecture

A new module **`service_utils::observability`** owns three pieces:

1. **`Observability::init()`** — called once from `main.rs` early in startup. Builds the OTel `SdkMeterProvider` with two readers: a `PrometheusExporter` (renders to `/metrics`) and (if `OTEL_EXPORTER_OTLP_ENDPOINT` is set) a `PeriodicReader` driving an `opentelemetry-otlp` HTTP exporter. The provider has an SDK View attached that drops the body-size histograms the upstream HTTP-instrumentation crate emits. Returns an `Observability` handle owning the registry, a cloned `SdkMeterProvider`, a `Meter`, and shutdown hooks.

2. **HTTP instrumentation** — the upstream `opentelemetry-instrumentation-actix-web` crate's `RequestMetrics` middleware, built via `build_request_metrics_middleware(provider)`. It's wired with:
   - a custom `RouteFormatter` (`CardinalityBoundedFormatter`) that collapses `"default"` to `__not_found__` and `/pkg|/assets|/favicon*` to `__static__`,
   - a custom `metric_attrs_from_req` fn pointer that normalizes the HTTP method (unknown verbs → `_OTHER`) and conditionally attaches `sp.org_id` / `sp.workspace_id` from request extensions per a process-wide `LabelConfig` (installed via `set_label_config()` on a `OnceLock`).

   The crate emits, per OTel HTTP semconv:
   - `http.server.request.duration` (Histogram, seconds) — drives latency, traffic, and error metrics
   - `http.server.active_requests` (UpDownCounter)
   - `http.server.request.body.size` and `http.server.response.body.size` (Histograms) — **dropped via SDK View**

3. **`saturation::*`** — observable-gauge callbacks for r2d2 (DB pool) and fred (Redis pool), plus three observable instruments reading `tokio::runtime::Handle::metrics()` directly. **No background tasks.** All emit OTel-namespaced metrics.

A second component, **`metrics_server`**, is a separate `actix_web::HttpServer` on `SUPERPOSITION_METRICS_PORT` that exposes:

- `GET /metrics` — Prometheus exposition rendered from the OTel registry

The main server (port `8080`) gets one new `.wrap(Condition::new(obs_enabled, metrics_middleware.clone()))` line. No new route registrations.

### 5.1 Data flow

```text
[request on :8080]
   ├── tracing-actix-web ─→ span
   ├── (auth_n / auth_z) ─→ extensions: org_id, workspace_id
   ├── RequestMetrics middleware ─→ start timer, inc active_requests, capture method
   │     └── handler runs
   └── RequestMetrics middleware ─→ record duration histogram, dec active_requests;
                                    attributes built via metric_attrs_from_req:
                                    method, http.route, status, sp.org_id?, sp.workspace_id?

[scrape on :9091/metrics] ←── PrometheusExporter ←── SdkMeterProvider ←── (HTTP instrumentation + saturation collectors)
                                                                       └─→ (optional) OTLP HTTP push to OTEL_EXPORTER_OTLP_ENDPOINT

[saturation, callback-driven]
   ObservableGauge.with_callback(|obs| obs.observe(pool.state(), …))     // r2d2 DB pool
   ObservableGauge.with_callback(|obs| obs.observe(client.metrics(), …)) // fred Redis pool
   ObservableGauge.with_callback(|obs| obs.observe(handle.metrics(), …)) // tokio runtime
   ObservableCounter.with_callback(|obs| obs.observe(sum_busy_time, …))  // tokio busy time
```

### 5.2 Middleware ordering (critical)

The `RequestMetrics` middleware must run *outside* `auth_n` / `auth_z` / `OrgWorkspaceMiddlewareFactory` so that, in the response phase, the `metric_attrs_from_req` hook can read `org_id` / `workspace_id` from request extensions. In Actix, the last `.wrap()` runs first on requests, so the registration chain in `main.rs` looks like:

```rust
App::new()
    .service(/* main api scopes */)
    // Auth innermost so outer middlewares still run on auth failures.
    .wrap(auth_z.clone())
    .wrap(auth_n.clone())
    .wrap(/* DefaultHeaders, Compress as today */)
    .wrap(RequestResponseLogger)
    .wrap(Condition::new(obs_enabled, metrics_middleware.clone()))  // observability — outermost wrap
    .wrap(TracingLogger::<CustomRootSpanBuilder>::new())            // outermost: span covers everything
```

`Condition::new` makes the wrap a no-op when `SUPERPOSITION_METRICS_ENABLED=false`, so disabling observability genuinely removes the middleware from the request path rather than gating inside it.

## 6. Module structure

All new code under `crates/service_utils`:

```text
crates/service_utils/src/
  observability.rs              -- pub use surface: Observability handle, errors, re-exports
  observability/
    config.rs                   -- ObservabilityConfig + LabelConfig parsed from env
    instrumentation.rs          -- CardinalityBoundedFormatter, metric_attrs_from_req,
                                   build_request_metrics_middleware, set_label_config,
                                   ROUTE_NOT_FOUND / ROUTE_STATIC sentinels
    metrics_server.rs           -- HttpServer on SUPERPOSITION_METRICS_PORT exposing /metrics
    saturation.rs               -- SaturationDeps, register_observers
    saturation/
      db_pool.rs                -- r2d2 ObservableGauge callbacks
      redis_pool.rs             -- fred ObservableGauge callbacks (no-op if Redis not configured)
      tokio_runtime.rs          -- ObservableGauge / ObservableCounter reading Handle::metrics() directly
  tests/observability_integration.rs  -- end-to-end pipeline tests
```

Files use the modern Rust 2018+ module layout (no `mod.rs`). `crates/service_utils/src/middlewares/` is left untouched.

### 6.1 Public API sketch

```rust
// observability.rs (re-exports)
pub use config::{LabelConfig, ObservabilityConfig};
pub use instrumentation::{
    ROUTE_NOT_FOUND, ROUTE_STATIC,
    build_request_metrics_middleware, set_label_config,
};
pub use metrics_server::spawn_metrics_server;
pub use opentelemetry_sdk::metrics::SdkMeterProvider;  // re-exported so callers don't need a direct dep
pub use saturation::{
    DbPoolHandle, FredPoolStats, RedisHandle, RedisStats, SaturationDeps, register_observers,
};

pub struct Observability { /* provider, registry, meter, otlp_handle, ... */ }

impl Observability {
    pub fn init(cfg: ObservabilityConfig) -> Result<Self, ObservabilityError>;
    pub fn meter(&self) -> opentelemetry::metrics::Meter;
    pub fn meter_provider(&self) -> &SdkMeterProvider;
    pub fn registry(&self) -> std::sync::Arc<prometheus::Registry>;
    pub fn shutdown(self) -> Result<(), ObservabilityError>;
}

// instrumentation.rs
pub fn build_request_metrics_middleware(
    provider: &SdkMeterProvider,
) -> opentelemetry_instrumentation_actix_web::RequestMetrics;

/// Install the process-wide LabelConfig used by `metric_attrs_from_req`. Must be
/// called before the first request flows through the middleware. Idempotent —
/// later calls are ignored (it's a `OnceLock`). Trade-off: changing labels at
/// runtime requires a redeploy; this is acceptable for an env-driven config
/// surface.
pub fn set_label_config(cfg: LabelConfig);

// saturation.rs
pub struct SaturationDeps {
    pub db_pool: Option<DbPoolHandle>,
    pub redis_client: Option<RedisHandle>,
}

pub fn register_observers(
    meter: &Meter,
    deps: SaturationDeps,
) -> Result<(), ObservabilityError>;
```

`DbPoolHandle` is the bare `diesel::r2d2::Pool<ConnectionManager<PgConnection>>` (r2d2's `Pool` is internally `Arc`-cloneable, so no outer `Arc` wrapping). `RedisHandle = Arc<dyn RedisStats + Send + Sync>` because the trait-object indirection is necessary for the fred decoupling.

## 7. Dependencies

Added to root `Cargo.toml` `[workspace.dependencies]` and enabled in `crates/service_utils/Cargo.toml`.

| Crate | Version | Purpose |
|---|---|---|
| `opentelemetry` | 0.32 | API surface: `Meter`, `Histogram`, `UpDownCounter`, `ObservableGauge`, `ObservableCounter` |
| `opentelemetry_sdk` | 0.32 | SDK: `SdkMeterProvider`, `PeriodicReader`, `Resource`, SDK Views, `rt-tokio` runtime feature |
| `opentelemetry-prometheus` | 0.32 | Bridge OTel → `prometheus::Registry` for scrape exposition |
| `opentelemetry-otlp` | 0.32 | OTLP HTTP push exporter (used when `OTEL_EXPORTER_OTLP_ENDPOINT` is set) |
| `opentelemetry-instrumentation-actix-web` | 0.24 | Upstream OTel-contrib `RequestMetrics` middleware (with the `metrics` feature) |
| `prometheus` | 0.14 | Required by `opentelemetry-prometheus` for `Registry` and `TextEncoder` |
| `humantime` | 2.1 | Parses `SUPERPOSITION_METRICS_COLLECT_INTERVAL` (`"10s"`, `"1m30s"`, …) |

`fred` already has its `metrics` feature enabled; `tokio` is at ≥1.50 (stable `Handle::metrics()`).

Notable crates **not** depended on:

- `opentelemetry-semantic-conventions` — the four attribute names we set (`http.request.method`, `http.route`, `http.response.status_code`, `error.type`) are inlined as string literals.
- `tokio-metrics` — stable tokio APIs cover what we need.
- `opentelemetry` is **not** a direct dep of `crates/superposition`; `service_utils::observability` re-exports `SdkMeterProvider` so the binary crate doesn't need its own OTel dep.

## 8. Metric definitions

All names follow OpenTelemetry semantic conventions where they exist; saturation metrics use OTel namespaces (`db.client.*`, `runtime.*`). The Prometheus exporter translates dots to underscores and appends `_seconds` to histograms with unit `s`, etc.

### 8.1 HTTP — golden signals

#### Latency, traffic, errors

One histogram covers all three. Traffic is `rate(_count)`; errors are `rate(_count{status_code=~"5.."})`. No separate counter is needed.

| Field | Value |
|---|---|
| Name | `http.server.request.duration` |
| Type | Histogram (f64, seconds) |
| Unit | `s` |
| Buckets (explicit) | `[0.005, 0.025, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]` (9 + `+Inf` = 10 buckets) |
| Attributes | `http.request.method`, `http.response.status_code`, `http.route`, `sp.org_id`*, `sp.workspace_id`* |

\* env-controlled, default on. Disable: `SUPERPOSITION_METRICS_LABEL_ORG=false`, `SUPERPOSITION_METRICS_LABEL_WORKSPACE=false`.

**Bucket rationale.** Most p50/p95/p99 for a config-fetch service land in 5 ms – 500 ms; the 1 s / 2.5 s / 5 s / 10 s buckets exist to detect tail badness, not to give resolution there. Halving from OTel's 15-bucket default cuts series count nearly in half — a direct cardinality win.

**Derived expressions** (PromQL/MetricsQL):

```promql
# Traffic — requests/sec by route
sum(rate(http_server_request_duration_seconds_count[1m])) by (http_route)

# Error rate — 5xx fraction by route
sum(rate(http_server_request_duration_seconds_count{http_response_status_code=~"5.."}[1m])) by (http_route)
  /
sum(rate(http_server_request_duration_seconds_count[1m])) by (http_route)

# Latency — p99 by route
histogram_quantile(0.99,
  sum(rate(http_server_request_duration_seconds_bucket[1m])) by (le, http_route))
```

#### Saturation — HTTP

One metric emitted directly; the smooth saturation signal is derived from the duration histogram.

| Field | Value |
|---|---|
| Name | `http.server.active_requests` |
| Type | UpDownCounter |
| Attributes | `http.request.method` |
| Semantics | OTel semconv standard. Instantaneous value at scrape time. For sub-100ms services this metric aliases badly; not the primary saturation signal. Kept for semconv compliance and dashboards that expect it. |

**Time-averaged concurrency** (the smooth, alert-safe signal) is computed at query time:

```promql
# Average request concurrency over the last minute, per route — equivalent to
# what a dedicated `http.server.busy.duration` Counter would have given via
# `rate(...)`. The histogram _sum is monotonically increasing by `elapsed` on
# each completed request, so its rate over a window is Σ elapsed / window —
# which is exactly time-averaged concurrency by Little's Law.
sum(rate(http_server_request_duration_seconds_sum[1m])) by (http_route)
```

Why no separate `http.server.busy.duration` Counter: it would increment by `elapsed` on each completed request, but so does the histogram's implicit `_sum`. Same numerator, same denominator under `rate(...)`, no new information. Dropping the Counter saves an emit per request and one Prometheus series per `(route, method)` pair.

This value can exceed worker count for I/O-bound work — expected, not a bug, since Tokio workers are not 1:1 with requests.

### 8.2 DB pool saturation (`saturation::db_pool`)

OTel `db.client.*` semantic conventions. Backed by `r2d2::Pool::state()` via observable callbacks — purely passive instrumentation, no changes at `pool.get()` call sites.

| Name | Type | Attributes | Source |
|---|---|---|---|
| `db.client.connections.usage` | UpDownCounter (observable) | `state="idle"\|"used"`, `pool.name` | `state.idle_connections`; `state.connections - state.idle_connections` |
| `db.client.connections.max` | Gauge (observable) | `pool.name` | `pool.max_size()` |

`pool.name` is `"primary"` initially; the API supports multiple pools later.

**Not in v1** (deferred to follow-up):

- `db.client.connection.wait.duration` (histogram) — would require timing every `pool.get()` invocation across the codebase.
- `db.client.connections.pending_requests` (gauge) — would require atomic-counter instrumentation at every `pool.get()` call site.

Both become cheap once a typed pool wrapper exists (a single `App`-level helper that wraps `r2d2::Pool` and is the only way connections are obtained); that wrapper is a separate codebase change and is not in scope here. In v1, DB-pool saturation is signalled by `connections.usage` ratios — `connections.usage{state="used"} / connections.max` near 1.0 means saturation. The request-duration histogram tail will spike under DB starvation regardless.

### 8.3 Redis pool saturation (`saturation::redis_pool`)

Compiled out via `cfg` if Redis is not configured. Names mirror the DB pool. Backed by `fred`'s built-in metrics surface.

| Name | Type | Attributes |
|---|---|---|
| `redis.client.connections.usage` | UpDownCounter (observable) | `state="idle"\|"used"`, `pool.name` |
| `redis.client.commands.in_flight` | Gauge (observable) | `pool.name` |
| `redis.client.command.latency` | Histogram (s) | `pool.name`, `command_kind="read"\|"write"\|"admin"` |

Exact mapping from `fred` stats to these metrics is finalized at implementation time; if any field is unavailable, that metric is dropped from v1 with a TODO.

### 8.4 Tokio runtime saturation (`saturation::tokio_runtime`)

Reads `tokio::runtime::Handle::metrics()` directly from each observable callback — no background sampler, no atomics snapshot, no `RuntimeMonitor`, no `tokio-metrics` dep. The APIs used are stable in tokio ≥1.50; the third metric is gated the same way tokio itself gates `worker_total_busy_duration`.

| Name | Type | Attributes | Source |
|---|---|---|---|
| `runtime.tokio.workers` | Gauge (observable, u64) | — | `Handle::metrics().num_workers()` |
| `runtime.tokio.global_queue.depth` | Gauge (observable, u64) | — | `Handle::metrics().global_queue_depth()` |
| `runtime.tokio.workers.busy.time` | Counter (observable, f64, seconds), `cfg(target_has_atomic = "64")` | — | `Σ Handle::metrics().worker_total_busy_duration(i).as_secs_f64()` across workers |

Saturation is computed at query time:

```promql
# Average per-worker busy fraction over 1 minute
sum(rate(runtime_tokio_workers_busy_time_seconds_total[1m]))
  / on() runtime_tokio_workers
```

When not running on a Tokio runtime (e.g., a sync test harness), `register()` early-returns and no instruments are created.

### 8.5 Resource attributes

Set once at `MeterProvider` init, applied to every metric.

| Attribute | Source |
|---|---|
| `service.name` | `OTEL_SERVICE_NAME` env; default `"superposition"` |
| `service.version` | `env!("CARGO_PKG_VERSION")` at build time |
| `service.instance.id` | `SUPERPOSITION_INSTANCE_ID` env; default to hostname |
| `deployment.environment` | existing env detection (`PROD`/`SANDBOX`/`DEV`) |
| `OTEL_RESOURCE_ATTRIBUTES` | merged in if set (standard OTel env var) |

## 9. Middleware mechanics

The middleware itself is upstream (`opentelemetry-instrumentation-actix-web 0.24`); this section describes the project-specific hooks we plug into it.

### 9.1 Route template extraction (`CardinalityBoundedFormatter`)

The crate's `RouteFormatter` trait is invoked once per request to produce the `http.route` attribute. By default it would emit the matched pattern (or `"default"` for unmatched paths) verbatim. We wrap that with three cases:

| Match outcome | `http.route` value |
|---|---|
| Pattern matched | the pattern string |
| `"default"` (unmatched, served by the Leptos catch-all) | `__not_found__` (`ROUTE_NOT_FOUND`) |
| Path starts with `/pkg`, `/assets`, or `/favicon` | `__static__` (`ROUTE_STATIC`) |

Sentinels are constants — finite set, bounded cardinality. `STATIC_PATTERN_PREFIXES` lives in `instrumentation.rs` alongside the formatter.

### 9.2 Label extraction (`metric_attrs_from_req`)

The crate exposes a `with_metric_attrs_from_req(fn(&ServiceRequest, &str) -> Vec<KeyValue>)` hook called per request to produce the final attribute set. Our implementation:

1. Always emits `http.request.method` (normalized — see §9.3) and `http.route` (the formatter's output).
2. Conditionally emits `sp.org_id` and `sp.workspace_id` from request extensions, gated on the process-wide `LabelConfig`.

Because the hook is a function pointer, it can't capture state. The `LabelConfig` lives in a `OnceLock<LabelConfig>` (`GLOBAL_LABEL_CONFIG`) installed once at startup via `set_label_config()`; the hook reads it via a `label_config()` accessor that returns `LabelConfig::default()` if unset.

| Case | Action |
|---|---|
| Extension present | Emit attribute with the value. |
| Extension absent because route does not have one (org-management routes) | Omit the attribute. Series simply lacks that label — distinct from a value of `""`. |
| Extension absent because middleware short-circuited (401, 403) | Omit the attribute. |
| `LabelConfig` has the label disabled | Never read the extension; never emit. |

### 9.3 HTTP method normalization

Per OTel HTTP semconv: known methods (`GET`, `POST`, `PUT`, `DELETE`, `PATCH`, `HEAD`, `OPTIONS`, `TRACE`, `CONNECT`) keep their literal value; anything else collapses to `_OTHER`. A small `match` macro lives in `instrumentation.rs`. Prevents weird clients (`XPROPFIND`, `INVALID-㊙️`) from blowing up cardinality.

### 9.4 Status code source

The upstream crate reads `res.status().as_u16()` after the response is produced and emits it as `http.response.status_code`. Actix's error conversion runs before the middleware's response phase, so error responses get their converted status. Panics that bubble through the Actix panic handler surface as 500s. We don't add an `error.type` attribute — the histogram's `status_code=~"5.."` slice serves the same purpose without extra cardinality.

### 9.5 Active-requests guard

Handled by the upstream crate. It uses the same RAII-on-`Drop` pattern (so panics and client disconnects still decrement), so we get panic-safety without owning the guard.

### 9.6 Endpoints excluded from instrumentation

- `/metrics` — physically isolated on the metrics port; cannot reach the middleware.
- Static asset routes — emit `__static__` for `http.route` instead of being skipped, so a flood is still visible.

### 9.7 Per-request overhead

Per-request work in the upstream middleware path:

- 1 `Instant::now()` capture on entry; one duration calc on exit.
- 1 `UpDownCounter::add(+1)` on entry; 1 `UpDownCounter::add(-1)` on drop.
- 1 `Histogram::record()` call (lock-free in OTel SDK 0.32).
- 1 invocation of `metric_attrs_from_req` (3 hashmap lookups on `req.extensions()` worst case, 1 `OnceLock::get()`).
- The crate's body-size histogram instruments still exist but are dropped at the SDK layer via the `Aggregation::Drop` View — they're cheap allocations that go nowhere, not free, but well under a microsecond.

**Hot-path allocations:** attribute *keys* are `&'static str` literals; attribute *values* (route, org, workspace, status as a string) require `String` allocations because they're dynamic. Unavoidable given the label choices.

Total expected overhead: **single-digit microseconds per request**, well below the millisecond scale of any handler.

## 10. Saturation collector internals

### 10.1 Pull-on-observation pattern

OTel's `ObservableGauge` and `ObservableCounter` invoke a callback at collection time (every scrape, every push interval). For sources that are cheap to read synchronously (`r2d2::Pool::state()`, `fred` stats), no background task is needed:

```rust
let pool_clone = pool.clone();
meter
    .u64_observable_gauge("db.client.connections.usage")
    .with_callback(move |observer| {
        let s = pool_clone.state();
        observer.observe(s.idle_connections as u64,
                         &[KeyValue::new("state", "idle"),
                           KeyValue::new("pool.name", "primary")]);
        observer.observe((s.connections - s.idle_connections) as u64,
                         &[KeyValue::new("state", "used"),
                           KeyValue::new("pool.name", "primary")]);
    })
    .init();
```

### 10.2 No background sampler

The original design carved out a polling exception for `tokio_metrics::RuntimeMonitor`. That's gone: tokio's stable `Handle::metrics()` API returns absolute values that are cheap to read synchronously, so `saturation::tokio_runtime` uses the same callback pattern as the pool collectors. Zero background tasks in the entire observability subsystem.

### 10.3 No build-flag requirement

No `--cfg tokio_unstable`, no `.cargo/config.toml` entry, no `README` / `makefile` callout. Tokio 1.50's stable surface covers everything. The one stable-but-gated API (`worker_total_busy_duration`) is `cfg(target_has_atomic = "64")`-conditional, mirroring tokio's own gating; on targets without 64-bit atomics the `runtime.tokio.workers.busy.time` Counter is simply not registered.

## 11. Configuration surface

All env-driven; no config file. Applies to the main `superposition` binary.

| Var | Default | Purpose |
|---|---|---|
| `SUPERPOSITION_METRICS_ENABLED` | `true` | Master switch. `false` ⇒ no init, no middleware, no listener. |
| `SUPERPOSITION_METRICS_PORT` | `9091` | Port for the `/metrics` listener. |
| `SUPERPOSITION_METRICS_BIND` | `0.0.0.0` | Bind address for the metrics listener. Set to `127.0.0.1` for loopback-only. |
| `SUPERPOSITION_METRICS_LABEL_ORG` | `true` | Include `sp.org_id` attribute on HTTP metrics. |
| `SUPERPOSITION_METRICS_LABEL_WORKSPACE` | `true` | Include `sp.workspace_id` attribute on HTTP metrics. |
| `SUPERPOSITION_METRICS_COLLECT_INTERVAL` | `10s` | OTLP push exporter cadence (`PeriodicReader` interval). Has no effect when OTLP is not configured — the Prometheus exporter is pull-driven. Parsed by `humantime`. |
| `SUPERPOSITION_INSTANCE_ID` | hostname | `service.instance.id` resource attribute. |
| `OTEL_EXPORTER_OTLP_ENDPOINT` | unset | Standard OTel env var. If set, enables OTLP push exporter. |
| `OTEL_EXPORTER_OTLP_PROTOCOL` | `http/protobuf` | Standard OTel env var. |
| `OTEL_EXPORTER_OTLP_HEADERS` | unset | Standard OTel env var. |
| `OTEL_SERVICE_NAME` | `superposition` | Standard OTel env var. |
| `OTEL_RESOURCE_ATTRIBUTES` | unset | Standard OTel env var; merged into resource. |

Env reading uses the existing `service_utils` env-loading idiom, matching what `auth_n` etc. already do.

### 11.1 Cardinality budget (worked)

For the HTTP request-duration histogram, per active workspace × org pair in steady state:

- ~30 routes × ~3 methods used × ~5 status codes seen × 12 series-per-bucket-set = **~5,400 series ceiling**, with realized usage typically 10–20 % → **~540–1,080 actual series per workspace**.

Other metrics (`http.server.active_requests`, saturation gauges) are method-only or unlabeled → ~30 series total, independent of tenant count.

So adding a workspace ≈ 600–1,100 new series. At 1,000 active workspaces ≈ **600 k – 1.1 M series** for the HTTP histogram. Comfortably within `vmsingle` on 16 GB.

If workspace count grows beyond ~5,000 active, set `SUPERPOSITION_METRICS_LABEL_WORKSPACE=false` and slice by workspace via traces instead — no code change required.

## 12. Testing strategy

### 12.1 Unit tests (`crates/service_utils/src/observability/`)

| Test module | What it asserts |
|---|---|
| `config::tests::defaults_when_unset` | All defaults parse correctly when no env vars are set. |
| `config::tests::explicit_overrides` | Each env var, individually and in combination, lands in the expected `ObservabilityConfig` field. |
| `config::tests::malformed_port_errors` | Bad port string surfaces a key-prefixed error. |
| `instrumentation::tests::formatter_passes_through_matched_templates` | `RouteFormatter` returns matched patterns unchanged. |
| `instrumentation::tests::formatter_maps_unmatched_to_not_found_sentinel` | `"default"` → `__not_found__`. |
| `instrumentation::tests::formatter_collapses_static_asset_prefixes` | `/pkg/*`, `/assets/*`, `/favicon*` → `__static__`. |
| `instrumentation::tests::normalize_method_passes_known_through` | Known HTTP verbs pass through. |
| `instrumentation::tests::normalize_method_collapses_unknown` | Anything else → `_OTHER`. |
| `metrics_server::tests::scrape_endpoint_returns_text_plain` | `/metrics` returns 200 with `text/plain; …` content type. |
| `observability::tests::init_builds_meter_and_registry` | `Observability::init` returns a usable `Meter` and `Registry`. |
| `observability::tests::meter_can_record_a_histogram_and_register_it_in_registry` | End-to-end: record on a `Meter`-built histogram → series shows up in the `Registry` exposition. |

### 12.2 Integration tests (`crates/service_utils/tests/observability_integration.rs`)

Three tests, each constructing an explicit `SdkMeterProvider` via `Observability::init` so they don't share OTel global state:

1. **`metrics_appear_after_requests`** — Boot a test app with the `RequestMetrics` middleware and a small `/test` scope. Issue requests of varying methods, paths, and status codes (including 404). Scrape the metrics port; parse the Prometheus exposition. Assert:
   - `http_server_request_duration_seconds_*` series exist for each `(route, method, status)`.
   - The `__not_found__` route appears for the 404.
   - Body-size histograms (`http_server_request_body_size`, `http_server_response_body_size`) are **absent** — proves the SDK View suppression works.

2. **`cardinality_stays_within_budget`** — Same setup, issue ~50 requests across a deliberately diverse set of routes/methods/statuses. Count distinct series in the exposition; assert under a budget (~200 series for the test scenario). Catches accidental high-cardinality labels in code review.

3. **`runtime_tokio_metrics_appear_after_register_observers`** — Boot a test app under `#[tokio::test]`, call `register_observers`, force a collection cycle, and assert that `runtime_tokio_workers`, `runtime_tokio_global_queue_depth`, and `runtime_tokio_workers_busy_time_seconds_total` all appear in the exposition.

The OTLP push path is exercised by `Observability::init` paths in unit tests; a full mock-OTLP receiver integration test is not in v1 (the periodic-reader plumbing is mostly upstream code).

## 13. Rollout

| Phase | Duration | Action | Exit criterion |
|---|---|---|---|
| **1 — code lands disabled** | 1 PR | Land code with `SUPERPOSITION_METRICS_ENABLED=false` as the *deployed* default in prod environments (override on in CI/staging). | Process startup time unchanged; per-request overhead within noise on existing locust suite; `/metrics` exposition parses cleanly in CI. |
| **2 — staging** | 48 h | `SUPERPOSITION_METRICS_ENABLED=true` in staging. | VM ingest rate stable; series count matches §11.1 estimate to within 30 %; no scrape errors. |
| **3 — prod, no workspace label** | 1 week | Prod on, `SUPERPOSITION_METRICS_LABEL_WORKSPACE=false`. | VM headroom > 30 %; alerts (when defined in follow-up PR) firing as expected. |
| **4 — prod, full** | — | `SUPERPOSITION_METRICS_LABEL_WORKSPACE=true`. | Steady-state. |

Existing `info!(latency, "GoldenSignal")` log line at `crates/service_utils/src/middlewares/request_response_logging.rs:84` stays for now. Marked for removal once Phase 4 is steady and dashboards have migrated.

## 14. Future work (not implemented)

- **Trace correlation via exemplars.** When `tracing-opentelemetry` bridges traces into the same SDK, the histogram emits exemplars linking percentile spikes to specific traces. Free win once the bridge exists.
- **Per-tenant separate histogram** (option D from Q3 brainstorm). If the global histogram's cardinality budget proves tight, add a second `http_server_request_duration_by_workspace_seconds` with fewer buckets, retaining tenant slicing without paying the cost on the global histogram.
- **OTLP traces export.** The `Observability::init` shape is structured to host trace export later.
- **Grafana dashboards.** JSON dashboards under `grafana/dashboards/` covering the four golden-signal panels. Separate PR.
- **Alert rules.** VM/Prometheus alert rule YAML covering: error rate > X %, p99 latency > X ms, DB pool usage near max, Tokio per-worker busy fraction (`rate(runtime_tokio_workers_busy_time_seconds_total) / runtime_tokio_workers`) sustained > 0.8. Separate PR.
- **Per-route overhead controls.** A route-level allowlist/denylist in `LabelConfig` so noisy or high-volume internal routes can be sampled or excluded at runtime without redeploying.
- **DB pool wait visibility.** `db.client.connection.wait.duration` (histogram) and `db.client.connections.pending_requests` (gauge), unlocked by a typed pool wrapper that is the only way to obtain a connection. One-time codebase migration, then both metrics fall out for free.
- **Removing the existing `GoldenSignal` log line.** Once dashboards are migrated, the log line in `request_response_logging.rs:84` becomes redundant.

## 15. Risks

| Risk | Mitigation |
|---|---|
| OTel Rust SDK 0.32 has historically had churn between minor versions; metrics API is stable but exporter and Resource APIs shifted between 0.27 and 0.32. | Pin the entire OTel family (api, sdk, prometheus, otlp, instrumentation-actix-web) to compatible versions in `[workspace.dependencies]`; bump in a single PR with the integration test as the gate. |
| `opentelemetry-instrumentation-actix-web 0.24` is a third-party OTel-contrib crate that may evolve independently of upstream OTel. | The crate exposes stable hooks (`RouteFormatter`, `with_metric_attrs_from_req`, `with_meter_provider`) for everything we customize. If it falls behind on an OTel bump, we can either pin OTel until it catches up, or fork the ~300 lines of middleware code. The body-size histograms are suppressed via SDK View, so changes there don't affect us. |
| Workspace label cardinality grows unexpectedly (workspace creation rate, churn from short-lived workspaces). | `SUPERPOSITION_METRICS_LABEL_WORKSPACE=false` is a runtime opt-out; rollout Phase 3 lands with it off. |
| OTel attribute construction allocates `String` on the hot path. | Confirmed unavoidable for dynamic attribute values; expected overhead single-digit microseconds. If profiling shows a problem, switch to `Cow<'static, str>` for attribute *values* where possible (e.g., method, status code) and keep allocations only for `route`/`org`/`workspace`. |
| `r2d2`'s waiter count and wait duration require call-site instrumentation; v1 has only `connections.usage` ratios. | Acceptable for v1: a usage ratio near `connections.max` signals saturation, and the request-duration histogram tail will spike under DB starvation. A typed pool wrapper in a follow-up unlocks both `wait.duration` and `pending_requests` cheaply. |
| `fred` metrics surface may not map 1:1 to OTel `db.client.*` style attributes. | Mapping resolved at implementation time via a `RedisStats` trait — any unavailable field returns `None` and is silently omitted from emission. |
| `LabelConfig` lives in a `OnceLock` because the upstream crate's `metric_attrs_from_req` is a fn pointer with no closure state. Changing label config requires a redeploy. | Acceptable: label config is env-driven and operationally rare to change. If runtime-mutable labels become a requirement, switch to `with_metric_attrs_from_req_and_state(F)` if/when upstream exposes it, or fork the middleware. |
