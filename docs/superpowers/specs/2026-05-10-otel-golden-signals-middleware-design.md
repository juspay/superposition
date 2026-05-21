# OpenTelemetry Golden-Signals Middleware

- **Date:** 2026-05-10
- **Status:** Design — shipped with deviations (see §0)
- **Owner:** Natarajan Kannan
- **Target crate:** `service_utils`
- **Reference TSDB:** VictoriaMetrics (single-node `vmsingle`); design is TSDB-agnostic

## 0. Post-implementation deviations

The PR shipped with the following changes versus the design captured below. The body of this document is preserved as the original design rationale.

- **Health endpoints (`/healthz`, `/livez`, `/readyz`) dropped.** The pre-existing `GET /health` already serves the up-check role; the k8s-conventional liveness/readiness split can be added in a follow-up PR when an actual deployment consumes it. This makes §5.2 ("Auth bypass for health endpoints"), the `health_endpoints()` API in §6, and Task 12/17 of the plan obsolete.
- **`tokio_unstable` flag, `tokio-metrics` dep, and `.cargo/config.toml` removed.** Tokio 1.50 exposes `Handle::metrics().num_workers()`, `.global_queue_depth()`, and `.worker_total_busy_duration(i)` as stable APIs (the last gated on `target_has_atomic = "64"`, like tokio itself does). `saturation::tokio_runtime` reads `Handle::metrics()` directly inside each observable callback — no background sampler, no `RuntimeMonitor`, no atomics snapshot.
- **`runtime.tokio.workers.busy_ratio` replaced with `runtime.tokio.workers.busy.time`.** Exposes cumulative busy time in seconds as a monotonic OTel Counter (summed across workers); Prometheus computes saturation via `rate(...) / num_workers` at query time. Same semantic, Prom-idiomatic.
- **`opentelemetry-semantic-conventions` dependency removed.** The handful of attribute names we use are inlined as string literals.
- **`SaturationDeps::tokio_collect_interval` field removed.** No background sampler → no interval to configure. `SUPERPOSITION_METRICS_COLLECT_INTERVAL` still controls the OTLP periodic-reader cadence.
- **`tenant_middleware_exclusion_list` reverted to env-only.** With health endpoints removed, there's no need to extend it programmatically.
- **HTTP middleware swapped to `opentelemetry-instrumentation-actix-web`.** The PR originally shipped a hand-written `MetricsMiddleware` + `HttpMeters` (~470 lines of custom code emitting `http.server.request.duration`, `http.server.busy.duration`, and `http.server.active_requests`). It has been replaced with the upstream OTel-contrib crate's `RequestMetrics` middleware, configured with a custom `RouteFormatter` (preserves the `__not_found__` / `__static__` cardinality sentinels) and a custom `metric_attrs_from_req` (normalizes HTTP method to `_OTHER` for unknown verbs; adds optional `sp.org_id` / `sp.workspace_id` per a process-wide `LabelConfig`). Required bumping the OTel family to `0.32` workspace-wide and `prometheus` to `0.14`. The body-size histograms the crate emits unconditionally are suppressed via an SDK View (`Aggregation::Drop`). The `http.server.busy.duration` Counter is gone — equivalent saturation information is recoverable from `rate(http_server_request_duration_seconds_sum[...])` since both increment by `elapsed` at the same point.

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
| TSDB (reference) | VictoriaMetrics (`vmsingle`) | Cheap to operate; Prom-compatible; cluster path exists if needed. |
| Exposition transport | Prometheus scrape on dedicated port + optional OTLP push | Pull-by-default for self-hosted users; OTLP path unlocks every OTel-native backend. |
| Labels on HTTP metrics | `route × method × status × org × workspace` | Tenant-level slicing in metrics; workspace label is env-disable-able for users with very high workspace counts. |
| Saturation signals | HTTP active requests + HTTP busy duration + Tokio runtime + DB pool + Redis pool | Multiple independent signals avoid single-metric blind spots. Host-level (CPU/mem/FD) stays with `node-exporter`. |
| Where `/metrics` lives | Separate listener on `SUPERPOSITION_METRICS_PORT` (default `9091`) | Network-policy isolation; scrape requests don't pollute the app's own metrics; no auth interaction. |
| Where `/healthz` lives | Main app port `8080`, paths added to `auth_n`'s exception set | Probes exercise the real user-facing port. |
| Module location | `crates/service_utils/src/observability.rs` (+ `observability/` for submodules, no `mod.rs`) | Matches existing convention for cross-cutting concerns; modern Rust 2018+ module layout. |
| Build config | `.cargo/config.toml` adds `--cfg tokio_unstable` workspace-wide | Required by `tokio-metrics` runtime instrumentation. |

## 5. Architecture

A new module **`service_utils::observability`** owns three pieces:

1. **`init()`** — called once from `main.rs` early in startup. Builds the OTel `MeterProvider` with two readers: a `PrometheusExporter` (renders to `/metrics`) and (if `OTEL_EXPORTER_OTLP_ENDPOINT` is set) a periodic OTLP push exporter. Returns an `Observability` handle owning the registry, a cloned `Meter`, and shutdown hooks.

2. **`MetricsMiddleware`** — Actix `Transform`/`Service` pair wrapping every request on the main server. Records:
   - `http.server.request.duration` (histogram, seconds)
   - `http.server.busy.duration` (counter, seconds)
   - `http.server.active_requests` (UpDownCounter)

3. **`saturation::*`** — observable-gauge callbacks (no background tasks for r2d2 / fred) plus one `tokio::spawn` for `tokio-metrics` runtime polling. All emit OTel-namespaced metrics.

A second component, **`metrics_server`**, is a separate `actix_web::HttpServer` on `SUPERPOSITION_METRICS_PORT` that exposes:

- `GET /metrics` — Prometheus exposition rendered from the OTel registry

The main server (port `8080`) gets one new `.wrap(MetricsMiddleware::new(meter.clone()))` line and three new route registrations for `/healthz`, `/livez`, `/readyz`.

### 5.1 Data flow

```text
[request on :8080]
   ├── tracing-actix-web ─→ span
   ├── (auth_n / auth_z) ─→ extensions: org_id, workspace_id
   ├── MetricsMiddleware ─→ start timer, inc active_requests (RAII guard)
   │     └── handler runs
   └── MetricsMiddleware ─→ record histogram, add busy_duration, dec active_requests, emit attributes

[scrape on :9091/metrics] ←── PrometheusExporter ←── MeterProvider ←── (HTTP middleware + saturation collectors)
                                                                  └─→ (optional) OTLP HTTP/gRPC push to OTEL_EXPORTER_OTLP_ENDPOINT

[saturation, callback-driven]
   ObservableGauge.with_callback(|obs| obs.observe(pool.state(), …))   // r2d2, fred
   ObservableGauge reads from AtomicU64 written by a 10s tokio::spawn  // tokio-metrics
```

### 5.2 Middleware ordering (critical)

`MetricsMiddleware` must run *outside* `auth_n` / `auth_z` / `OrgWorkspaceMiddlewareFactory` so that, when emitting metrics in the response phase, it can read `org_id` / `workspace_id` from request extensions. In Actix, the last `.wrap()` runs first on requests, so the registration chain in `main.rs` should look like (matching the existing convention noted at lines 204–219 of `main.rs`):

```rust
App::new()
    .service(/* main api scopes */)
    .service(health_endpoints())                  // /healthz /livez /readyz
    // Auth innermost so outer middlewares still run on auth failures.
    .wrap(auth_z.clone())
    .wrap(auth_n.clone())
    .wrap(/* DefaultHeaders, Compress as today */)
    .wrap(RequestResponseLogger)
    .wrap(MetricsMiddleware::new(meter.clone(), label_cfg))   // observability — outermost wrap
    .wrap(TracingLogger::<CustomRootSpanBuilder>::new())      // outermost: span covers everything
```

**Auth bypass for health endpoints.** `auth_n` (`crates/service_utils/src/middlewares/auth_n.rs:44–60`) returns `Login::None` when the matched path is in its exception set. The existing `/health` route uses this mechanism. The new `/healthz`, `/livez`, `/readyz` paths are added to the same exception set construction site (the call site that builds the `HashSet<String>` passed into `auth_n`). With the exception in place, requests to health endpoints traverse all the middlewares above (so `MetricsMiddleware` does observe them — desirable) but `auth_n` short-circuits authentication and `auth_z` follows suit.

## 6. Module structure

All new code under `crates/service_utils`:

```text
crates/service_utils/src/
  observability.rs              -- pub use surface: init(), Observability, shutdown(), errors
  observability/
    config.rs                   -- ObservabilityConfig parsed from env
    meters.rs                   -- typed handles: HttpMeters, DbMeters, RedisMeters, RuntimeMeters
    middleware.rs               -- MetricsMiddleware (Transform + Service + InFlightGuard)
    metrics_server.rs           -- HttpServer on SUPERPOSITION_METRICS_PORT exposing /metrics
    health.rs                   -- /healthz /livez /readyz handlers
    saturation.rs               -- spawn entry: register_saturation_observers(...)
    saturation/
      db_pool.rs                -- r2d2 ObservableGauge callbacks
      redis_pool.rs             -- fred ObservableGauge callbacks (cfg-gated on Redis configured)
      tokio_runtime.rs          -- cfg(tokio_unstable); 10s poll task + AtomicU64 → ObservableGauge
```

Files use the modern Rust 2018+ module layout (no `mod.rs`). `crates/service_utils/src/middlewares/` is left untouched and continues to use whatever pattern it currently uses.

### 6.1 Public API sketch

```rust
// observability.rs
pub struct Observability { /* meter_provider, registry, otlp_pipeline, shutdown_handles */ }

impl Observability {
    pub fn init(cfg: ObservabilityConfig) -> Result<Self, ObservabilityError>;
    pub fn meter(&self) -> opentelemetry::metrics::Meter;
    pub fn registry(&self) -> std::sync::Arc<prometheus::Registry>;
    pub fn shutdown(self) -> Result<(), ObservabilityError>;
}

pub fn metrics_middleware(meter: Meter, cfg: LabelConfig) -> middleware::MetricsMiddleware;

pub fn spawn_metrics_server(
    registry: std::sync::Arc<prometheus::Registry>,
    bind: std::net::SocketAddr,
) -> std::io::Result<actix_web::dev::Server>;

pub fn health_endpoints() -> actix_web::Scope;
pub fn health_endpoint_paths() -> &'static [&'static str];  // for auth_n exception set

pub mod saturation {
    pub fn register_observers(
        meter: &Meter,
        deps: SaturationDeps,
    ) -> Result<(), ObservabilityError>;
}

pub struct SaturationDeps {
    pub db_pool: Option<DbPoolHandle>,
    pub redis_client: Option<FredClientHandle>,
    pub tokio_collect_interval: std::time::Duration,
}
```

## 7. Dependencies

Added to root `Cargo.toml` `[workspace.dependencies]` and enabled in `crates/service_utils/Cargo.toml`. Versions pinned to whatever is current and compatible at implementation time; the table below is the intent.

| Crate | Approx version | Purpose |
|---|---|---|
| `opentelemetry` | 0.27 | API surface: `Meter`, `Counter`, `Histogram`, `UpDownCounter`, `ObservableGauge` |
| `opentelemetry_sdk` | 0.27 | SDK: `MeterProvider`, periodic readers, resource detection |
| `opentelemetry-prometheus` | 0.27 | Bridge OTel → `prometheus::Registry` for scrape exposition |
| `opentelemetry-otlp` | 0.27 | Optional OTLP HTTP/gRPC push exporter |
| `opentelemetry-semantic-conventions` | 0.27 | String constants for attributes (`HTTP_ROUTE`, etc.) |
| `prometheus` | 0.13 | Required by `opentelemetry-prometheus` for `Registry` and `TextEncoder` |
| `tokio-metrics` | 0.3 | Runtime metrics; gated by `cfg(tokio_unstable)` |

`fred` already has its `metrics` feature available; we will enable it in `service_utils/Cargo.toml` at implementation time.

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

Two metrics, each capturing a different aspect:

| Field | Value |
|---|---|
| Name | `http.server.busy.duration` |
| Type | Counter (f64, seconds) |
| Unit | `s` |
| Attributes | `http.request.method` |
| Semantics | On each completed request, add elapsed seconds. `rate(...)` over a window gives **time-averaged request concurrency** (Little's Law). Insensitive to scrape aliasing. |

| Field | Value |
|---|---|
| Name | `http.server.active_requests` |
| Type | UpDownCounter |
| Attributes | `http.request.method` |
| Semantics | OTel semconv standard. Instantaneous value at scrape time. **Note:** for sub-100ms services this metric aliases badly; not the primary saturation signal. Kept for semconv compliance and dashboards that expect it. |

`http.server.busy.duration` is the smooth, alert-safe saturation signal. `rate(http_server_busy_duration_seconds_total[1m])` is the average request concurrency over the last minute and can exceed worker count for I/O-bound work — that is expected, not a bug, because Tokio workers are not 1:1 with requests.

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

`#[cfg(tokio_unstable)]`-gated. Backed by `tokio_metrics::RuntimeMonitor`, polled every `SUPERPOSITION_METRICS_COLLECT_INTERVAL` (default 10 s) into `AtomicU64`s read by observable-gauge callbacks.

| Name | Type | Attributes | Source |
|---|---|---|---|
| `runtime.tokio.workers` | Gauge (observable) | — | `num_workers` |
| `runtime.tokio.workers.busy_ratio` | Gauge (observable, f64) | — | `total_busy_duration / total_polls / interval` |
| `runtime.tokio.global_queue.depth` | Gauge (observable) | — | `global_queue_depth` |
| `runtime.tokio.tasks.alive` | Gauge (observable) | — | `live_tasks_count` if available; otherwise dropped |

If a contributor builds without `--cfg tokio_unstable`, the module compiles to a no-op stub; everything else still works.

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

### 9.1 Route template extraction

Actix exposes `req.match_pattern() -> Option<String>` returning the registered template (e.g., `/contexts/{context_id}`), not the raw URI. Three cases for `http.route`:

| Match outcome | `http.route` value |
|---|---|
| Pattern matched | the pattern string |
| No route matched (404 from no match) | `__not_found__` |
| Static asset / Leptos frontend route | `__static__` |

Sentinels are constants — finite set, bounded cardinality.

`match_pattern()` is only populated after routing resolves. The middleware reads it in the response phase. The active-requests increment on entry uses `http.request.method` only, which is available immediately, so no ordering issue.

### 9.2 Label extraction

Read from request extensions during the response phase, set upstream by `OrgWorkspaceMiddlewareFactory`:

```rust
let org_id = req.extensions().get::<OrgId>().map(|o| o.as_str().to_owned());
let workspace = req.extensions().get::<WorkspaceName>().map(|w| w.as_str().to_owned());
```

For each:

| Case | Action |
|---|---|
| Present | Emit attribute with the value. |
| Absent because route does not have one (e.g., org-management routes) | Omit the attribute. Series simply lacks that label — distinct from a value of `""`. |
| Absent because middleware short-circuited before setting it (401, 403) | Omit the attribute. |
| `LabelConfig` has the label disabled | Never emit, regardless of presence. |

### 9.3 HTTP method normalization

Per OTel HTTP semconv: known methods (`GET`, `POST`, `PUT`, `DELETE`, `PATCH`, `HEAD`, `OPTIONS`, `TRACE`, `CONNECT`) keep their literal value; anything else collapses to `_OTHER`. Implemented as a small match — no library dependency. Prevents weird clients (`XPROPFIND`, `INVALID-㊙️`) from blowing up cardinality.

### 9.4 Status code source

| Outcome | Status used |
|---|---|
| Normal response | `res.status().as_u16()` |
| Handler error converted by Actix | the converted response's status |
| Panic (caught by Actix's panic handler → 500) | `500`, with `error.type="panic"` set on the histogram observation only |

### 9.5 Active-requests guard (panic-safe)

```rust
struct InFlightGuard {
    counter: UpDownCounter<i64>,
    method_attr: KeyValue,
    decremented: AtomicBool,
}

impl Drop for InFlightGuard {
    fn drop(&mut self) {
        if !self.decremented.swap(true, Ordering::Relaxed) {
            self.counter.add(-1, &[self.method_attr.clone()]);
        }
    }
}
```

On entry: increment, build guard, store in the request future. On normal completion: explicitly decrement (sets the flag). On client disconnect / future drop / panic upstream: `Drop` decrements as a fallback. The histogram is recorded only on normal completion — a half-finished request's latency is not meaningful.

### 9.6 Endpoints excluded from instrumentation

Hard-coded in v1 (configurable later):

- `/metrics` — physically isolated on the metrics port; cannot reach the middleware.
- Static asset routes — emit `__static__` for `http.route` instead of being skipped, so a flood is still visible.
- `/healthz` `/livez` `/readyz` — instrumented (we want to observe them); auth bypass via `auth_n`'s existing path exception set. Their own latency contributes to `http.server.request.duration` under their own routes.

### 9.7 Per-request overhead

Expected:

- ~3 hashmap lookups on `req.extensions()`
- 2 system clock reads (`Instant::now()` on entry/exit)
- 1 atomic increment + 1 atomic decrement on the active-requests gauge
- 1 histogram `record()` call (lock-free in OTel SDK 0.27+)
- 1 counter `add()` call for `http.server.busy.duration`

**Hot-path allocations:** attribute *keys* are interned via `opentelemetry::Key::from_static_str`; attribute *values* (route, org, workspace) require `String` allocations because they are dynamic. This is unavoidable given Q3's label choices and is intrinsic to OTel attribute construction.

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

### 10.2 Tokio-metrics polling exception

`tokio_metrics::RuntimeMonitor::intervals()` is a delta iterator — it returns stats since the last call, not absolute values. This requires one `tokio::spawn` polling at `SUPERPOSITION_METRICS_COLLECT_INTERVAL` (default 10 s). The task writes derived values into `AtomicU64`s; observable-gauge callbacks read those atomics. Single background task in the whole observability subsystem.

### 10.3 Build configuration

Workspace `.cargo/config.toml`:

```toml
[build]
rustflags = ["--cfg", "tokio_unstable"]
```

Without the flag, the `saturation::tokio_runtime` module compiles to a no-op stub; everything else still works. `README.md` and `makefile` get a one-line callout. `tokio_unstable` only enables additional Tokio APIs — no behavioural change for existing code.

CI runs `cargo check` both with and without the flag to keep the no-op stub honest.

## 11. Configuration surface

All env-driven; no config file. Applies to the main `superposition` binary.

| Var | Default | Purpose |
|---|---|---|
| `SUPERPOSITION_METRICS_ENABLED` | `true` | Master switch. `false` ⇒ no init, no middleware, no listener. |
| `SUPERPOSITION_METRICS_PORT` | `9091` | Port for the `/metrics` listener. |
| `SUPERPOSITION_METRICS_BIND` | `0.0.0.0` | Bind address for the metrics listener. Set to `127.0.0.1` for loopback-only. |
| `SUPERPOSITION_METRICS_LABEL_ORG` | `true` | Include `sp.org_id` attribute on HTTP metrics. |
| `SUPERPOSITION_METRICS_LABEL_WORKSPACE` | `true` | Include `sp.workspace_id` attribute on HTTP metrics. |
| `SUPERPOSITION_METRICS_COLLECT_INTERVAL` | `10s` | Tokio runtime metrics poll interval (only used if `tokio_unstable`). Parsed by `humantime`. |
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

Other metrics (active_requests, busy_duration, saturation gauges) are method-only or unlabeled → ~30 series total, independent of tenant count.

So adding a workspace ≈ 600–1,100 new series. At 1,000 active workspaces ≈ **600 k – 1.1 M series** for the HTTP histogram. Comfortably within `vmsingle` on 16 GB.

If workspace count grows beyond ~5,000 active, set `SUPERPOSITION_METRICS_LABEL_WORKSPACE=false` and slice by workspace via traces instead — no code change required.

## 12. Testing strategy

### 12.1 Unit tests (`crates/service_utils/src/observability/`)

| Test module | What it asserts |
|---|---|
| `middleware::tests::label_extraction` | Table-driven: request fixtures with various extension states → expected `Vec<KeyValue>` produced. |
| `middleware::tests::method_normalization` | `XPROPFIND` → `_OTHER`; known methods pass through. |
| `middleware::tests::route_template_sentinels` | Unmatched path → `__not_found__`; static path → `__static__`. |
| `middleware::tests::active_requests_panic_safety` | Handler that panics still decrements the gauge via `Drop`. |
| `middleware::tests::label_config_disabled` | With `with_workspace_label=false`, attribute is not emitted even when present in extensions. |
| `config::tests::env_parsing` | Env-var combinations produce expected `ObservabilityConfig`. |

### 12.2 Integration test (`crates/service_utils/tests/observability_integration.rs`)

1. Boot a test app: `MetricsMiddleware` + a small `/test` scope with several routes + the metrics server on a random port.
2. Issue requests of varying methods, paths, status codes (including 404 to a non-route).
3. Scrape the metrics port; parse the Prometheus exposition with the `prometheus-parse` crate (or equivalent).
4. Assert:
   - All expected metric names exist.
   - `http_server_request_duration_seconds_count` per `(route, method, status)` matches the issued count.
   - `__not_found__` route appears for the 404.
   - `http_server_active_requests` returns to 0 after all requests complete.
   - `http_server_busy_duration_seconds_total` is approximately `Σ request_duration` (within 10 %).
5. Smoke-test the OTLP pipeline against a mock OTLP receiver if cheap; otherwise gate behind `#[ignore]` and document.

### 12.3 Cardinality regression test

After §12.2 scenarios run, count distinct series in the exposition. Fail the test if total exceeds a budget (initial: 200 series for the test scenario). Catches accidental high-cardinality labels in code review.

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
- **Alert rules.** VM/Prometheus alert rule YAML covering: error rate > X %, p99 latency > X ms, DB pool wait p99 > X ms, Tokio busy-ratio sustained > 0.8. Separate PR.
- **Per-route overhead controls.** A route-level allowlist/denylist in `LabelConfig` so noisy or high-volume internal routes can be sampled or excluded at runtime without redeploying.
- **DB pool wait visibility.** `db.client.connection.wait.duration` (histogram) and `db.client.connections.pending_requests` (gauge), unlocked by a typed pool wrapper that is the only way to obtain a connection. One-time codebase migration, then both metrics fall out for free.
- **Removing the existing `GoldenSignal` log line.** Once dashboards are migrated, the log line in `request_response_logging.rs:84` becomes redundant.

## 15. Risks

| Risk | Mitigation |
|---|---|
| OTel Rust SDK 0.27 has historically had churn between minor versions; metrics API was stabilized but exporter integrations may shift. | Pin to a single minor version; central import via `service_utils::observability`; bump in a single PR with the integration test as the gate. |
| `tokio_unstable` workspace flag affects all crates and may interact with future Tokio releases. | CI matrix runs `cargo check` with and without the flag. The `saturation::tokio_runtime` module is the only consumer; everything else compiles either way. |
| Workspace label cardinality grows unexpectedly (workspace creation rate, churn from short-lived workspaces). | `SUPERPOSITION_METRICS_LABEL_WORKSPACE=false` is a runtime opt-out; rollout Phase 3 lands with it off. |
| OTel attribute construction allocates `String` on the hot path. | Confirmed unavoidable for dynamic attribute values; benchmarked overhead expected single-digit microseconds. If profiling shows a problem, switch to `Cow<'static, str>` for attribute *values* where possible (e.g., method, status code) and keep allocations only for `route`/`org`/`workspace`. |
| `r2d2`'s waiter count and wait duration require call-site instrumentation; v1 has only `connections.usage` ratios. | Acceptable for v1: a usage ratio near `connections.max` signals saturation, and the request-duration histogram tail will spike under DB starvation. A typed pool wrapper in a follow-up unlocks both `wait.duration` and `pending_requests` cheaply. |
| `fred` metrics surface may not map 1:1 to OTel `db.client.*` style attributes. | Mapping is finalized at implementation time; any unavailable field is dropped from v1 with a TODO and noted in the PR description. |
| Health-check probes on the main port get instrumented and add noise to `http_server_request_duration_seconds`. | Acceptable: probe cardinality is fixed (3 routes × 1 method × 1 status), and observing probe latency is desirable. |
