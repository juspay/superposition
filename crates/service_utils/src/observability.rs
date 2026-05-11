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
pub use meters::HttpMeters;
pub use metrics_server::spawn_metrics_server;
pub use middleware::MetricsMiddleware;
pub use saturation::{
    DbPoolHandle, FredPoolStats, RedisHandle, RedisStats, register_observers, SaturationDeps,
};

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
    #[error("meter provider shutdown failed: {0}")]
    Shutdown(String),
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
            .map_err(|e| ObservabilityError::Shutdown(e.to_string()))
    }

    pub fn init(cfg: ObservabilityConfig) -> Result<Self, ObservabilityError> {
        use opentelemetry::KeyValue;
        use opentelemetry_sdk::Resource;
        use opentelemetry_sdk::metrics::SdkMeterProvider;

        let registry = Arc::new(prometheus::Registry::new());

        let exporter = opentelemetry_prometheus::exporter()
            .with_registry((*registry).clone())
            .without_scope_info()
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

        // §8.5 — merge OTEL_RESOURCE_ATTRIBUTES ("k1=v1,k2=v2,...") if set.
        if let Ok(extra) = std::env::var("OTEL_RESOURCE_ATTRIBUTES") {
            for pair in extra.split(',') {
                if let Some((k, v)) = pair.split_once('=') {
                    let k = k.trim().to_owned();
                    let v = v.trim().to_owned();
                    if !k.is_empty() {
                        resource_attrs.push(KeyValue::new(k, v));
                    }
                }
            }
        }

        let resource = Resource::new(resource_attrs);

        let mut builder = SdkMeterProvider::builder()
            .with_reader(exporter)
            .with_resource(resource.clone());

        if let Some(endpoint) = &cfg.otlp_endpoint {
            match with_otlp_reader(builder, endpoint, cfg.collect_interval) {
                Ok(b) => builder = b,
                Err(e) => {
                    tracing::warn!(
                        error = %e,
                        endpoint = %endpoint,
                        "OTLP exporter init failed; metrics will be exposed via /metrics only",
                    );
                    // Rebuild Prom-only builder (base was consumed by with_otlp_reader).
                    let prom_exporter = opentelemetry_prometheus::exporter()
                        .with_registry((*registry).clone())
                        .without_scope_info()
                        .build()
                        .map_err(|e| ObservabilityError::PrometheusInit(e.to_string()))?;
                    builder = SdkMeterProvider::builder()
                        .with_reader(prom_exporter)
                        .with_resource(resource);
                }
            }
        }

        let provider = builder.build();
        opentelemetry::global::set_meter_provider(provider.clone());
        let meter = {
            use opentelemetry::metrics::MeterProvider as _;
            provider.meter("superposition")
        };

        Ok(Self { provider, registry, meter })
    }
}

#[cfg(not(test))]
fn with_otlp_reader(
    builder: opentelemetry_sdk::metrics::MeterProviderBuilder,
    endpoint: &str,
    interval: std::time::Duration,
) -> Result<opentelemetry_sdk::metrics::MeterProviderBuilder, ObservabilityError> {
    // Warn if the operator requested a protocol we do not support (gRPC).
    // This binary is compiled with `http-proto` only; `grpc` silently falls
    // back to HTTP, which can mask misconfiguration.
    if let Ok(protocol) = std::env::var("OTEL_EXPORTER_OTLP_PROTOCOL") {
        if !protocol.is_empty() && protocol != "http/protobuf" {
            tracing::warn!(
                requested_protocol = %protocol,
                "OTEL_EXPORTER_OTLP_PROTOCOL set to '{}'; only 'http/protobuf' is supported in v1, using HTTP",
                protocol
            );
        }
    }

    // Headers: the opentelemetry-otlp 0.27 HTTP exporter reads
    // `OTEL_EXPORTER_OTLP_HEADERS` (and `OTEL_EXPORTER_OTLP_METRICS_HEADERS`)
    // automatically during `build()` — no explicit wiring needed here.
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
        let families = registry.gather();
        assert_eq!(families.len(), 1, "only target_info should be present before any instrument records");
        assert_eq!(families[0].get_name(), "target_info");
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
