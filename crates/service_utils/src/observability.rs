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
