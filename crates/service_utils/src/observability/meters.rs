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
