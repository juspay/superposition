//! Tokio runtime saturation gauges.
//!
//! Reads `tokio::runtime::Handle::metrics()` directly from each observable
//! callback — no background sampler, no atomics snapshot, no `RuntimeMonitor`.
//! Worker count and global queue depth are stable instantaneous values; total
//! busy time is exposed as a monotonic Counter (per-worker durations summed
//! and reported in seconds), letting Prometheus compute the rate / saturation
//! ratio at query time.
//!
//! No-op when not running on a Tokio runtime.

use opentelemetry::metrics::Meter;

pub fn register(meter: &Meter) {
    let handle = match tokio::runtime::Handle::try_current() {
        Ok(h) => h,
        Err(_) => return,
    };

    let h = handle.clone();
    meter
        .u64_observable_gauge("runtime.tokio.workers")
        .with_description("Number of tokio worker threads.")
        .with_callback(move |observer| {
            observer.observe(h.metrics().num_workers() as u64, &[]);
        })
        .build();

    let h = handle.clone();
    meter
        .u64_observable_gauge("runtime.tokio.global_queue.depth")
        .with_description("Tasks queued in the runtime's global injection queue.")
        .with_callback(move |observer| {
            observer.observe(h.metrics().global_queue_depth() as u64, &[]);
        })
        .build();

    // `worker_total_busy_duration` requires 64-bit atomics; gate the
    // instrument the same way tokio gates the method.
    #[cfg(target_has_atomic = "64")]
    {
        let h = handle;
        meter
            .f64_observable_counter("runtime.tokio.workers.busy.time")
            .with_unit("s")
            .with_description(
                "Cumulative time tokio worker threads have spent busy, summed across workers.",
            )
            .with_callback(move |observer| {
                let m = h.metrics();
                let total_secs: f64 = (0..m.num_workers())
                    .map(|i| m.worker_total_busy_duration(i).as_secs_f64())
                    .sum();
                observer.observe(total_secs, &[]);
            })
            .build();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::observability::{LabelConfig, Observability, ObservabilityConfig};

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

    #[actix_web::test]
    async fn registers_and_emits_under_tokio() {
        let obs = Observability::init(test_cfg()).unwrap();
        register(&obs.meter());

        let mut buf = Vec::new();
        let encoder = prometheus::TextEncoder::new();
        prometheus::Encoder::encode(&encoder, &obs.registry().gather(), &mut buf).unwrap();
        let text = String::from_utf8(buf).unwrap();

        assert!(
            text.contains("runtime_tokio_workers"),
            "expected workers gauge, got:\n{text}"
        );
        assert!(
            text.contains("runtime_tokio_global_queue_depth"),
            "expected global_queue_depth gauge, got:\n{text}"
        );
        // busy.time counter is gated on 64-bit atomics, which is always true
        // on the platforms we build for.
        assert!(
            text.contains("runtime_tokio_workers_busy_time"),
            "expected busy.time counter, got:\n{text}"
        );
    }

    #[test]
    fn register_outside_runtime_is_noop() {
        let obs = Observability::init(test_cfg()).unwrap();
        // No tokio runtime in scope -> bail silently.
        register(&obs.meter());
    }
}
