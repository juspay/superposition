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
