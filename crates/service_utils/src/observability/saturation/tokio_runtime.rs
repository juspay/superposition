//! Tokio runtime saturation, gated on `cfg(tokio_unstable)`.
//!
//! Unlike DB/Redis, `tokio_metrics::RuntimeMonitor` is delta-based: each
//! `.intervals()` call returns stats since the last call. So we run a
//! background task that samples every `interval` and stores derived values
//! in atomics that observable-gauge callbacks read.

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
        busy_ratio_milli: AtomicU64, // busy_ratio * 1000 stored as integer
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
                    // In tokio-metrics 0.3.x the global (injection) queue depth
                    // is exposed as `injection_queue_depth`, not `global_queue_depth`.
                    snap_for_task
                        .global_queue_depth
                        .store(m.injection_queue_depth as u64, Ordering::Relaxed);
                    // Busy fraction = total worker-seconds busy / total
                    // worker-seconds available over the sample interval.
                    // Use m.elapsed (actual time since last sample) rather than
                    // the configured interval so the ratio stays accurate when
                    // the sampler tick runs late under load.
                    let workers_count = m.workers_count.max(1) as f64;
                    let elapsed = m.elapsed.as_secs_f64();
                    let busy = m.total_busy_duration.as_secs_f64();
                    let total = elapsed * workers_count;
                    let ratio = if total > 0.0 {
                        (busy / total).clamp(0.0, 1.0)
                    } else {
                        0.0
                    };
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
