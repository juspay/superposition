//! Saturation gauges for the Redis client pool (fred crate).
//!
//! fred's `metrics` feature exposes per-client / per-pool stats. The
//! callbacks below are intentionally tolerant: if a stat is unavailable
//! in the version we use, the metric is simply not emitted.
//!
//! ## fred 9.2.1 API notes
//!
//! * `ClientLike::is_connected()` returns true when the client's underlying
//!   connection to Redis is active. Counting these across the pool gives a
//!   useful "healthy connections" gauge.
//! * `MetricsInterface` is implemented on `RedisClient`, **not** on
//!   `RedisPool`. The pool exposes `.clients() -> &[RedisClient]` so we can
//!   iterate over individual clients.
//! * `command_queue_len()` (via `MetricsInterface`) counts buffered commands
//!   waiting to be written to the socket. Summing across pool clients gives a
//!   useful "pending work" gauge.

use std::sync::Arc;

use fred::{
    interfaces::{ClientLike, MetricsInterface},
    prelude::RedisPool,
};
use opentelemetry::{KeyValue, metrics::Meter};

/// Wraps whatever fred client/pool type the rest of `service_utils` uses.
/// The wrapping type implements `RedisStats` so the observability module
/// does not have to know fred's concrete types.
pub type RedisHandle = Arc<dyn RedisStats + Send + Sync>;

/// Thin abstraction over the fred metrics surface so the saturation module
/// is decoupled from fred's exact API.
///
/// Returning `None` from any getter simply omits the corresponding metric.
/// This is intentional so that not-yet-wired stats do not break the build.
pub trait RedisStats {
    /// Number of clients in the pool with an active connection to Redis.
    fn connected_connections(&self) -> Option<u64>;
    fn commands_in_flight(&self) -> Option<u64>;
}

/// Implements `RedisStats` for the project's fred `RedisPool`.
pub struct FredPoolStats(pub RedisPool);

impl RedisStats for FredPoolStats {
    fn connected_connections(&self) -> Option<u64> {
        Some(self.0.clients().iter().filter(|c| c.is_connected()).count() as u64)
    }

    fn commands_in_flight(&self) -> Option<u64> {
        // `command_queue_len()` is available via `MetricsInterface` on each
        // `RedisClient`. It counts commands buffered in the client that have
        // not yet been written to the network socket. Summing across all
        // pool clients gives an approximate "pending work" measure.
        let total: usize = self.0.clients().iter().map(|c| c.command_queue_len()).sum();
        Some(total as u64)
    }
}

pub fn register(meter: &Meter, client: RedisHandle, pool_name: &'static str) {
    let pool_label = KeyValue::new("pool.name", pool_name);

    let c = client.clone();
    let label = pool_label.clone();
    meter
        .u64_observable_gauge("redis.client.connections.connected")
        .with_description(
            "Number of Redis client connections currently connected to the server.",
        )
        .with_callback(move |observer| {
            if let Some(n) = c.connected_connections() {
                observer.observe(n, std::slice::from_ref(&label));
            }
        })
        .build();

    meter
        .u64_observable_gauge("redis.client.commands.in_flight")
        .with_description(
            "Number of Redis commands currently buffered (waiting to be sent to the server).",
        )
        .with_callback(move |observer| {
            if let Some(n) = client.commands_in_flight() {
                observer.observe(n, std::slice::from_ref(&pool_label));
            }
        })
        .build();
}
