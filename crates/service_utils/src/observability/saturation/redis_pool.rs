//! Saturation gauges for the Redis client pool (fred crate).
//!
//! fred's `metrics` feature exposes per-client / per-pool stats. The
//! callbacks below are intentionally tolerant: if a stat is unavailable
//! in the version we use, the metric is simply not emitted.
//!
//! ## fred 9.2.1 API notes
//!
//! * `MetricsInterface` is implemented on `RedisClient`, **not** on
//!   `RedisPool`. The pool exposes `.clients() -> &[RedisClient]` so we can
//!   iterate over individual clients.
//! * There is no idle/used connection concept in fred's non-blocking pool:
//!   all connections are async and the pool just round-robins. No public API
//!   exposes a connection-level in-flight counter.
//! * `command_queue_len()` (via `MetricsInterface`) counts buffered commands
//!   waiting to be written to the socket. Summing across pool clients gives a
//!   useful "pending work" gauge.

use std::sync::Arc;

use fred::{interfaces::MetricsInterface, prelude::RedisPool};
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
    fn idle_connections(&self) -> Option<u64>;
    fn used_connections(&self) -> Option<u64>;
    fn commands_in_flight(&self) -> Option<u64>;
}

/// Implements `RedisStats` for the project's fred `RedisPool`.
#[allow(dead_code)]
///
/// ### Fields populated
/// - `commands_in_flight`: sum of `command_queue_len()` across all clients in
///   the pool (commands buffered in memory waiting to be sent).
///
/// ### Fields returning `None`
/// - `idle_connections`: fred's non-blocking async pool has no concept of
///   "idle" connections — all pool slots share the event loop and the pool
///   does not expose per-connection state counters publicly.
///   TODO(observability): expose idle connection count if fred adds a public
///   API for per-connection state (e.g. `ClientState::Disconnected` count).
/// - `used_connections`: same limitation as `idle_connections` above.
///   TODO(observability): expose used/active connection count when fred adds
///   a public API to read per-connection in-flight request counters (currently
///   internal to `protocol::connection::Counters`).
pub struct FredPoolStats(pub RedisPool);

impl RedisStats for FredPoolStats {
    fn idle_connections(&self) -> Option<u64> {
        // TODO(observability): fred 9.2.1 does not expose idle-connection
        // counts via any public API. The pool's `RedisPoolInner.clients` are
        // all async and there is no `is_idle()` method on `RedisClient`.
        // Return None until fred provides such an API.
        None
    }

    fn used_connections(&self) -> Option<u64> {
        // TODO(observability): fred 9.2.1 does not expose active/used
        // connection counts. The internal `Counters.in_flight` field tracks
        // per-connection in-flight commands, but it is private to
        // `fred::protocol::connection`. Return None until exposed publicly.
        None
    }

    fn commands_in_flight(&self) -> Option<u64> {
        // `command_queue_len()` is available via `MetricsInterface` on each
        // `RedisClient`. It counts commands buffered in the client that have
        // not yet been written to the network socket. Summing across all
        // pool clients gives an approximate "pending work" measure.
        let total: usize = self
            .0
            .clients()
            .iter()
            .map(|c| c.command_queue_len())
            .sum();
        Some(total as u64)
    }
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
        .with_description(
            "Number of Redis commands currently buffered (waiting to be sent to the server).",
        )
        .with_callback(move |observer| {
            if let Some(n) = c.commands_in_flight() {
                observer.observe(n, &[label.clone()]);
            }
        })
        .build();
}
