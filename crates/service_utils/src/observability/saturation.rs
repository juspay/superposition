//! Saturation collectors: DB pool, Redis pool, Tokio runtime.
//!
//! Most metrics are observable-gauge callbacks (no background tasks).
//! Only `tokio_runtime` requires a polling loop.

mod db_pool;
mod redis_pool;
mod tokio_runtime;

use opentelemetry::metrics::Meter;

pub use db_pool::DbPoolHandle;
pub use redis_pool::{FredPoolStats, RedisHandle, RedisStats};

/// Optional dependencies the saturation subsystem can observe.
#[derive(Default, Clone)]
pub struct SaturationDeps {
    pub db_pool: Option<DbPoolHandle>,
    pub redis_client: Option<redis_pool::RedisHandle>,
    pub tokio_collect_interval: std::time::Duration,
}

pub fn register_observers(
    meter: &Meter,
    deps: SaturationDeps,
) -> Result<(), super::ObservabilityError> {
    if let Some(pool) = deps.db_pool {
        db_pool::register(meter, pool, "primary");
    }
    if let Some(client) = deps.redis_client {
        redis_pool::register(meter, client, "primary");
    }

    #[cfg(tokio_unstable)]
    if deps.tokio_collect_interval > std::time::Duration::ZERO {
        tokio_runtime::spawn(meter, deps.tokio_collect_interval);
    }

    Ok(())
}
