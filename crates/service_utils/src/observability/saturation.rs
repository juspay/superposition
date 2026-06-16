//! Saturation collectors: DB pool, Redis pool, Tokio runtime.
//!
//! All metrics are observable-instrument callbacks — no background tasks.

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
    tokio_runtime::register(meter);

    Ok(())
}
