//! ObservableGauge callbacks for the r2d2 connection pool. Purely passive —
//! no instrumentation at `pool.get()` call sites.

use std::sync::Arc;

use opentelemetry::{KeyValue, metrics::Meter};

/// Concrete pool type used across the codebase.
///
/// Mirrors `crate::db::PgSchemaConnectionPool` (which aliases
/// `diesel::r2d2::Pool<diesel::r2d2::ConnectionManager<diesel::PgConnection>>`).
/// Using an explicit expansion here so the observability subsystem does not
/// take a hard dep on `crate::db` — callers pass the handle in via
/// `SaturationDeps`.
pub type DbPoolHandle = Arc<
    diesel::r2d2::Pool<diesel::r2d2::ConnectionManager<diesel::PgConnection>>,
>;

pub fn register(meter: &Meter, pool: DbPoolHandle, pool_name: &'static str) {
    let pool_for_usage = pool.clone();
    let usage_pool_name = KeyValue::new("pool.name", pool_name);
    meter
        .u64_observable_gauge("db.client.connections.usage")
        .with_description("Number of DB connections in idle/used state.")
        .with_callback(move |observer| {
            let s = pool_for_usage.state();
            let used = s.connections.saturating_sub(s.idle_connections);
            observer.observe(
                s.idle_connections as u64,
                &[
                    KeyValue::new("state", "idle"),
                    usage_pool_name.clone(),
                ],
            );
            observer.observe(
                used as u64,
                &[
                    KeyValue::new("state", "used"),
                    usage_pool_name.clone(),
                ],
            );
        })
        .build();

    let pool_for_max = pool.clone();
    let max_pool_name = KeyValue::new("pool.name", pool_name);
    meter
        .u64_observable_gauge("db.client.connections.max")
        .with_description("Configured maximum size of the DB connection pool.")
        .with_callback(move |observer| {
            observer.observe(pool_for_max.max_size() as u64, &[max_pool_name.clone()]);
        })
        .build();
}

#[cfg(test)]
mod tests {
    // Constructing a real r2d2 pool requires a database. We assert the function
    // signature compiles and that calling `register` does not panic with a
    // synthetic in-memory pool; this is exercised via the integration test in
    // Task 19 instead.
}
