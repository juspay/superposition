use fred::{
    prelude::{KeysInterface, RedisClient, RedisPool},
    types::Expiration,
};
use serde::{Serialize, de::DeserializeOwned};
use superposition_types::{DBConnection, result as superposition};

use crate::{
    db::{PgSchemaConnectionPool, run_query},
    helpers::get_from_env_or_default,
    service::types::SchemaName,
};

pub const LAST_MODIFIED_KEY_SUFFIX: &str = "::cac_config::last_modified_at";
pub const CONFIG_VERSION_KEY_SUFFIX: &str = "::cac_config::config_version";
pub const CONFIG_KEY_SUFFIX: &str = "::cac_config";
pub const EXPERIMENTS_LIST_KEY_SUFFIX: &str = "::experiments_list";
pub const EXPERIMENTS_LAST_MODIFIED_KEY_SUFFIX: &str = "::experiments::last_modified_at";
pub const EXPERIMENT_GROUPS_LIST_KEY_SUFFIX: &str = "::experiment_groups_list";
pub const EXPERIMENT_GROUPS_LAST_MODIFIED_KEY_SUFFIX: &str =
    "::experiment_groups::last_modified_at";
pub const EXPERIMENT_CONFIG_LAST_MODIFIED_KEY_SUFFIX: &str =
    "::experiment_config::last_modified_at";

/// Fetch data from Redis if available, else fall back to database call and write back to Redis
/// if redis is disabled read from the database directly
/// the fallback function is expected to return Result<T, diesel::error::Error>
/// You can use move closures to capture variables in the run_query
pub async fn read_through_cache<T>(
    key: String,
    schema_name: &SchemaName,
    redis_pool: &Option<RedisPool>,
    db_pool: &PgSchemaConnectionPool,
    fallback_fn: impl FnOnce(&mut DBConnection) -> superposition::DieselResult<T>,
) -> superposition::Result<T>
where
    T: Serialize + DeserializeOwned,
{
    let Some(pool) = redis_pool else {
        log::trace!("Redis pool not configured, using fallback");
        return run_query(db_pool, fallback_fn);
    };

    let client = pool.next_connected();

    if let Ok(data) = get_data_from_redis(key.clone(), client).await {
        return Ok(data);
    }

    log::info!(
        "Cache miss for schema {}, falling back to DB",
        **schema_name,
    );

    let data = run_query(db_pool, fallback_fn)?;

    // Best-effort writeback — don't fail the request if Redis write fails
    if let Ok(serialized) = serde_json::to_string(&data) {
        let key_ttl: i64 = get_from_env_or_default("REDIS_KEY_TTL", 604800);
        let _ = client
            .set::<(), String, String>(
                key,
                serialized,
                Some(Expiration::EX(key_ttl)),
                None,
                false,
            )
            .await
            .map_err(|e| log::error!("Failed to write back to Redis: {e}"));
    } else {
        log::error!("Failed to serialize data for Redis writeback");
    }

    Ok(data)
}

pub async fn get_data_from_redis<T>(
    key_name: String,
    client: &RedisClient,
) -> Result<T, String>
where
    T: DeserializeOwned,
{
    use fred::interfaces::MetricsInterface;

    log::debug!("Started redis fetch for config");
    let data = {
        // this block is so that the client connection is dropped
        // before we move on to parsing the config
        let data = client
            .get::<String, String>(key_name.clone())
            .await
            .map_err(|e| {
                log::error!("Failed to fetch {key_name} from redis: {}", e);
                format!("Failed to fetch {key_name} from redis due to: {}", e)
            })?;
        let metrics = client.take_latency_metrics();
        let network_metrics = client.take_network_latency_metrics();
        log::trace!(
            "Network metrics for data fetch in milliseconds :: max: {}, min: {}, avg: {}; Latency metrics :: max: {}, min: {}, avg: {}",
            network_metrics.max,
            network_metrics.min,
            network_metrics.avg,
            metrics.max,
            metrics.min,
            metrics.avg
        );
        data
    };

    let value = serde_json::from_str::<T>(&data).map_err(|e| {
        log::error!("Failed to parse value from redis: {}", e);
        format!("Failed to parse value from redis due to: {}", e)
    })?;
    Ok(value)
}
