use fred::{
    prelude::{KeysInterface, RedisClient, RedisPool},
    types::Expiration,
};
use serde::{Serialize, de::DeserializeOwned};
use superposition_types::result as superposition;

use crate::{
    db::PgSchemaConnectionPool, helpers::get_from_env_or_default,
    service::types::SchemaName,
};

pub const LAST_MODIFIED_KEY_SUFFIX: &str = "::cac_config::last_modified_at";
pub const AUDIT_ID_KEY_SUFFIX: &str = "::cac_config::audit_id";
pub const CONFIG_VERSION_KEY_SUFFIX: &str = "::cac_config::config_version";
pub const CONFIG_KEY_SUFFIX: &str = "::cac_config";
pub const EXPERIMENTS_LIST_KEY_SUFFIX: &str = "::experiments_list";
pub const EXPERIMENTS_LAST_MODIFIED_KEY_SUFFIX: &str = "::experiments::last_modified_at";
pub const EXPERIMENT_GROUPS_LIST_KEY_SUFFIX: &str = "::experiment_groups_list";

/// Fetch data from Redis if available, else fall back to database call and write back to Redis
/// if redis is disabled read from the database directly
/// the fallback function is expected to return Result<T, String>
/// You can use move closures to capture variables in the database_call
pub async fn fetch_from_redis_else_writeback<T>(
    key: String,
    schema_name: &SchemaName,
    redis_pool: Option<RedisPool>,
    db_pool: PgSchemaConnectionPool,
    database_call: impl FnOnce(PgSchemaConnectionPool) -> superposition::Result<T>,
) -> superposition::Result<T>
where
    T: Serialize + DeserializeOwned,
{
    let Some(pool) = redis_pool else {
        log::trace!("Redis pool not configured, using fallback");
        return database_call(db_pool);
    };
    let client = pool.next_connected();
    match get_data_from_redis(key.clone(), client).await {
        Ok(data) => Ok(data),
        Err(e) => {
            log::info!(
                "Falling back to DB for schema {} due to Redis error: {}",
                **schema_name,
                e
            );
            let data = database_call(db_pool);
            if let Ok(ref value) = data {
                // If the write to redis fails, do not fail the whole request, just pass the data along
                if let Ok(serialized) = serde_json::to_string(value).map_err(|e| {
                    log::error!("Failed to serialize data for redis writeback: {}", e);
                }) {
                    let key_ttl: i64 = get_from_env_or_default("REDIS_KEY_TTL", 604800);
                    let expiration = Some(Expiration::EX(key_ttl));
                    let _ = client
                        .set::<(), String, String>(
                            key, serialized, expiration, None, false,
                        )
                        .await
                        .map_err(|e| {
                            log::error!("Failed to write back data to redis: {}", e);
                        });
                }
            }
            data
        }
    }
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
    let config = {
        // this block is so that the client connection is dropped
        // before we move on to parsing the config
        let config = client
            .get::<String, String>(key_name.clone())
            .await
            .map_err(|e| {
                log::error!("Failed to fetch {key_name} from redis: {}", e);
                format!("Failed to fetch {key_name} from redis due to: {}", e)
            })?;
        let metrics = client.take_latency_metrics();
        let network_metrics = client.take_network_latency_metrics();
        log::trace!(
            "Network metrics for config fetch in milliseconds :: max: {}, min: {}, avg: {}; Latency metrics :: max: {}, min: {}, avg: {}",
            network_metrics.max,
            network_metrics.min,
            network_metrics.avg,
            metrics.max,
            metrics.min,
            metrics.avg
        );
        config
    };

    let value = serde_json::from_str::<T>(&config).map_err(|e| {
        log::error!("Failed to parse value from redis: {}", e);
        format!("Failed to parse value from redis due to: {}", e)
    })?;
    Ok(value)
}
