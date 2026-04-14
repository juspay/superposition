use std::{
    collections::HashSet,
    sync::{Arc, Mutex},
    time::Duration,
};

use fred::{
    clients::RedisPool,
    interfaces::ClientLike,
    types::{ConnectionConfig, PerformanceConfig, ReconnectPolicy, RedisConfig},
};
use service_utils::{
    db::utils::{get_superposition_token, init_pool_manager},
    encryption::get_master_encryption_keys,
    helpers::{get_from_env_or_default, get_from_env_unsafe},
    service::types::{AppEnv, AppState, ExperimentationFlags},
};
use snowflake::SnowflakeIdGenerator;

pub async fn get(
    app_env: AppEnv,
    port: u16,
    kms_client: &Option<aws_sdk_kms::Client>,
    service_prefix: String,
    base: &str,
) -> AppState {
    let master_encryption_key = get_master_encryption_keys(kms_client, &app_env)
        .await
        .unwrap_or_else(|e| {
            panic!("Error getting encryption keys: {e}");
        });
    let cac_host =
        get_from_env_or_default::<String>("CAC_HOST", format!("http://localhost:{port}"))
            + base;
    let max_pool_size = get_from_env_or_default("MAX_DB_CONNECTION_POOL_SIZE", 2);
    let redis_lock_ttl_ms = get_from_env_or_default("REDIS_LOCK_TTL_MS", 30_000_u64);
    let redis_lock_heartbeat_enabled =
        get_from_env_or_default("REDIS_LOCK_HEARTBEAT_ENABLED", true);
    let workspace_lock_retry_interval_ms =
        get_from_env_or_default("WORKSPACE_LOCK_RETRY_INTERVAL_MS", 100_u64);
    let workspace_lock_retry_timeout_ms =
        get_from_env_or_default("WORKSPACE_LOCK_RETRY_TIMEOUT_MS", 2_000_u64);

    let snowflake_generator = Arc::new(Mutex::new(SnowflakeIdGenerator::new(1, 1)));

    // Initialize Redis pool only if REDIS_URL is explicitly set (not default)
    let redis_pool = match std::env::var("REDIS_URL") {
        Ok(redis_url) if !redis_url.is_empty() => {
            let redis_pool_size = get_from_env_or_default("REDIS_POOL_SIZE", 10);
            let redis_max_attempts = get_from_env_or_default("REDIS_MAX_ATTEMPTS", 10);
            let redis_connection_timeout =
                get_from_env_or_default("REDIS_CONN_TIMEOUT", 1000);
            let config = RedisConfig::from_url(&redis_url).unwrap_or_else(|_| {
                panic!("Failed to create RedisConfig from url {}", redis_url)
            });
            let reconnect_policy = ReconnectPolicy::new_constant(redis_max_attempts, 100);
            let redis_pool = RedisPool::new(
                config,
                Some(PerformanceConfig {
                    auto_pipeline: true,
                    ..Default::default()
                }),
                Some(ConnectionConfig {
                    connection_timeout: Duration::from_millis(redis_connection_timeout),
                    ..Default::default()
                }),
                Some(reconnect_policy),
                redis_pool_size,
            )
            .map_err(|e| format!("Could not connect to redis due to {e}"))
            .unwrap();

            redis_pool.connect();
            redis_pool
                .wait_for_connect()
                .await
                .expect("Failed to connect to Redis");

            Some(redis_pool)
        }
        _ => {
            log::info!("REDIS_URL not set, Redis caching disabled");
            None
        }
    };

    AppState {
        db_pool: init_pool_manager(kms_client, &app_env, max_pool_size).await,
        cac_host,
        cac_version: get_from_env_unsafe("SUPERPOSITION_VERSION")
            .expect("SUPERPOSITION_VERSION is not set"),
        experimentation_flags: ExperimentationFlags {
            allow_same_keys_overlapping_ctx: get_from_env_unsafe(
                "ALLOW_SAME_KEYS_OVERLAPPING_CTX",
            )
            .expect("ALLOW_SAME_KEYS_OVERLAPPING_CTX not set"),
            allow_diff_keys_overlapping_ctx: get_from_env_unsafe(
                "ALLOW_DIFF_KEYS_OVERLAPPING_CTX",
            )
            .expect("ALLOW_DIFF_KEYS_OVERLAPPING_CTX not set"),
            allow_same_keys_non_overlapping_ctx: get_from_env_unsafe(
                "ALLOW_SAME_KEYS_NON_OVERLAPPING_CTX",
            )
            .expect("ALLOW_SAME_KEYS_NON_OVERLAPPING_CTX not set"),
        },
        snowflake_generator,
        app_env,
        tenant_middleware_exclusion_list: get_from_env_unsafe::<String>(
            "TENANT_MIDDLEWARE_EXCLUSION_LIST",
        )
        .expect("TENANT_MIDDLEWARE_EXCLUSION_LIST is not set")
        .split(',')
        .map(String::from)
        .collect::<HashSet<_>>(),
        service_prefix,
        superposition_token: get_superposition_token(kms_client, &app_env).await,
        redis: redis_pool,
        redis_lock_ttl: Duration::from_millis(redis_lock_ttl_ms),
        redis_lock_heartbeat_enabled,
        workspace_lock_retry_interval: Duration::from_millis(
            workspace_lock_retry_interval_ms,
        ),
        workspace_lock_retry_timeout: Duration::from_millis(
            workspace_lock_retry_timeout_ms,
        ),
        http_client: reqwest::Client::new(),
        master_encryption_key,
    }
}
