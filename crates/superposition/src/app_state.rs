use std::{
    collections::HashSet,
    sync::{Arc, Mutex},
};

#[cfg(feature = "high-performance-mode")]
use std::time::Duration;

use context_aware_config::helpers::get_meta_schema;

#[cfg(feature = "high-performance-mode")]
use fred::{
    clients::RedisPool,
    interfaces::ClientLike,
    types::{ConnectionConfig, PerformanceConfig, ReconnectPolicy, RedisConfig},
};
use futures_util::future::join_all;
use service_utils::{
    aws::kms,
    db::utils::{get_superposition_token, init_pool_manager},
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
    let cac_host =
        get_from_env_or_default::<String>("CAC_HOST", format!("http://localhost:{port}"))
            + base;
    let max_pool_size = get_from_env_or_default("MAX_DB_CONNECTION_POOL_SIZE", 2);

    let snowflake_generator = Arc::new(Mutex::new(SnowflakeIdGenerator::new(1, 1)));

    #[cfg(feature = "high-performance-mode")]
    let redis_pool = {
        let redis_url =
            get_from_env_or_default("REDIS_URL", String::from("http://localhost:6379"));
        let redis_pool_size = get_from_env_or_default("REDIS_POOL_SIZE", 10);
        let redis_max_attempts = get_from_env_or_default("REDIS_MAX_ATTEMPTS", 10);
        let redis_connection_timeout =
            get_from_env_or_default("REDIS_CONN_TIMEOUT", 1000);
        let config = RedisConfig::from_url(&redis_url).expect(
            format!("Failed to create RedisConfig from url {}", redis_url).as_str(),
        );
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

        redis_pool
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
        meta_schema: get_meta_schema(),
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
        #[cfg(feature = "high-performance-mode")]
        redis: redis_pool,
        http_client: reqwest::Client::new(),
        encrypted_keys: join_all(get_from_env_or_default::<String>("ENCRYPTED_KEYS", String::new())
            .split(',')
            .filter(|s| !s.is_empty())
            .map(|key| {
                async move {
                    let decrypted_value: String = match app_env {
                        AppEnv::DEV | AppEnv::TEST => {
                            get_from_env_or_default(key, "1234".into())
                        }
                        _ => {
                            let kms_client = kms_client.clone().expect(
                                "The KMS client could not be initialized. Please check the AWS configuration for this service.",
                            );
                            kms::decrypt(kms_client, key).await
                        }
                    };
                    (key.to_string(), decrypted_value)
                }
            })
        ).await
        .into_iter()
        .collect(),
    }
}
