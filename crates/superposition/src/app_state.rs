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
use kronos_worker::{
    KronosClient, KronosHttpClient, KronosLibraryClient, WorkerConfig, WorkerHandle,
};
use service_utils::{
    db::utils::{
        get_database_url, get_kronos_api_key, get_superposition_token, init_pool_manager,
    },
    encryption::get_master_encryption_keys,
    helpers::{get_from_env_or_default, get_from_env_unsafe},
    kronos_dispatch::{SuperpositionSchemaProvider, setup_dispatcher},
    service::types::{AppEnv, AppState, ExperimentationFlags},
};
use snowflake::SnowflakeIdGenerator;

pub async fn get(
    app_env: AppEnv,
    port: u16,
    kms_client: &Option<aws_sdk_kms::Client>,
    service_prefix: String,
    base: &str,
) -> (AppState, Option<WorkerHandle>, u64) {
    let master_encryption_key = get_master_encryption_keys(kms_client, &app_env)
        .await
        .unwrap_or_else(|e| {
            panic!("Error getting encryption keys: {e}");
        });
    let cac_host =
        get_from_env_or_default::<String>("CAC_HOST", format!("http://localhost:{port}"))
            + base;
    let superposition_host =
        std::env::var("SUPERPOSITION_HOST").unwrap_or_else(|_| cac_host.clone());
    let max_pool_size = get_from_env_or_default("MAX_DB_CONNECTION_POOL_SIZE", 2);
    let workspace_lock_default_ttl_ms =
        get_from_env_or_default("WORKSPACE_LOCK_DEFAULT_TTL_MS", 60_000_u64);
    let workspace_lock_batch_ttl_ms =
        get_from_env_or_default("WORKSPACE_LOCK_BATCH_TTL_MS", 1_200_000_u64);

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

    let superposition_token = get_superposition_token(kms_client, &app_env).await;

    let kronos_url = std::env::var("KRONOS_URL").ok();
    let (kronos_client, worker_handle, kronos_shutdown_timeout_sec, kronos_workspace): (
        Arc<dyn KronosClient>,
        Option<WorkerHandle>,
        u64,
        Option<String>,
    ) = if let Some(kronos_url) = kronos_url {
        let api_key = get_kronos_api_key(kms_client, &app_env).await;
        let org_id =
            get_from_env_or_default("KRONOS_ORG_ID", "superposition".to_string());
        // Single shared Kronos workspace for all SP schemas. Slug must be Kronos-valid
        // (lowercase/digits/hyphens, <=25 chars).
        let workspace =
            get_from_env_or_default("KRONOS_WORKSPACE", "superposition".to_string());
        log::info!(
            "Kronos service mode: {kronos_url} (org={org_id}, workspace={workspace})"
        );
        let client: Arc<dyn KronosClient> =
            Arc::new(KronosHttpClient::new(kronos_url, api_key, org_id));

        setup_dispatcher(
            client.as_ref(),
            &workspace,
            &superposition_host,
            &superposition_token,
        )
        .await;

        (client, None, 0, Some(workspace))
    } else {
        let database_url = get_database_url(kms_client, &app_env, None).await;
        let pool_size = get_from_env_or_default("KRONOS_DB_POOL_SIZE", 1);
        let kronos_encryption_key =
            get_from_env_or_default("KRONOS_ENCRYPTION_KEY", "0".repeat(64));
        let table_prefix =
            get_from_env_or_default("KRONOS_TABLE_PREFIX", "kronos_".to_string());
        let client = KronosLibraryClient::from_database_url(
            &database_url,
            pool_size,
            &table_prefix,
            &kronos_encryption_key,
            None,
        )
        .await
        .expect("Failed to create KronosLibraryClient");
        let schema_provider = SuperpositionSchemaProvider::new(client.pool().clone());
        let worker_config = WorkerConfig::default();
        let shutdown_timeout_sec = worker_config.shutdown_timeout_sec;
        let handle = client.start_worker(schema_provider, worker_config);
        log::info!("Kronos library mode: embedded worker started");
        (Arc::new(client), Some(handle), shutdown_timeout_sec, None)
    };

    let state = AppState {
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
        superposition_token,
        redis: redis_pool,
        workspace_lock_default_ttl: Duration::from_millis(workspace_lock_default_ttl_ms),
        workspace_lock_batch_ttl: Duration::from_millis(workspace_lock_batch_ttl_ms),
        http_client: reqwest::Client::new(),
        master_encryption_key,
        kronos_client,
        kronos_workspace,
        superposition_host,
    };

    (state, worker_handle, kronos_shutdown_timeout_sec)
}
