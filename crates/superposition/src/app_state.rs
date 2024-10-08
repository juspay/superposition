use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, Mutex},
};

use cac_toml::ContextAwareConfig;
use context_aware_config::helpers::get_meta_schema;
use service_utils::{
    aws::kms,
    db::utils::{get_superposition_token, init_pool_manager},
    helpers::{get_from_env_or_default, get_from_env_unsafe},
    service::types::{AppEnv, AppState, ExperimentationFlags},
};
use snowflake::SnowflakeIdGenerator;
use superposition_types::TenantConfig;

const TENANT_CONFIG_FILE: &str = "crates/superposition/Superposition.cac.toml";

pub async fn get(
    service_prefix: String,
    base: &String,
    tenants: &HashSet<String>,
) -> AppState {
    let cac_host =
        get_from_env_unsafe::<String>("CAC_HOST").expect("CAC host is not set") + base;
    let max_pool_size = get_from_env_or_default("MAX_DB_CONNECTION_POOL_SIZE", 2);
    let app_env = get_from_env_unsafe("APP_ENV").expect("APP_ENV is not set");
    let enable_tenant_and_scope = get_from_env_unsafe("ENABLE_TENANT_AND_SCOPE")
        .expect("ENABLE_TENANT_AND_SCOPE is not set");

    let cac = ContextAwareConfig::parse(TENANT_CONFIG_FILE)
        .expect(&format!("File {TENANT_CONFIG_FILE} not found"));

    let tenant_configs = tenants
        .clone()
        .into_iter()
        .filter_map(|tenant| {
            serde_json::to_value(cac.get_resolved_config(&HashMap::from_iter(vec![(
                String::from("tenant"),
                toml::Value::String(tenant.clone()),
            )])))
            .and_then(serde_json::from_value::<TenantConfig>)
            .map(|config| ((tenant, config)))
            .ok()
        })
        .collect::<HashMap<_, _>>();

    let snowflake_generator = Arc::new(Mutex::new(SnowflakeIdGenerator::new(1, 1)));

    let kms_client = match app_env {
        AppEnv::DEV | AppEnv::TEST => None,
        _ => Some(kms::new_client().await),
    };

    AppState {
        db_pool: init_pool_manager(
            tenants.clone(),
            enable_tenant_and_scope,
            &kms_client,
            &app_env,
            max_pool_size,
        )
        .await,
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
        enable_tenant_and_scope,
        tenants: tenants.clone(),
        tenant_middleware_exclusion_list: get_from_env_unsafe::<String>(
            "TENANT_MIDDLEWARE_EXCLUSION_LIST",
        )
        .expect("TENANT_MIDDLEWARE_EXCLUSION_LIST is not set")
        .split(',')
        .map(String::from)
        .collect::<HashSet<_>>(),
        service_prefix,
        tenant_configs,
        superposition_token: get_superposition_token(&kms_client, &app_env).await,
    }
}
