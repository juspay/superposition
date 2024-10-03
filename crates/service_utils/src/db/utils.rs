use std::collections::HashSet;

use aws_sdk_kms::Client;
use urlencoding::encode;

use crate::aws::kms;
use crate::db::pgschema_manager::{ConnectionConfig, PgSchemaManager};
use crate::helpers::{get_from_env_or_default, get_from_env_unsafe};
use crate::service::types::AppEnv;

pub async fn get_superposition_token(
    kms_client: &Option<Client>,
    app_env: &AppEnv,
) -> String {
    match app_env {
        AppEnv::DEV | AppEnv::TEST | AppEnv::SANDBOX => {
            get_from_env_or_default("SUPERPOSITION_TOKEN", "123456".into())
        }
        _ => {
            let kms_client = kms_client.clone().unwrap();
            let superposition_token_raw =
                kms::decrypt(kms_client, "SUPERPOSITION_TOKEN").await;
            encode(superposition_token_raw.as_str()).to_string()
        }
    }
}

pub async fn get_database_url(kms_client: &Option<Client>, app_env: &AppEnv) -> String {
    let db_user: String = get_from_env_unsafe("DB_USER").unwrap();
    let db_password: String = match app_env {
        AppEnv::DEV | AppEnv::TEST => {
            get_from_env_or_default("DB_PASSWORD", "docker".into())
        }
        _ => {
            let kms_client = kms_client.clone().unwrap();
            let db_password_raw = kms::decrypt(kms_client, "DB_PASSWORD").await;
            encode(db_password_raw.as_str()).to_string()
        }
    };
    let db_host: String = get_from_env_unsafe("DB_HOST").unwrap();
    let db_name: String = get_from_env_unsafe("DB_NAME").unwrap();
    format!("postgres://{db_user}:{db_password}@{db_host}/{db_name}")
}

pub async fn init_pool_manager(
    tenants: HashSet<String>,
    enable_tenant_and_scope: bool,
    kms_client: &Option<Client>,
    app_env: &AppEnv,
    max_pool_size: u32,
) -> PgSchemaManager {
    let database_url = get_database_url(kms_client, app_env).await;
    let namespaces = match (enable_tenant_and_scope, app_env) {
        (true, _) => tenants
            .iter()
            .flat_map(|tenant| {
                [
                    format!("{}_cac", tenant),
                    format!("{}_experimentation", tenant),
                ]
            })
            .collect::<Vec<String>>(),
        (false, _) => vec!["cac_v1".to_string()],
    };

    let connection_configs = namespaces
        .iter()
        .map(|namespace| {
            ConnectionConfig::new(
                namespace.to_string(),
                database_url.clone(),
                namespace.to_string(),
                max_pool_size,
            )
        })
        .collect::<Vec<ConnectionConfig>>();

    PgSchemaManager::from(connection_configs)
}
