use crate::aws::kms;
use crate::db::pgschema_manager::{ConnectionConfig, PgSchemaManager};
use crate::helpers::{get_from_env_or_default, get_from_env_unsafe};
use crate::service::types::AppEnv;
use diesel::{
    r2d2::{ConnectionManager, Pool},
    PgConnection,
};
use std::collections::HashSet;
use urlencoding::encode;

pub async fn get_database_url() -> String {
    let db_user: String = get_from_env_unsafe("DB_USER").unwrap();
    let kms_client = kms::new_client();
    let db_password_raw = kms::decrypt(kms_client, "DB_PASSWORD").await;
    let db_password = encode(db_password_raw.as_str()).to_string();
    let db_host: String = get_from_env_unsafe("DB_HOST").unwrap();
    let db_name: String = get_from_env_unsafe("DB_NAME").unwrap();
    format!("postgres://{db_user}:{db_password}@{db_host}/{db_name}")
}

pub async fn get_pool() -> Pool<ConnectionManager<PgConnection>> {
    let db_url = get_database_url().await;
    let manager: ConnectionManager<PgConnection> =
        ConnectionManager::<PgConnection>::new(db_url);
    Pool::builder()
        .max_size(get_from_env_or_default("MAX_DB_CONNECTION_POOL_SIZE", 3))
        .build(manager)
        .expect("Error building a connection pool")
}

pub async fn init_pool_manager(
    tenants: HashSet<String>,
    enable_tenant_and_scope: bool,
    app_env: AppEnv,
    max_pool_size: u32,
) -> PgSchemaManager {
    let database_url = get_database_url().await;
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
