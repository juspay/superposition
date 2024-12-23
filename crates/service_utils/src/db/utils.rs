use diesel::r2d2::{ConnectionManager, Pool};
use diesel::PgConnection;

use crate::helpers::{get_from_env_or_default, get_from_env_unsafe};

pub async fn get_superposition_token() -> String {
    get_from_env_or_default("SUPERPOSITION_TOKEN", "123456".into())
}

pub async fn get_oidc_client_secret() -> String {
    get_from_env_or_default("OIDC_CLIENT_SECRET", "123456".into())
}

pub async fn get_database_url() -> String {
    let db_user: String = get_from_env_unsafe("DB_USER").unwrap();
    let db_password: String = get_from_env_or_default("DB_PASSWORD", "docker".into());
    let db_host: String = get_from_env_unsafe("DB_HOST").unwrap();
    let db_name: String = get_from_env_unsafe("DB_NAME").unwrap();
    format!("postgres://{db_user}:{db_password}@{db_host}/{db_name}")
}

pub async fn init_pool_manager(
    max_pool_size: u32,
) -> Pool<ConnectionManager<PgConnection>> {
    let database_url = get_database_url().await;
    let manager = ConnectionManager::<PgConnection>::new(database_url);
    Pool::builder()
        .max_size(max_pool_size)
        .build(manager)
        .unwrap()
}
