use aws_sdk_kms::Client;
use diesel::{
    PgConnection,
    r2d2::{ConnectionManager, Pool},
};
use urlencoding::encode;

use crate::aws::kms;
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
        _ => kms::decrypt(kms_client.clone().unwrap(), "SUPERPOSITION_TOKEN").await,
    }
}

pub async fn get_kronos_dispatch_token(
    kms_client: &Option<Client>,
    app_env: &AppEnv,
) -> String {
    match app_env {
        AppEnv::DEV | AppEnv::TEST => {
            get_from_env_or_default("KRONOS_DISPATCH_TOKEN", "dispatch-123456".into())
        }
        _ => kms::decrypt(kms_client.clone().unwrap(), "KRONOS_DISPATCH_TOKEN").await,
    }
}

pub async fn get_oidc_client_secret(
    kms_client: &Option<Client>,
    app_env: &AppEnv,
) -> String {
    match app_env {
        AppEnv::DEV | AppEnv::TEST | AppEnv::SANDBOX => {
            get_from_env_or_default("OIDC_CLIENT_SECRET", "123456".into())
        }
        _ => kms::decrypt(kms_client.clone().unwrap(), "OIDC_CLIENT_SECRET").await,
    }
}

/// The verbatim `Authorization` header value Superposition presents to the RFC
/// 7662 introspection endpoint (e.g. `Bearer <token>` or `Basic <base64>`).
/// This is a secret and is KMS-decrypted like other secrets in non-dev
/// environments. Returns `None` when the (optional) API-token flow is not
/// configured.
pub async fn get_introspection_auth_header(
    kms_client: &Option<Client>,
    app_env: &AppEnv,
) -> Option<String> {
    if std::env::var("OIDC_INTROSPECTION_AUTH_HEADER").is_err() {
        return None;
    }
    match app_env {
        AppEnv::DEV | AppEnv::TEST | AppEnv::SANDBOX => {
            std::env::var("OIDC_INTROSPECTION_AUTH_HEADER").ok()
        }
        _ => Some(
            kms::decrypt(
                kms_client.clone().unwrap(),
                "OIDC_INTROSPECTION_AUTH_HEADER",
            )
            .await,
        ),
    }
}

/// The JSON array of static API tokens (`OIDC_API_STATIC_TOKENS`), each entry
/// `{token, principal[, email, org]}`. A secret, KMS-decrypted like other
/// secrets in non-dev environments. Returns `None` when unset (static-token
/// mechanism disabled).
pub async fn get_static_api_tokens(
    kms_client: &Option<Client>,
    app_env: &AppEnv,
) -> Option<String> {
    if std::env::var("OIDC_API_STATIC_TOKENS").is_err() {
        return None;
    }
    match app_env {
        AppEnv::DEV | AppEnv::TEST | AppEnv::SANDBOX => {
            std::env::var("OIDC_API_STATIC_TOKENS").ok()
        }
        _ => Some(
            kms::decrypt(kms_client.clone().unwrap(), "OIDC_API_STATIC_TOKENS").await,
        ),
    }
}

pub async fn get_kronos_api_key(kms_client: &Option<Client>, app_env: &AppEnv) -> String {
    match app_env {
        AppEnv::DEV | AppEnv::TEST => {
            get_from_env_or_default("KRONOS_API_KEY", "dev-api-key".into())
        }
        _ => kms::decrypt(kms_client.clone().unwrap(), "KRONOS_API_KEY").await,
    }
}

pub async fn get_database_url(
    kms_client: &Option<Client>,
    app_env: &AppEnv,
    env_prefix: Option<&str>,
) -> String {
    let env_prefix = env_prefix
        .filter(|s| !s.is_empty())
        .map(|s| format!("{s}_"))
        .unwrap_or_default();

    let db_user: String = get_from_env_unsafe(&format!("{env_prefix}DB_USER")).unwrap();
    let db_password: String = match app_env {
        AppEnv::DEV | AppEnv::TEST => {
            get_from_env_or_default(&format!("{env_prefix}DB_PASSWORD"), "docker".into())
        }
        _ => {
            let kms_client = kms_client.clone().unwrap();
            let db_password_raw =
                kms::decrypt(kms_client, &format!("{env_prefix}DB_PASSWORD")).await;
            encode(db_password_raw.as_str()).to_string()
        }
    };
    let db_host: String = get_from_env_unsafe(&format!("{env_prefix}DB_HOST")).unwrap();
    let db_name: String = get_from_env_unsafe(&format!("{env_prefix}DB_NAME")).unwrap();
    format!("postgres://{db_user}:{db_password}@{db_host}/{db_name}")
}

pub async fn init_pool_manager(
    kms_client: &Option<Client>,
    app_env: &AppEnv,
    max_pool_size: u32,
) -> Pool<ConnectionManager<PgConnection>> {
    let database_url = get_database_url(kms_client, app_env, None).await;
    let manager = ConnectionManager::<PgConnection>::new(database_url);
    Pool::builder()
        .max_size(max_pool_size)
        .build(manager)
        .unwrap()
}
