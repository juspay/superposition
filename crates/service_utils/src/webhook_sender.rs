use std::collections::HashMap;

use diesel::{QueryDsl, RunQueryDsl};
use secrecy::ExposeSecret;
use superposition_macros::unexpected_error;
use superposition_types::{
    DBConnection,
    database::{
        models::others::Webhook,
        schema::{secrets::dsl as secrets_dsl, variables::dsl as variables_dsl},
    },
    result as superposition,
};

use crate::{
    encryption::{EncryptionError, decrypt_secret, decrypt_workspace_key},
    helpers::get_from_env_or_default,
    kronos_dispatch::{has_pattern_in_headers, substitute_templates},
    service::types::{AppState, WorkspaceContext},
};

/// Sends `body` to the webhook target once, with `{{VARS.*}}` / `{{SECRETS.*}}`
/// placeholders in its custom headers substituted. No retries here: Kronos retries
/// failed callbacks; with Kronos disabled the call is best effort.
pub async fn send_webhook(
    webhook: &Webhook,
    body: &serde_json::Value,
    workspace_context: &WorkspaceContext,
    state: &AppState,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    let raw_headers = &*webhook.custom_headers;
    let (has_vars, has_secrets) = has_pattern_in_headers(raw_headers);

    let vars = if has_vars {
        fetch_variables(workspace_context, conn)?
    } else {
        HashMap::new()
    };

    let secrets = if has_secrets {
        fetch_decrypted_secrets(workspace_context, conn, state)?
    } else {
        HashMap::new()
    };

    let method =
        reqwest::Method::from_bytes(webhook.method.to_string().to_uppercase().as_bytes())
            .unwrap_or(reqwest::Method::POST);

    let mut builder = state
        .http_client
        .request(method, webhook.url.as_str())
        .timeout(std::time::Duration::from_secs(get_from_env_or_default(
            "WEBHOOK_OUTBOUND_TIMEOUT_SEC",
            10u64,
        )))
        .json(body);

    for (key, value) in raw_headers {
        let value_str = value
            .as_str()
            .map(String::from)
            .unwrap_or_else(|| value.to_string());
        let rendered = substitute_templates(&value_str, &vars, &secrets);
        builder = builder.header(key.as_str(), rendered.as_str());
    }

    let resp = builder
        .send()
        .await
        .map_err(|e| unexpected_error!("Webhook HTTP send failed: {}", e))?;

    if resp.status().is_success() {
        Ok(())
    } else {
        Err(unexpected_error!(
            "Target returned unexpected status {}",
            resp.status()
        ))
    }
}

fn fetch_variables(
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
) -> superposition::Result<HashMap<String, String>> {
    let variables_map = variables_dsl::variables
        .select((variables_dsl::name, variables_dsl::value))
        .schema_name(&workspace_context.schema_name)
        .load(conn)?
        .into_iter()
        .collect();
    Ok(variables_map)
}

fn fetch_decrypted_secrets(
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
    state: &AppState,
) -> superposition::Result<HashMap<String, String>> {
    let master_encryption_key = match state.master_encryption_key {
        Some(ref key) => key,
        None => {
            log::warn!("Master encryption key not configured, skipping secrets");
            return Ok(HashMap::new());
        }
    };

    let workspace_key = match decrypt_workspace_key(
        &workspace_context.settings.encryption_key,
        master_encryption_key,
    ) {
        Ok(key) => key,
        Err(e) => {
            log::error!("Failed to decrypt workspace key: {}", e);
            return Err(unexpected_error!("Failed to decrypt workspace key"));
        }
    };

    let db_secrets: Vec<(String, String)> = secrets_dsl::secrets
        .schema_name(&workspace_context.schema_name)
        .select((secrets_dsl::name, secrets_dsl::encrypted_value))
        .load(conn)
        .map_err(|e| {
            log::error!("Failed to load secrets: {}", e);
            unexpected_error!("Failed to load secrets")
        })?;

    let result: Result<HashMap<String, String>, EncryptionError> = db_secrets
        .into_iter()
        .map(|(name, encrypted_value)| {
            decrypt_secret(&encrypted_value, &workspace_key)
                .map(|decrypted| (name, decrypted.expose_secret().to_string()))
        })
        .collect();

    result.map_err(|e| {
        log::error!("Failed to decrypt secrets: {}", e);
        unexpected_error!("Failed to decrypt secrets")
    })
}
