use std::collections::HashMap;

use base64::{Engine, engine::general_purpose};
use kronos_common::{sqlx, tenant::SchemaProvider};
use kronos_worker::{JobTrigger, KronosClient};
use once_cell::sync::Lazy;
use serde::Serialize;
use serde_json::json;

use crate::helpers::get_from_env_or_default;

static CONFIG_REFERENCE_REGEX: Lazy<regex::Regex> = Lazy::new(|| {
    regex::Regex::new(r"\{\{(?P<type>VARS|SECRETS)\.(?P<name>[A-Z0-9_]+)\}\}")
        .expect("Invalid config pattern")
});

pub const DISPATCHER_ENDPOINT_NAME: &str = "superposition-webhook-dispatcher";
pub const DISPATCHER_SECRET_NAME: &str = "superposition-internal-token";
/// Username part of the Basic credential the dispatcher calls back with.
pub const DISPATCHER_USERNAME: &str = "kronos-dispatcher";

/// Build the Basic credential blob stored as the Kronos secret:
/// `base64("kronos-dispatcher:<token>")`. Kronos stores it encrypted and sends
/// it verbatim in the Authorization header; SP decodes and verifies it.
pub fn dispatcher_basic_credential(dispatch_token: &str) -> String {
    general_purpose::STANDARD.encode(format!("{DISPATCHER_USERNAME}:{dispatch_token}"))
}

pub struct SuperpositionSchemaProvider {
    pool: sqlx::PgPool,
}

impl SuperpositionSchemaProvider {
    pub fn new(pool: sqlx::PgPool) -> Self {
        Self { pool }
    }
}

impl SchemaProvider for SuperpositionSchemaProvider {
    async fn get_active_schemas(&self) -> Result<Vec<String>, sqlx::Error> {
        // TODO: add in-memory caching here (e.g. Arc<RwLock<(Vec<String>, Instant)>> with a
        // configurable TTL) to avoid querying the DB on every poll tick (~200ms). See
        // SchemaRegistry in kronos/crates/common/src/tenant.rs for the pattern to follow.

        let schemas: Vec<(String,)> = sqlx::query_as(
            "SELECT w.workspace_schema_name
             FROM superposition.workspaces w
             WHERE w.workspace_status = 'ENABLED'",
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(schemas.into_iter().map(|r| r.0).collect())
    }
}

/// Endpoint spec for the webhook dispatcher (both service and library mode).
///
/// The callback URL is generic (no org/workspace in the path); the SP workspace is carried
/// per-job via `{{input.org_id}}` / `{{input.workspace}}` templated headers, which Kronos
/// fills from each job's input before dispatching. SP's header-based dispatch route then
/// resolves the right schema.
pub fn dispatcher_endpoint_spec(dispatcher_url: &str) -> serde_json::Value {
    let timeout_ms: u64 = get_from_env_or_default("DISPATCHER_TIMEOUT_MS", 15000);
    json!({
        "url": dispatcher_url,
        "method": "POST",
        // `Authorization: Basic <base64(kronos-dispatcher:token)>` — matches the
        // org-wide API-key-in-Basic convention. The stored secret is already the
        // base64 blob (see `dispatcher_basic_credential`); Kronos sends it as-is.
        // SP's auth middleware decodes it, verifies the token, and assigns the
        // fixed kronos-dispatcher identity. The SP workspace rides in the
        // `x-org-id`/`x-workspace` headers, templated from the job input.
        "headers": {
            "Authorization": format!("Basic {{{{secret.{DISPATCHER_SECRET_NAME}}}}}"),
            "x-org-id": "{{input.org_id}}",
            "x-workspace": "{{input.workspace}}"
        },
        "timeout_ms": timeout_ms,
        "expected_status_codes": [200]
    })
}

// max_attempts is intentionally omitted — Kronos uses per-job max_attempts (from webhook.max_retries).
// The endpoint retry policy only controls backoff timing between attempts.
pub fn dispatcher_retry_policy() -> serde_json::Value {
    json!({
        "backoff": "exponential",
        "initial_delay_ms": 1000,
        "max_delay_ms": 30000
    })
}

pub async fn setup_dispatcher(
    kronos_client: &dyn KronosClient,
    workspace: &str,
    superposition_host: &str,
    dispatch_token: &str,
) {
    if let Err(e) = kronos_client.provision_workspace(workspace).await {
        log::warn!(
            "Kronos dispatcher: provision_workspace('{workspace}') failed (may already exist): {e}"
        );
    }

    let dispatcher_url = format!("{superposition_host}/dispatch/webhook");
    let basic_credential = dispatcher_basic_credential(dispatch_token);
    if let Err(e) = kronos_client
        .upsert_secret(workspace, DISPATCHER_SECRET_NAME, &basic_credential)
        .await
    {
        log::warn!("Kronos dispatcher: secret upsert failed for '{workspace}': {e}");
    }
    if let Err(e) = kronos_client
        .register_endpoint(
            workspace,
            DISPATCHER_ENDPOINT_NAME,
            "HTTP",
            dispatcher_endpoint_spec(&dispatcher_url),
            Some(dispatcher_retry_policy()),
        )
        .await
    {
        log::warn!("Kronos dispatcher: endpoint register failed for '{workspace}': {e}");
    }
}

pub fn has_pattern_in_headers(
    headers: &serde_json::Map<String, serde_json::Value>,
) -> (bool, bool) {
    let mut has_vars = false;
    let mut has_secrets = false;
    for value in headers.values() {
        let ref_type = value
            .as_str()
            .and_then(|s| CONFIG_REFERENCE_REGEX.captures(s))
            .and_then(|caps| caps.name("type"))
            .map(|m| m.as_str());
        match ref_type {
            Some("VARS") => has_vars = true,
            Some("SECRETS") => has_secrets = true,
            _ => (),
        }
    }
    (has_vars, has_secrets)
}

pub fn substitute_templates(
    template: &str,
    variables: &HashMap<String, String>,
    secrets: &HashMap<String, String>,
) -> String {
    CONFIG_REFERENCE_REGEX
        .replace_all(template, |caps: &regex::Captures| {
            let ref_type = caps.name("type").map(|m| m.as_str());
            let ref_name = caps.name("name").map(|m| m.as_str());

            match (ref_type, ref_name) {
                (Some("VARS"), Some(name)) => variables.get(name).cloned(),
                (Some("SECRETS"), Some(name)) => secrets.get(name).cloned(),
                _ => None,
            }
            .unwrap_or_else(|| caps[0].to_string())
        })
        .into_owned()
}

// `target_workspace` is the Kronos workspace the job is created in (the shared workspace in
// service mode, or the SP schema in library mode). `org_id`/`workspace` identify the *SP*
// workspace the webhook belongs to; they ride in the input so the dispatcher's templated
// headers can route the callback back to the right SP schema.
#[allow(clippy::too_many_arguments)]
pub async fn submit_webhook_job<T: Serialize>(
    kronos_client: &dyn KronosClient,
    target_workspace: &str,
    org_id: &str,
    workspace: &str,
    webhook_name: &str,
    payload: &T,
    idempotency_key: &str,
    max_attempts: i32,
) -> anyhow::Result<String> {
    let input = json!({
        "org_id": org_id,
        "workspace": workspace,
        "webhook_name": webhook_name,
        "data": serde_json::to_value(payload)?
    });
    kronos_client
        .create_job(
            target_workspace,
            DISPATCHER_ENDPOINT_NAME,
            input,
            max_attempts as i64,
            JobTrigger::Immediate,
            Some(idempotency_key),
        )
        .await
}
