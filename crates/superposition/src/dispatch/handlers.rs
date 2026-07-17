use std::collections::HashMap;

use actix_web::{
    HttpResponse, Scope, post,
    web::{Data, Json},
};
use context_aware_config::api::{
    config::execute_reduce, context::execute_priority_recompute,
};
use diesel::{QueryDsl, RunQueryDsl};
use secrecy::ExposeSecret;
use serde::Deserialize;
use service_utils::{
    encryption::{EncryptionError, decrypt_secret, decrypt_workspace_key},
    helpers::get_from_env_or_default,
    kronos_dispatch::{
        append_job_logs, has_pattern_in_headers, substitute_templates,
        update_job_progress, update_job_status,
    },
    service::types::{AppState, DbConnection, WorkspaceContext},
};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::unexpected_error;
use superposition_types::{
    User,
    api::jobs::{JobDispatchRequest, JobRequest},
    database::models::BackgroundJobStatus,
    database::schema::{secrets::dsl as secrets_dsl, variables::dsl as variables_dsl},
    result as superposition,
};

use crate::webhooks::helper::fetch_webhook;

declare_resource!(Webhook);

#[derive(Deserialize)]
struct DispatchWebhookRequest {
    webhook_name: String,
    data: serde_json::Value,
}

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(dispatch_handler)
        .service(dispatch_job_handler)
}

#[authorized]
#[post("/webhook")]
async fn dispatch_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    db_conn: DbConnection,
    body: Json<DispatchWebhookRequest>,
) -> superposition::Result<HttpResponse> {
    let DispatchWebhookRequest { webhook_name, data } = body.into_inner();
    let DbConnection(mut conn) = db_conn;

    execute_webhook_dispatch(&workspace_context, &state, &mut conn, &webhook_name, &data)
        .await?;
    Ok(HttpResponse::Ok().finish())
}

#[authorized]
#[post("/job")]
async fn dispatch_job_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    db_conn: DbConnection,
    user: User,
    body: Json<JobDispatchRequest>,
) -> superposition::Result<HttpResponse> {
    let JobDispatchRequest {
        job_id,
        job_request,
    } = body.into_inner();
    let DbConnection(mut conn) = db_conn;

    update_job_status(&mut conn, job_id, BackgroundJobStatus::Inprogress)
        .map_err(|e| unexpected_error!("Failed to update job status: {}", e))?;

    let result = match &job_request {
        JobRequest::Webhook(req) => {
            execute_webhook_dispatch(
                &workspace_context,
                &state,
                &mut conn,
                &req.webhook_name,
                &req.data,
            )
            .await
        }
        JobRequest::PriorityRecompute(_) => {
            execute_priority_recompute(&workspace_context, &state, &mut conn, &user).await
        }
        JobRequest::Reduce(req) => {
            execute_reduce(&workspace_context, &state, &mut conn, &user, req.approve)
                .await
        }
    };

    match result {
        Ok(()) => {
            update_job_status(&mut conn, job_id, BackgroundJobStatus::Completed)
                .map_err(|e| unexpected_error!("Failed to update job status: {}", e))?;
            update_job_progress(&mut conn, job_id, 100)
                .map_err(|e| unexpected_error!("Failed to update job progress: {}", e))?;
            Ok(HttpResponse::Ok().finish())
        }
        Err(e) => {
            let error_msg = format!("{e}");
            log::error!("Job {job_id} failed: {error_msg}");
            update_job_status(&mut conn, job_id, BackgroundJobStatus::Failed)
                .map_err(|e| unexpected_error!("Failed to update job status: {}", e))?;
            append_job_logs(&mut conn, job_id, &error_msg)
                .map_err(|e| unexpected_error!("Failed to append logs: {}", e))?;
            Err(unexpected_error!("Job {job_id} failed: {error_msg}"))
        }
    }
}

async fn execute_webhook_dispatch(
    workspace_context: &WorkspaceContext,
    state: &Data<AppState>,
    conn: &mut diesel::r2d2::PooledConnection<
        diesel::r2d2::ConnectionManager<diesel::PgConnection>,
    >,
    webhook_name: &String,
    data: &serde_json::Value,
) -> superposition::Result<()> {
    let webhook = fetch_webhook(webhook_name, &workspace_context.schema_name, conn)?;

    if !webhook.enabled {
        return Ok(());
    }

    let raw_headers = (*webhook.custom_headers).clone();
    let (has_vars, has_secrets) = has_pattern_in_headers(&raw_headers);

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
        .json(data);

    for (key, value) in &raw_headers {
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
        .map_err(|e| unexpected_error!("Dispatcher HTTP send failed: {}", e))?;

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
    conn: &mut diesel::r2d2::PooledConnection<
        diesel::r2d2::ConnectionManager<diesel::PgConnection>,
    >,
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
    conn: &mut diesel::r2d2::PooledConnection<
        diesel::r2d2::ConnectionManager<diesel::PgConnection>,
    >,
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
