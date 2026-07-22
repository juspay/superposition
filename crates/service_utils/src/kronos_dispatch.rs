use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use base64::{Engine, engine::general_purpose};
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use kronos_common::{sqlx, tenant::SchemaProvider};
use kronos_worker::{JobTrigger, KronosClient};
use once_cell::sync::Lazy;
use serde::Serialize;
use serde_json::json;
use snowflake::SnowflakeIdGenerator;
use superposition_types::{
    DBConnection,
    api::jobs::{JobCreateResponse, JobListFilters, JobRequest},
    custom_query::PaginationParams,
    database::{
        models::{
            BackgroundJob, BackgroundJobStatus, JobWorkspace, others::WorkspaceJobView,
        },
        schema::job_manager::dsl as job_manager_view_dsl,
        superposition_schema::superposition::job_manager::dsl as job_manager_dsl,
    },
};

use crate::{helpers::get_from_env_or_default, service::types::SchemaName};

static CONFIG_REFERENCE_REGEX: Lazy<regex::Regex> = Lazy::new(|| {
    regex::Regex::new(r"\{\{(?P<type>VARS|SECRETS)\.(?P<name>[A-Z0-9_]+)\}\}")
        .expect("Invalid config pattern")
});

pub const DISPATCHER_ENDPOINT_NAME: &str = "superposition-webhook-dispatcher";
pub const JOB_DISPATCHER_ENDPOINT_NAME: &str = "superposition-job-dispatcher";
pub const DISPATCHER_SECRET_NAME: &str = "superposition-internal-token";
pub const DISPATCHER_USERNAME: &str = "kronos-dispatcher";

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
    let basic_credential = general_purpose::STANDARD
        .encode(format!("{DISPATCHER_USERNAME}:{dispatch_token}"));
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

pub fn job_dispatcher_endpoint_spec(dispatcher_url: &str) -> serde_json::Value {
    let timeout_ms: u64 = get_from_env_or_default("DISPATCHER_TIMEOUT_MS", 15000);
    json!({
        "url": dispatcher_url,
        "method": "POST",
        "headers": {
            "Authorization": format!("Basic {{{{secret.{DISPATCHER_SECRET_NAME}}}}}"),
            "x-org-id": "{{input.org_id}}",
            "x-workspace": "{{input.workspace}}"
        },
        "timeout_ms": timeout_ms,
        "expected_status_codes": [200]
    })
}

/// Register the generic job dispatcher endpoint in Kronos. Called alongside `setup_dispatcher`
/// during app startup. Reuses the same secret as the webhook dispatcher.
pub async fn setup_job_dispatcher(
    kronos_client: &dyn KronosClient,
    workspace: &str,
    superposition_host: &str,
) {
    let dispatcher_url = format!("{superposition_host}/dispatch/job");
    if let Err(e) = kronos_client
        .register_endpoint(
            workspace,
            JOB_DISPATCHER_ENDPOINT_NAME,
            "HTTP",
            job_dispatcher_endpoint_spec(&dispatcher_url),
            Some(dispatcher_retry_policy()),
        )
        .await
    {
        log::warn!(
            "Kronos job dispatcher: endpoint register failed for '{workspace}': {e}"
        );
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

/// Submit a background job to Kronos and track it in the BJM table.
///
/// 1. Generates a snowflake ID and inserts a `CREATED` entry into `superposition.job_manager`.
/// 2. Calls `kronos_client.create_job()` with the `JobRequest` as input.
/// 3. On success → updates BJM status to `SCHEDULED` and stores the Kronos job ID.
/// 4. On failure → updates BJM status to `FAILED` with the error in logs.
///
#[allow(clippy::too_many_arguments)]
pub async fn submit_job(
    kronos_client: &dyn KronosClient,
    target_workspace: &str,
    workspace: &JobWorkspace,
    org_id: &str,
    workspace_id: &str,
    job_request: &JobRequest,
    snowflake_generator: &Arc<Mutex<SnowflakeIdGenerator>>,
    conn: &mut DBConnection,
    max_attempts: i64,
    description: &str,
) -> anyhow::Result<JobCreateResponse> {
    let job_id = {
        let mut id_gen = snowflake_generator
            .lock()
            .map_err(|e| anyhow::anyhow!("snowflake lock failed: {e}"))?;
        id_gen.real_time_generate()
    };

    let job_type = job_request.job_type();
    let job_name = job_request.job_name();
    let schema_str = workspace.as_db_string();

    let bjm_entry = BackgroundJob {
        id: job_id,
        kronos_job_id: String::new(),
        description: description.to_string(),
        job_type,
        status: BackgroundJobStatus::Created,
        name: job_name.clone(),
        progress: 0,
        workspace_schema: workspace.clone(),
        created_at: Utc::now(),
        logs: json!({}),
    };

    diesel::insert_into(job_manager_dsl::job_manager)
        .values(&bjm_entry)
        .execute(conn)?;

    let job_request_value = serde_json::to_value(job_request)?;
    let mut input = job_request_value;
    if let Some(obj) = input.as_object_mut() {
        obj.insert("org_id".to_string(), json!(org_id));
        obj.insert("workspace".to_string(), json!(workspace_id));
        obj.insert("job_id".to_string(), json!(job_id.to_string()));
    }

    let idempotency_key = format!(
        "{}_{}_{}_{}",
        schema_str,
        job_name,
        job_type,
        Utc::now().timestamp_millis()
    );

    match kronos_client
        .create_job(
            target_workspace,
            JOB_DISPATCHER_ENDPOINT_NAME,
            input,
            max_attempts,
            JobTrigger::Immediate,
            Some(&idempotency_key),
        )
        .await
    {
        Ok(kronos_job_id) => {
            diesel::update(
                job_manager_dsl::job_manager.filter(job_manager_dsl::id.eq(job_id)),
            )
            .set((
                job_manager_dsl::kronos_job_id.eq(&kronos_job_id),
                job_manager_dsl::status.eq(BackgroundJobStatus::Scheduled),
            ))
            .execute(conn)?;

            Ok(JobCreateResponse {
                id: job_id,
                kronos_job_id,
                status: BackgroundJobStatus::Scheduled,
            })
        }
        Err(e) => {
            let error_msg = format!("Kronos job creation failed: {e}");
            log::error!("submit_job: {error_msg}");
            let mut log_map = serde_json::Map::new();
            log_map.insert(
                Utc::now().to_rfc3339(),
                serde_json::Value::String(error_msg),
            );
            let log_entry = serde_json::Value::Object(log_map);
            diesel::update(
                job_manager_dsl::job_manager.filter(job_manager_dsl::id.eq(job_id)),
            )
            .set((
                job_manager_dsl::status.eq(BackgroundJobStatus::Failed),
                job_manager_dsl::logs.eq(&log_entry),
            ))
            .execute(conn)?;
            Err(e)
        }
    }
}

pub fn get_job_by_id(
    conn: &mut DBConnection,
    workspace: &JobWorkspace,
    job_id: i64,
) -> anyhow::Result<WorkspaceJobView> {
    let schema = workspace.as_db_string();
    job_manager_view_dsl::job_manager
        .filter(job_manager_view_dsl::id.eq(job_id))
        .schema_name(&schema)
        .select(WorkspaceJobView::as_select())
        .first::<WorkspaceJobView>(conn)
        .map_err(|e| anyhow::anyhow!("Failed to fetch job {job_id}: {e}"))
}

pub fn list_jobs(
    conn: &mut DBConnection,
    schema: &SchemaName,
    filters: &JobListFilters,
    pagination: &PaginationParams,
) -> anyhow::Result<(i64, Vec<WorkspaceJobView>)> {
    let build_query = |f: &JobListFilters| {
        let mut query = job_manager_view_dsl::job_manager
            .schema_name(schema)
            .into_boxed();
        if let Some(jt) = f.job_type {
            query = query.filter(job_manager_view_dsl::job_type.eq(jt));
        }
        if let Some(st) = f.status {
            query = query.filter(job_manager_view_dsl::status.eq(st));
        }
        query
    };

    let base_query = build_query(filters);

    if let Some(true) = pagination.all {
        let data = base_query
            .order(job_manager_view_dsl::created_at.desc())
            .get_results(conn)?;
        return Ok((data.len() as i64, data));
    }

    let count_query = build_query(filters);

    let total_items: i64 = count_query.count().get_result(conn)?;

    let limit = pagination.count.unwrap_or(10);
    let mut paged_query = base_query
        .order(job_manager_view_dsl::created_at.desc())
        .limit(limit);
    if let Some(page) = pagination.page {
        let offset = (page - 1) * limit;
        paged_query = paged_query.offset(offset);
    }
    let data = paged_query.get_results(conn)?;

    Ok((total_items, data))
}

pub fn list_jobs_global(
    conn: &mut DBConnection,
    filters: &JobListFilters,
    pagination: &PaginationParams,
) -> anyhow::Result<(i64, Vec<BackgroundJob>)> {
    let build_query = |f: &JobListFilters| {
        let mut query = job_manager_dsl::job_manager.into_boxed();
        if let Some(jt) = f.job_type {
            query = query.filter(job_manager_dsl::job_type.eq(jt));
        }
        if let Some(st) = f.status {
            query = query.filter(job_manager_dsl::status.eq(st));
        }
        query
    };

    let base_query = build_query(filters);

    if let Some(true) = pagination.all {
        let data = base_query
            .order(job_manager_dsl::created_at.desc())
            .get_results(conn)?;
        return Ok((data.len() as i64, data));
    }

    let count_query = build_query(filters);

    let total_items: i64 = count_query.count().get_result(conn)?;

    let limit = pagination.count.unwrap_or(10);
    let mut paged_query = base_query
        .order(job_manager_dsl::created_at.desc())
        .limit(limit);
    if let Some(page) = pagination.page {
        let offset = (page - 1) * limit;
        paged_query = paged_query.offset(offset);
    }
    let data = paged_query.get_results(conn)?;

    Ok((total_items, data))
}

pub fn update_job_status(
    conn: &mut DBConnection,
    job_id: i64,
    status: BackgroundJobStatus,
) -> anyhow::Result<()> {
    diesel::update(job_manager_dsl::job_manager.filter(job_manager_dsl::id.eq(job_id)))
        .set(job_manager_dsl::status.eq(status))
        .execute(conn)?;
    Ok(())
}

pub fn update_job_progress(
    conn: &mut DBConnection,
    job_id: i64,
    progress: i32,
) -> anyhow::Result<()> {
    let threshold: i32 = get_from_env_or_default("JOB_PROGRESS_DIFF_UPDATE", 10);
    let previous_progress = job_manager_dsl::job_manager
        .filter(job_manager_dsl::id.eq(job_id))
        .select(job_manager_dsl::progress)
        .first::<i32>(conn)?;

    if progress == 100 || (progress - previous_progress).abs() > threshold {
        diesel::update(
            job_manager_dsl::job_manager.filter(job_manager_dsl::id.eq(job_id)),
        )
        .set(job_manager_dsl::progress.eq(progress))
        .execute(conn)?;
    }
    Ok(())
}

pub fn append_job_logs(
    conn: &mut DBConnection,
    job_id: i64,
    log_line: &str,
    key: Option<String>,
) -> anyhow::Result<String> {
    let current = job_manager_dsl::job_manager
        .filter(job_manager_dsl::id.eq(job_id))
        .select(job_manager_dsl::logs)
        .first::<serde_json::Value>(conn)?;

    let timed_key = key.unwrap_or(Utc::now().to_rfc3339());

    let mut logs = current.as_object().cloned().unwrap_or_default();
    logs.insert(
        timed_key.clone(),
        serde_json::Value::String(log_line.to_string()),
    );
    let new_logs = serde_json::Value::Object(logs);

    diesel::update(job_manager_dsl::job_manager.filter(job_manager_dsl::id.eq(job_id)))
        .set(job_manager_dsl::logs.eq(new_logs))
        .execute(conn)?;
    Ok(timed_key)
}
