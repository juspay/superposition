use actix_web::{
    HttpResponse, Scope, get, post,
    web::{Data, Json, Path, Query},
};
use service_utils::{
    kronos_dispatch::{append_job_logs, get_job_by_id, list_jobs, update_job_status},
    service::types::{AppState, DbConnection, WorkspaceContext},
};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::unexpected_error;
use superposition_types::{
    api::jobs::{ExecutionDetails, JobDetailResponse, JobListFilters, JobResponse},
    database::models::{BackgroundJobStatus, JobWorkspace, others::WorkspaceJobView},
    result as superposition,
};

declare_resource!(WorkspaceJob);

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(list_handler)
        .service(get_handler)
        .service(cancel_handler)
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    filters: Query<JobListFilters>,
) -> superposition::Result<Json<Vec<WorkspaceJobView>>> {
    let DbConnection(mut conn) = db_conn;
    let filters = filters.into_inner();
    let job_workspace = JobWorkspace::from(&workspace_context.schema_name.0);

    let jobs = list_jobs(&mut conn, &job_workspace, filters.job_type, filters.status)
        .map_err(|e| unexpected_error!("Failed to list jobs: {}", e))?;

    Ok(Json(jobs))
}

#[authorized]
#[get("/{job_id}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    db_conn: DbConnection,
    job_id: Path<String>,
) -> superposition::Result<Json<JobDetailResponse>> {
    let DbConnection(mut conn) = db_conn;
    let job_id_str = job_id.into_inner();
    let job_id: i64 = job_id_str
        .parse()
        .map_err(|e| unexpected_error!("Invalid job_id '{}': {}", job_id_str, e))?;

    let job_workspace = JobWorkspace::from(&workspace_context.schema_name.0);
    let job = get_job_by_id(&mut conn, &job_workspace, job_id)
        .map_err(|e| unexpected_error!("Failed to fetch job: {}", e))?;

    let target_workspace = state
        .kronos_workspace
        .as_deref()
        .unwrap_or(&workspace_context.schema_name);

    let execution = match state
        .kronos_client
        .get_execution(target_workspace, &job.kronos_job_id)
        .await
    {
        Ok(Some(exec)) => Some(ExecutionDetails {
            attempt_count: Some(exec.attempt_count),
            max_attempts: Some(exec.max_attempts),
            started_at: exec.started_at,
            completed_at: exec.completed_at,
            duration_ms: exec.duration_ms,
            execution_status: Some(exec.status),
        }),
        Ok(None) => None,
        Err(e) => {
            log::warn!("Failed to fetch Kronos execution details: {}", e);
            None
        }
    };
    let schema_name = workspace_context.schema_name.0;
    Ok(Json(JobDetailResponse {
        job: JobResponse::from_view(&job, &schema_name),
        execution,
    }))
}

#[authorized]
#[post("/{job_id}/cancel")]
async fn cancel_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    db_conn: DbConnection,
    job_id: Path<String>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let job_id_str = job_id.into_inner();
    let job_id: i64 = job_id_str
        .parse()
        .map_err(|e| unexpected_error!("Invalid job_id '{}': {}", job_id_str, e))?;

    let job_workspace = JobWorkspace::from(&workspace_context.schema_name.0);
    let job = get_job_by_id(&mut conn, &job_workspace, job_id)
        .map_err(|e| unexpected_error!("Failed to fetch job: {}", e))?;

    if matches!(
        job.status,
        BackgroundJobStatus::Completed | BackgroundJobStatus::Failed
    ) {
        return Err(unexpected_error!(
            "Cannot cancel job {} with terminal status {}",
            job_id,
            job.status
        ));
    }

    let target_workspace = state
        .kronos_workspace
        .clone()
        .unwrap_or_else(|| workspace_context.schema_name.to_string());

    if let Err(e) = state
        .kronos_client
        .cancel_job(&target_workspace, &job.kronos_job_id)
        .await
    {
        append_job_logs(&mut conn, job_id, &format!("Cancel attempt failed: {e}"))
            .map_err(|e| unexpected_error!("Failed to append logs: {}", e))?;
        return Err(unexpected_error!("Failed to cancel Kronos job: {}", e));
    }

    update_job_status(&mut conn, job_id, BackgroundJobStatus::Failed)
        .map_err(|e| unexpected_error!("Failed to update job status: {}", e))?;

    append_job_logs(&mut conn, job_id, "Job cancelled by user")
        .map_err(|e| unexpected_error!("Failed to append logs: {}", e))?;

    Ok(HttpResponse::Ok().finish())
}
