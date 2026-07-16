use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::database::models::{
    BackgroundJob, BackgroundJobStatus, BackgroundJobType,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "job_type", content = "job_data")]
pub enum JobRequest {
    Webhook(DispatchWebhookRequest),
    PriorityRecompute(PriorityRecomputeRequest),
    Reduce(ReduceRequest),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DispatchWebhookRequest {
    pub webhook_name: String,
    pub data: Value,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PriorityRecomputeRequest {}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ReduceRequest {
    #[serde(default)]
    pub approve: bool,
}

impl JobRequest {
    pub fn job_type(&self) -> BackgroundJobType {
        match self {
            Self::Webhook(_) => BackgroundJobType::Webhook,
            Self::PriorityRecompute(_) => BackgroundJobType::PriorityRecompute,
            Self::Reduce(_) => BackgroundJobType::Reduce,
        }
    }

    pub fn job_name(&self) -> String {
        match self {
            Self::Webhook(r) => r.webhook_name.clone(),
            Self::PriorityRecompute(_) => "priority_recompute".to_string(),
            Self::Reduce(_) => "reduce".to_string(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobResponse {
    #[serde(with = "crate::database::models::i64_formatter")]
    pub id: i64,
    pub kronos_job_id: String,
    pub description: String,
    #[serde(rename = "type")]
    pub job_type: BackgroundJobType,
    pub status: BackgroundJobStatus,
    pub name: String,
    pub progress: i32,
    pub workspace_schema: String,
    pub created_at: DateTime<Utc>,
    pub logs: String,
}

impl From<BackgroundJob> for JobResponse {
    fn from(job: BackgroundJob) -> Self {
        Self {
            id: job.id,
            kronos_job_id: job.kronos_job_id,
            description: job.description,
            job_type: job.job_type,
            status: job.status,
            name: job.name,
            progress: job.progress,
            workspace_schema: job.workspace_schema,
            created_at: job.created_at,
            logs: job.logs,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobCreateResponse {
    #[serde(with = "crate::database::models::i64_formatter")]
    pub id: i64,
    pub kronos_job_id: String,
    pub status: BackgroundJobStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ExecutionDetails {
    pub attempt_count: Option<i64>,
    pub max_attempts: Option<i64>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
    pub duration_ms: Option<i64>,
    pub execution_status: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JobDetailResponse {
    #[serde(flatten)]
    pub job: JobResponse,
    pub execution: Option<ExecutionDetails>,
}

#[derive(Debug, Deserialize, Serialize, Default, Clone)]
pub struct JobListFilters {
    #[serde(default)]
    pub status: Option<BackgroundJobStatus>,
    #[serde(default)]
    pub job_type: Option<BackgroundJobType>,
}
