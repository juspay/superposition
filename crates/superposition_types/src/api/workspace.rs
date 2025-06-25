use crate::database::models::{Metrics, Workspace, WorkspaceStatus};
#[cfg(feature = "diesel_derives")]
use crate::database::superposition_schema::superposition::workspaces;
use chrono::{DateTime, Utc};
#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Serialize};

use super::{deserialize_option_i64, I64Update};

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct WorkspaceResponse {
    pub organisation_id: String,
    pub organisation_name: String,
    pub workspace_name: String,
    pub workspace_schema_name: String,
    pub workspace_status: WorkspaceStatus,
    pub workspace_admin_email: String,
    pub config_version: Option<String>,
    pub created_by: String,
    pub last_modified_by: String,
    pub last_modified_at: DateTime<Utc>,
    pub created_at: DateTime<Utc>,
    pub mandatory_dimensions: Option<Vec<String>>,
    pub strict_mode: bool,
    pub metrics: Metrics,
    pub allow_experiment_self_approval: bool,
}

impl From<Workspace> for WorkspaceResponse {
    fn from(workspace: Workspace) -> Self {
        Self {
            organisation_id: workspace.organisation_id,
            organisation_name: workspace.organisation_name,
            workspace_name: workspace.workspace_name,
            workspace_schema_name: workspace.workspace_schema_name,
            workspace_status: workspace.workspace_status,
            workspace_admin_email: workspace.workspace_admin_email,
            config_version: workspace.config_version.map(|v| v.to_string()),
            created_by: workspace.created_by,
            last_modified_by: workspace.last_modified_by,
            last_modified_at: workspace.last_modified_at,
            created_at: workspace.created_at,
            mandatory_dimensions: workspace.mandatory_dimensions,
            strict_mode: workspace.strict_mode,
            metrics: workspace.metrics,
            allow_experiment_self_approval: workspace.allow_experiment_self_approval,
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct CreateWorkspaceRequest {
    pub workspace_admin_email: String,
    pub workspace_name: String,
    pub workspace_status: Option<WorkspaceStatus>,
    #[serde(alias = "workspace_strict_mode")]
    pub strict_mode: bool,
    pub metrics: Option<Metrics>,
    pub allow_experiment_self_approval: bool,
}

#[derive(Debug, Deserialize, Serialize)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = workspaces))]
pub struct UpdateWorkspaceRequest {
    pub workspace_admin_email: String,
    pub workspace_status: Option<WorkspaceStatus>,
    pub mandatory_dimensions: Option<Vec<String>>,
    #[serde(default, deserialize_with = "deserialize_option_i64")]
    pub config_version: Option<I64Update>,
    pub metrics: Option<Metrics>,
    pub allow_experiment_self_approval: Option<bool>,
}

#[derive(Deserialize, Debug)]
pub struct WorkspaceListFilters {
    pub workspace_name: Option<String>,
}
