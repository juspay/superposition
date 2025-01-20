use serde::{Deserialize, Serialize};
use superposition_types::database::models::WorkspaceStatus;

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateWorkspaceRequest {
    pub workspace_admin_email: String,
    pub workspace_name: String,
    pub workspace_status: Option<WorkspaceStatus>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct UpdateWorkspaceRequest {
    pub workspace_admin_email: String,
    pub workspace_status: Option<WorkspaceStatus>,
    pub mandatory_dimensions: Option<Vec<String>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RowData {
    pub workspace_name: String,
    pub workspace_schema_name: String,
    pub workspace_status: WorkspaceStatus,
    pub workspace_admin_email: String,
    pub mandatory_dimensions: Option<Vec<String>>,
    pub created_by: String,
    pub created_at: String,
}
