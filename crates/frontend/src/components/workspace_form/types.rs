use serde::{Deserialize, Serialize};
use superposition_types::database::models::{Metrics, WorkspaceStatus};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RowData {
    pub workspace_name: String,
    pub workspace_schema_name: String,
    pub workspace_status: WorkspaceStatus,
    pub workspace_admin_email: String,
    pub mandatory_dimensions: Option<Vec<String>>,
    pub created_by: String,
    pub created_at: String,
    pub metrics: Metrics,
}
