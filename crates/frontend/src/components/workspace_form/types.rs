use serde::{Deserialize, Serialize};
use std::str::FromStr;
use strum_macros::Display;

#[derive(Debug, Serialize, Deserialize, Clone, Display, PartialEq)]
pub enum WorkspaceStatus {
    ENABLED,
    DISABLED,
}

impl FromStr for WorkspaceStatus {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "ENABLED" => Ok(WorkspaceStatus::ENABLED),
            "DISABLED" => Ok(WorkspaceStatus::DISABLED),
            _ => Err(format!("Invalid enum string: {}", s)),
        }
    }
}

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
