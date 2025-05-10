#[cfg(feature = "diesel_derives")]
use diesel::AsChangeset;
use serde::{Deserialize, Serialize};

use crate::database::models::{Metrics, WorkspaceStatus};
#[cfg(feature = "diesel_derives")]
use crate::database::superposition_schema::superposition::workspaces;

#[derive(Debug, Deserialize, Serialize)]
pub struct CreateWorkspaceRequest {
    pub workspace_admin_email: String,
    pub workspace_name: String,
    pub workspace_status: Option<WorkspaceStatus>,
    pub workspace_strict_mode: bool,
    pub metrics: Option<Metrics>,
}

#[derive(Debug, Deserialize, Serialize)]
#[cfg_attr(feature = "diesel_derives", derive(AsChangeset))]
#[cfg_attr(feature = "diesel_derives", diesel(table_name = workspaces))]
pub struct UpdateWorkspaceRequest {
    pub workspace_admin_email: String,
    pub workspace_status: Option<WorkspaceStatus>,
    pub mandatory_dimensions: Option<Vec<String>>,
    pub metrics: Option<Metrics>,
}

#[derive(Deserialize, Debug)]
pub struct WorkspaceListFilters {
    pub workspace_name: Option<String>,
}
