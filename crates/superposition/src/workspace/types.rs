use serde::Deserialize;

#[derive(Deserialize, Debug)]
pub struct WorkspaceListFilters {
    pub workspace_name: Option<String>,
}
