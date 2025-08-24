// Individual tool modules
pub mod create_workspace;
pub mod list_workspace;
pub mod update_workspace;

// Re-export individual tools
pub use create_workspace::CreateWorkspaceTool;
pub use list_workspace::ListWorkspaceTool;
pub use update_workspace::UpdateWorkspaceTool;

// Compose workspace tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    WorkspaceTools,
    CreateWorkspaceTool,
    ListWorkspaceTool,
    UpdateWorkspaceTool
);