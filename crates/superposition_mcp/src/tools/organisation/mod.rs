// Individual tool modules
pub mod create_organisation;
pub mod get_organisation;
pub mod list_organisation;
pub mod update_organisation;

// Re-export individual tools
pub use create_organisation::CreateOrganisationTool;
pub use get_organisation::GetOrganisationTool;
pub use list_organisation::ListOrganisationTool;
pub use update_organisation::UpdateOrganisationTool;

// Compose organisation tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    OrganisationTools,
    CreateOrganisationTool,
    GetOrganisationTool,
    ListOrganisationTool,
    UpdateOrganisationTool
);