// Individual tool modules
pub mod create_type_templates;
pub mod get_type_templates_list;
pub mod update_type_templates;
pub mod delete_type_templates;

// Re-export individual tools
pub use create_type_templates::CreateTypeTemplatesTool;
pub use get_type_templates_list::GetTypeTemplatesListTool;
pub use update_type_templates::UpdateTypeTemplatesTool;
pub use delete_type_templates::DeleteTypeTemplatesTool;

// Compose all type tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    TypeTools,
    CreateTypeTemplatesTool,
    GetTypeTemplatesListTool,
    UpdateTypeTemplatesTool,
    DeleteTypeTemplatesTool
);