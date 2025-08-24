// Individual tool modules
pub mod get_resolved_config;
pub mod get_default_config;
pub mod create_default_config;
pub mod update_default_config;
pub mod delete_default_config;
pub mod list_default_configs;
pub mod get_config_fast;
pub mod get_config;

// Re-export individual tools
pub use get_resolved_config::GetResolvedConfigTool;
pub use get_default_config::GetDefaultConfigTool;
pub use create_default_config::CreateDefaultConfigTool;
pub use update_default_config::UpdateDefaultConfigTool;
pub use delete_default_config::DeleteDefaultConfigTool;
pub use list_default_configs::ListDefaultConfigsTool;
pub use get_config_fast::GetConfigFastTool;
pub use get_config::GetConfigTool;

// Compose all config tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    ConfigTools,
    GetResolvedConfigTool,
    GetDefaultConfigTool,
    CreateDefaultConfigTool,
    UpdateDefaultConfigTool,
    DeleteDefaultConfigTool,
    ListDefaultConfigsTool,
    GetConfigFastTool,
    GetConfigTool
);
