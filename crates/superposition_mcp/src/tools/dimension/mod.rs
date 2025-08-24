// Individual tool modules
pub mod create_dimension;
pub mod get_dimension;
pub mod list_dimensions;
pub mod update_dimension;
pub mod delete_dimension;

// Re-export individual tools
pub use create_dimension::CreateDimensionTool;
pub use get_dimension::GetDimensionTool;
pub use list_dimensions::ListDimensionsTool;
pub use update_dimension::UpdateDimensionTool;
pub use delete_dimension::DeleteDimensionTool;

// Compose dimension tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    DimensionTools,
    CreateDimensionTool,
    GetDimensionTool,
    ListDimensionsTool,
    UpdateDimensionTool,
    DeleteDimensionTool
);