// Individual tool modules
pub mod list_function;
pub mod create_function;
pub mod get_function;
pub mod update_function;
pub mod delete_function;

// Re-export individual tools
pub use list_function::ListFunctionTool;
pub use create_function::CreateFunctionTool;
pub use get_function::GetFunctionTool;
pub use update_function::UpdateFunctionTool;
pub use delete_function::DeleteFunctionTool;

// Compose function tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    FunctionTools,
    ListFunctionTool,
    CreateFunctionTool,
    GetFunctionTool,
    UpdateFunctionTool,
    DeleteFunctionTool
);
