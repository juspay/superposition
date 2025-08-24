// Individual tool modules
pub mod create_context;
pub mod get_context;
pub mod list_contexts;
pub mod delete_context;
pub mod get_context_from_condition;
pub mod move_context;

// Re-export individual tools
pub use create_context::CreateContextTool;
pub use get_context::GetContextTool;
pub use list_contexts::ListContextsTool;
pub use delete_context::DeleteContextTool;
pub use get_context_from_condition::GetContextFromConditionTool;
pub use move_context::MoveContextTool;

// Compose context tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    ContextTools,
    CreateContextTool,
    GetContextTool,
    ListContextsTool,
    DeleteContextTool,
    GetContextFromConditionTool,
    MoveContextTool
);