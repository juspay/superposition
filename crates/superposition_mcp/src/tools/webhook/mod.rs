// Individual tool modules
pub mod create_webhook;
pub mod get_webhook;
pub mod list_webhook;
pub mod update_webhook;

// Re-export individual tools
pub use create_webhook::CreateWebhookTool;
pub use get_webhook::GetWebhookTool;
pub use list_webhook::ListWebhookTool;
pub use update_webhook::UpdateWebhookTool;

// Compose all webhook tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    WebhookTools,
    CreateWebhookTool,
    GetWebhookTool,
    ListWebhookTool,
    UpdateWebhookTool
);