use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct GetWebhookTool;

impl MCPTool for GetWebhookTool {
    fn get_definition() -> Tool {
        Tool {
            name: "get_webhook".to_string(),
            description: "Get a webhook by ID".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "webhook_id": {"type": "string", "description": "Webhook ID"}
                },
                "required": ["org_id", "workspace_id", "webhook_id"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let webhook_id = arguments["webhook_id"].as_str().unwrap_or("");
        service
            .superposition_client
            .get_webhook()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .name(webhook_id)
            .send()
            .await
            .map(|_| json!({"status": "found"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "get_webhook"
    }
}
