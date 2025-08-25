use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct ListWebhookTool;

impl MCPTool for ListWebhookTool {
    fn get_definition() -> Tool {
        Tool {
            name: "list_webhook".to_string(),
            description: "List all webhooks".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"}
                },
                "required": ["org_id", "workspace_id"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
        token: Option<&str>,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let client = service.get_client(token);
        client
            .list_webhook()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .send()
            .await
            .map(|output| {
                json!({
                    "count": output.data().len(),
                    "message": "Webhooks found"
                })
            })
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "list_webhook"
    }
}
