use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct DeleteContextTool;

impl MCPTool for DeleteContextTool {
    fn get_definition() -> Tool {
        Tool {
            name: "delete_context".to_string(),
            description: "Delete a context".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "context_id": {
                        "type": "string",
                        "description": "Context ID to delete"
                    }
                },
                "required": ["context_id"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let context_id = arguments["context_id"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .delete_context()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .id(context_id)
            .send()
            .await
            .map(|_| json!({"status": "deleted"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "delete_context"
    }
}
