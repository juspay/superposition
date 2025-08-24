use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct GetContextTool;

impl MCPTool for GetContextTool {
    fn get_definition() -> Tool {
        Tool {
            name: "get_context".to_string(),
            description: "Get a specific context".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "context_id": {
                        "type": "string",
                        "description": "ID of the context to retrieve"
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
            .get_context()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .id(context_id)
            .send()
            .await
            .map(|output| {
                json!({
                    "id": output.id(),
                    "message": "Context found (detailed output serialization needs to be implemented)"
                })
            })
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "get_context"
    }
}
