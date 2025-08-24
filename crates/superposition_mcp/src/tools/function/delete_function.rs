use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct DeleteFunctionTool;

impl MCPTool for DeleteFunctionTool {
    fn get_definition() -> Tool {
        Tool {
            name: "delete_function".to_string(),
            description: "Delete a function".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "function_name": {"type": "string", "description": "Function name"}
                },
                "required": ["org_id", "workspace_id", "function_name"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let function_name = arguments["function_name"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .delete_function()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .function_name(function_name)
            .send()
            .await
            .map(|_| json!({"status": "deleted"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "delete_function"
    }
}