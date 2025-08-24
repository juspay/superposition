use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct CreateFunctionTool;

impl MCPTool for CreateFunctionTool {
    fn get_definition() -> Tool {
        Tool {
            name: "create_function".to_string(),
            description: "Create a new validation function".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "function_name": {"type": "string", "description": "Function name"},
                    "function": {"type": "string", "description": "Function code"},
                    "description": {"type": "string", "description": "Function description"},
                    "change_reason": {"type": "string", "description": "Reason for creating function"}
                },
                "required": ["org_id", "workspace_id", "function_name", "function", "description", "change_reason"]
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
        let function_code = arguments["function"].as_str().unwrap_or("");
        let description = arguments["description"].as_str().unwrap_or("");
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .create_function()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .function_name(function_name)
            .function(function_code)
            .description(description)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "created"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "create_function"
    }
}
