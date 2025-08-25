use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct UpdateFunctionTool;

impl MCPTool for UpdateFunctionTool {
    fn get_definition() -> Tool {
        Tool {
            name: "update_function".to_string(),
            description: "Update an existing function".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "function_name": {"type": "string", "description": "Function name"},
                    "function": {"type": "string", "description": "Updated function code"},
                    "description": {"type": "string", "description": "Updated function description"},
                    "change_reason": {"type": "string", "description": "Reason for updating function"}
                },
                "required": ["org_id", "workspace_id", "function_name", "change_reason"]
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
        let function_name = arguments["function_name"].as_str().unwrap_or("");
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");
        
        let client = service.get_client(token);
        let mut builder = client
            .update_function()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .function_name(function_name)
            .change_reason(change_reason);
        
        if let Some(function_code) = arguments["function"].as_str() {
            builder = builder.function(function_code);
        }
        if let Some(description) = arguments["description"].as_str() {
            builder = builder.description(description);
        }
        
        builder
            .send()
            .await
            .map(|_| json!({"status": "updated"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "update_function"
    }
}
