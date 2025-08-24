use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct ListFunctionTool;

impl MCPTool for ListFunctionTool {
    fn get_definition() -> Tool {
        Tool {
            name: "list_function".to_string(),
            description: "List all custom validation functions".to_string(),
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
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .list_function()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .send()
            .await
            .map(|output| {
                json!({
                    "count": output.data().len(),
                    "message": "Functions found (detailed data not available due to serialization constraints)"
                })
            })
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "list_function"
    }
}
