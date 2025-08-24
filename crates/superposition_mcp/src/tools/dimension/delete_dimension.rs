use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct DeleteDimensionTool;

impl MCPTool for DeleteDimensionTool {
    fn get_definition() -> Tool {
        Tool {
            name: "delete_dimension".to_string(),
            description: "Delete a dimension".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "dimension": {
                        "type": "string",
                        "description": "Name of the dimension to delete"
                    }
                },
                "required": ["org_id", "workspace_id", "dimension"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let dimension = arguments["dimension"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .delete_dimension()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .dimension(dimension)
            .send()
            .await
            .map(|_| json!({"status": "deleted", "dimension": dimension}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "delete_dimension"
    }
}
