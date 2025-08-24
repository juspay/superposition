use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct ListVersionsTool;

impl MCPTool for ListVersionsTool {
    fn get_definition() -> Tool {
        Tool {
            name: "list_versions".to_string(),
            description: "List configuration versions".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "key": {"type": "string", "description": "Configuration key to get versions for"}
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
            .list_versions()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .send()
            .await
            .map(|output| {
                json!({
                    "count": output.data().len(),
                    "message": "Versions found"
                })
            })
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "list_versions"
    }
}