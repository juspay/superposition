use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct DeleteDefaultConfigTool;

impl MCPTool for DeleteDefaultConfigTool {
    fn get_definition() -> Tool {
        Tool {
            name: "delete_default_config".to_string(),
            description: "Delete a default configuration".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "key": {"type": "string", "description": "Configuration key to delete"}
                },
                "required": ["org_id", "workspace_id", "key"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let key = arguments["key"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .delete_default_config()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .key(key)
            .send()
            .await
            .map(|_| json!({"status": "deleted", "key": key}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "delete_default_config"
    }
}