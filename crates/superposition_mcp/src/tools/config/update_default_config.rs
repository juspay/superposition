use crate::mcp_service::{value_to_document, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct UpdateDefaultConfigTool;

impl MCPTool for UpdateDefaultConfigTool {
    fn get_definition() -> Tool {
        Tool {
            name: "update_default_config".to_string(),
            description: "Update an existing default configuration".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "key": {"type": "string", "description": "Configuration key to update"},
                    "value": {"description": "New default value"},
                    "change_reason": {"type": "string", "description": "Reason for updating this configuration"}
                },
                "required": ["org_id", "workspace_id", "key", "value", "change_reason"]
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
        let key = arguments["key"].as_str().unwrap_or("");
        let value = arguments["value"].clone();
        let change_reason = arguments["change_reason"]
            .as_str()
            .unwrap_or("Updated via MCP server");

        let value_doc = value_to_document(&value);

        let client = service.get_client(token);
        client
            .update_default_config()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .key(key)
            .value(value_doc)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "updated", "key": key}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "update_default_config"
    }
}
