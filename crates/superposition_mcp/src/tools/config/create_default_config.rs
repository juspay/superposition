use crate::mcp_service::{value_to_document, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct CreateDefaultConfigTool;

impl MCPTool for CreateDefaultConfigTool {
    fn get_definition() -> Tool {
        Tool {
            name: "create_default_config".to_string(),
            description: "Create a new default configuration".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "key": {"type": "string", "description": "Configuration key name"},
                    "value": {"description": "Default value for the configuration"},
                    "schema": {"type": "object", "description": "JSON schema for validating the configuration value"},
                    "description": {"type": "string", "description": "Description of the key"},
                    "change_reason": {"type": "string", "description": "Reason for adding this key"}
                },
                "required": ["org_id", "workspace_id", "key", "value", "schema", "description", "change_reason"]
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
        let value = arguments["value"].clone();
        let schema = arguments["schema"].clone();
        let description = arguments["description"]
            .as_str()
            .unwrap_or("no description");
        let change_reason = arguments["change_reason"]
            .as_str()
            .unwrap_or("no change_reason");

        let value_doc = value_to_document(&value);
        let schema_doc = value_to_document(&schema);

        service
            .superposition_client
            .create_default_config()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .key(key)
            .value(value_doc)
            .schema(schema_doc)
            .description(description)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "created", "key": key}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "create_default_config"
    }
}
