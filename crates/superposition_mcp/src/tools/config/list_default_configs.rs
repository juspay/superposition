use crate::mcp_service::{document_to_value, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct ListDefaultConfigsTool;

impl MCPTool for ListDefaultConfigsTool {
    fn get_definition() -> Tool {
        Tool {
            name: "list_default_configs".to_string(),
            description: "List all default configurations".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "all": {"type": "boolean", "description": "Whether to include all configurations", "default": true}
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
        let all = arguments["all"].as_bool().unwrap_or(true);
        
        service
            .superposition_client
            .list_default_configs()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .all(all)
            .send()
            .await
            .map(|output| {
                let configs: Vec<Value> = output
                    .data()
                    .iter()
                    .map(|config| {
                        json!({
                            "key": config.key(),
                            "value": document_to_value(config.value()),
                            "schema": document_to_value(config.schema()),
                            "description": config.description(),
                            "change_reason": config.change_reason(),
                            "function_name": config.function_name(),
                            "created_at": config.created_at().to_string(),
                            "created_by": config.created_by(),
                            "last_modified_at": config.last_modified_at().to_string(),
                            "last_modified_by": config.last_modified_by()
                        })
                    })
                    .collect();
                json!(configs)
            })
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "list_default_configs"
    }
}