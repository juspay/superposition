use crate::mcp_service::{document_to_value, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct GetDefaultConfigTool;

impl MCPTool for GetDefaultConfigTool {
    fn get_definition() -> Tool {
        Tool {
            name: "get_default_config".to_string(),
            description: "Get all default configurations or a specific config by key".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "key": {"type": "string", "description": "Optional specific configuration key to retrieve"}
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
        let key = arguments["key"].as_str();
        
        if let Some(k) = key {
            service
                .superposition_client
                .get_config()
                .workspace_id(workspace_id)
                .org_id(org_id)
                .prefix(k)
                .send()
                .await
                .map(|output| {
                    if let Some(default_configs) = output.default_configs() {
                        let mut result = serde_json::Map::new();
                        for (key, doc) in default_configs {
                            result.insert(key.clone(), document_to_value(doc));
                        }
                        Value::Object(result)
                    } else {
                        Value::Null
                    }
                })
                .map_err(|e| format!("SDK error: {}", e).into())
        } else {
            service
                .superposition_client
                .list_default_configs()
                .workspace_id(workspace_id)
                .org_id(org_id)
                .all(true)
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
    }

    fn name() -> &'static str {
        "get_default_config"
    }
}
