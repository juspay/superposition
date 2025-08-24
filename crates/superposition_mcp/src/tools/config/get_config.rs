use crate::mcp_service::{document_to_value, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct GetConfigTool;

impl MCPTool for GetConfigTool {
    fn get_definition() -> Tool {
        Tool {
            name: "get_config".to_string(),
            description: "Get configuration for a specific key or all configs".to_string(),
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
                .get_config()
                .workspace_id(workspace_id)
                .org_id(org_id)
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
        }
    }

    fn name() -> &'static str {
        "get_config"
    }
}
