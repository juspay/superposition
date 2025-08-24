use crate::mcp_service::{document_to_value, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct GetConfigFastTool;

impl MCPTool for GetConfigFastTool {
    fn get_definition() -> Tool {
        Tool {
            name: "get_config_fast".to_string(),
            description: "Get configuration with fast access pattern".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "context": {"type": "object", "description": "Context for configuration resolution"},
                    "prefix": {"type": "string", "description": "Optional key prefix to filter configurations"}
                },
                "required": ["org_id", "workspace_id", "context"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let _context = &arguments["context"];
        let prefix = arguments["prefix"].as_str();

        let mut builder = service
            .superposition_client
            .get_config()
            .workspace_id(workspace_id)
            .org_id(org_id);

        if let Some(p) = prefix {
            builder = builder.prefix(p);
        }

        builder
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

    fn name() -> &'static str {
        "get_config_fast"
    }
}