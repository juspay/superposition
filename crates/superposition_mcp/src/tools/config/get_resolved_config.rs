use crate::mcp_service::{document_to_value, value_to_document, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;
use superposition_sdk::types::MergeStrategy;

pub struct GetResolvedConfigTool;

impl MCPTool for GetResolvedConfigTool {
    fn get_definition() -> Tool {
        Tool {
            name: "get_resolved_config".to_string(),
            description: "Get resolved configuration based on context using Superposition CAC".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "context": {
                        "type": "object",
                        "description": "Context object containing dimensions for configuration resolution"
                    },
                    "prefix": {
                        "type": "string",
                        "description": "Optional key prefix to filter configurations"
                    },
                    "merge_strategy": {
                        "type": "string",
                        "enum": ["MERGE", "REPLACE"],
                        "default": "MERGE",
                        "description": "Strategy for merging configurations"
                    }
                },
                "required": ["org_id", "workspace_id", "context"]
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
        let default_context = serde_json::Map::new();
        let context = arguments["context"].as_object().unwrap_or(&default_context);
        let prefix = arguments["prefix"].as_str();

        let strategy = match arguments["merge_strategy"].as_str().unwrap_or("MERGE") {
            "REPLACE" => MergeStrategy::Replace,
            _ => MergeStrategy::Merge,
        };

        let client = service.get_client(token);
        let mut builder = client
            .get_resolved_config()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .merge_strategy(strategy);

        if let Some(p) = prefix {
            builder = builder.prefix(p);
        }

        for (key, value) in context {
            let doc = value_to_document(value);
            builder = builder.context(key, doc);
        }

        builder
            .send()
            .await
            .map(|output| {
                output
                    .config()
                    .map(|c| document_to_value(c))
                    .unwrap_or(Value::Null)
            })
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "get_resolved_config"
    }
}
