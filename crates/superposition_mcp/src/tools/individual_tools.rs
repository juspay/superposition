// Example implementation of individual MCPTool implementations
// This shows how to structure individual tools using the new MCPTool trait

use super::MCPTool;
use crate::mcp_service::{document_to_value, value_to_document, McpService, Tool};
use serde_json::{json, Value};
use std::error::Error;
use superposition_sdk::types::MergeStrategy;

/// Example: GetResolvedConfig as an individual MCPTool
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

        let mut builder = service
            .superposition_client
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

/// Example: CreateDefaultConfig as an individual MCPTool
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