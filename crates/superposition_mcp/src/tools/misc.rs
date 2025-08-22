use serde_json::{json, Value};
use std::error::Error;
use crate::mcp_service::{Tool, McpService, value_to_hashmap};
use super::ToolsModule;

pub struct MiscTools;

impl ToolsModule for MiscTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "applicable_variants".to_string(),
                description: "Get applicable variants for a context".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context": {"type": "object", "description": "Context to check variants for"}
                    },
                    "required": ["context"]
                }),
            },
            Tool {
                name: "bulk_operation".to_string(),
                description: "Perform bulk operations on configurations".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "operations": {"type": "array", "description": "List of operations to perform"},
                        "change_reason": {"type": "string", "description": "Reason for bulk operation"}
                    },
                    "required": ["operations", "change_reason"]
                }),
            },
            Tool {
                name: "publish".to_string(),
                description: "Publish configuration changes".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "comment": {"type": "string", "description": "Comment for the publish operation"}
                    }
                }),
            },
            Tool {
                name: "test".to_string(),
                description: "Test configuration changes".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "test_cases": {"type": "array", "description": "Test cases to execute"}
                    },
                    "required": ["test_cases"]
                }),
            },
            Tool {
                name: "update_override".to_string(),
                description: "Update configuration override".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context_id": {"type": "string", "description": "Context ID"},
                        "key": {"type": "string", "description": "Configuration key"},
                        "value": {"description": "Override value"},
                        "change_reason": {"type": "string", "description": "Reason for override update"}
                    },
                    "required": ["context_id", "key", "value", "change_reason"]
                }),
            },
            Tool {
                name: "weight_recompute".to_string(),
                description: "Recompute weights for configurations".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "config_keys": {"type": "array", "items": {"type": "string"}, "description": "Configuration keys to recompute weights for"}
                    }
                }),
            },
        ]
    }

    async fn execute_tool(
        service: &McpService,
        tool_name: &str,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        match tool_name {
            "applicable_variants" => {
                let context = &arguments["context"];
                let context_hashmap = if let Some(context_map) = value_to_hashmap(context.clone()) {
                    context_map
                } else {
                    return Err("Invalid context format".into());
                };
                
                service
                    .superposition_client
                    .applicable_variants()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .set_context(Some(context_hashmap))
                    .send()
                    .await
                    .map(|_| json!({"status": "variants found"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "bulk_operation" => {
                // Complex type conversion needed for BulkOperationReq, placeholder for now
                Err(format!("bulk_operation requires complex BulkOperationReq type conversion").into())
            }
            "publish" => {
                let mut builder = service
                    .superposition_client
                    .publish()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id);
                
                if let Some(comment) = arguments["comment"].as_str() {
                    builder = builder.change_reason(comment);
                }
                
                builder
                    .send()
                    .await
                    .map(|_| json!({"status": "published"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "test" => {
                // Complex type conversion needed for FunctionExecutionRequest, placeholder for now
                Err(format!("test requires complex FunctionExecutionRequest type conversion").into())
            }
            "update_override" => {
                // Complex type conversion needed for UpdateContextOverrideRequest, placeholder for now
                Err(format!("update_override requires complex UpdateContextOverrideRequest type conversion").into())
            }
            "weight_recompute" => {
                let mut builder = service
                    .superposition_client
                    .weight_recompute()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id);
                
                if let Some(config_keys_array) = arguments["config_keys"].as_array() {
                    let config_keys: Vec<String> = config_keys_array
                        .iter()
                        .filter_map(|v| v.as_str())
                        .map(|s| s.to_string())
                        .collect();
                    let config_keys_str = config_keys.join(",");
                    builder = builder.config_tags(config_keys_str);
                }
                
                builder
                    .send()
                    .await
                    .map(|_| json!({"status": "weights recomputed"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown misc tool: {}", tool_name).into()),
        }
    }
}