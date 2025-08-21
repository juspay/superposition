use serde_json::{json, Value};
use std::error::Error;
use crate::mcp_service::{Tool, McpService};
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
        _arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        match tool_name {
            "applicable_variants" => {
                // This operation has complex parameter requirements, placeholder for now
                Err(format!("applicable_variants operation needs SDK parameter mapping").into())
            }
            "bulk_operation" => {
                service
                    .superposition_client
                    .bulk_operation()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "bulk operation completed"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "publish" => {
                service
                    .superposition_client
                    .publish()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "published"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "test" => {
                service
                    .superposition_client
                    .test()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "test completed"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_override" => {
                service
                    .superposition_client
                    .update_override()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "override updated"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "weight_recompute" => {
                service
                    .superposition_client
                    .weight_recompute()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "weights recomputed"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown misc tool: {}", tool_name).into()),
        }
    }
}