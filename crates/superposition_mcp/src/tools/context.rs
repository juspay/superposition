use serde_json::{json, Value};
use std::error::Error;
use crate::mcp_service::{Tool, McpService, value_to_hashmap};
use super::ToolsModule;

pub struct ContextTools;

impl ToolsModule for ContextTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "create_context".to_string(),
                description: "Create a new context with conditions".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context_id": {
                            "type": "string",
                            "description": "Unique identifier for the context"
                        },
                        "condition": {
                            "type": "object",
                            "description": "Condition expression for the context"
                        },
                        "override_with_keys": {
                            "type": "object",
                            "description": "List of configuration keys that can be overridden in this context"
                        },
                        "description": {
                            "type": "string",
                            "description": "Description of the context"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for creating this context"
                        }
                    },
                    "required": ["context_id", "condition", "override_with_keys", "description", "change_reason"]
                }),
            },
            Tool {
                name: "get_context".to_string(),
                description: "Get a specific context".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context_id": {
                            "type": "string",
                            "description": "ID of the context to retrieve"
                        }
                    },
                    "required": ["context_id"]
                }),
            },
            Tool {
                name: "list_contexts".to_string(),
                description: "List all contexts".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "delete_context".to_string(),
                description: "Delete a context".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context_id": {
                            "type": "string",
                            "description": "Context ID to delete"
                        }
                    },
                    "required": ["context_id"]
                }),
            },
            Tool {
                name: "get_context_from_condition".to_string(),
                description: "Get contexts based on condition matching".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "condition": {
                            "type": "object",
                            "description": "Condition to match contexts against"
                        }
                    },
                    "required": ["condition"]
                }),
            },
            Tool {
                name: "move_context".to_string(),
                description: "Move a context to a different position".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context_id": {
                            "type": "string",
                            "description": "ID of the context to move"
                        },
                        "dest_position": {
                            "type": "integer",
                            "description": "Destination position for the context"
                        }
                    },
                    "required": ["context_id", "dest_position"]
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
            "create_context" => {
                let _context_id = arguments["context_id"].as_str().unwrap_or("");
                let condition = arguments["condition"].clone();
                let condition_doc = value_to_hashmap(condition);
                let override_with_keys = value_to_hashmap(arguments["override_with_keys"].clone());
                let description = arguments["description"].as_str().unwrap_or("no description");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("no change_reason");

                service
                    .superposition_client
                    .create_context()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .description(description)
                    .change_reason(change_reason)
                    .set_context(condition_doc)
                    .set_override(override_with_keys)
                    .send()
                    .await
                    .map(|_| json!({"status": "created context"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_context" => {
                let context_id = arguments["context_id"].as_str().unwrap_or("");
                service
                    .superposition_client
                    .get_context()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(context_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "id": output.id(),
                            "message": "Context found (detailed output serialization needs to be implemented)"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_contexts" => {
                service
                    .superposition_client
                    .list_contexts()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Contexts found (detailed data not available due to serialization constraints)"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "delete_context" => {
                let context_id = arguments["context_id"].as_str().unwrap_or("");
                service
                    .superposition_client
                    .delete_context()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .id(context_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "deleted"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_context_from_condition" => {
                // This operation might not be available in the current SDK
                // Return a placeholder response
                Err(format!("get_context_from_condition operation is not available in current SDK version").into())
            }
            "move_context" => {
                Err(format!("move_context implementation needs parameter verification").into())
            }
            _ => Err(format!("Unknown context tool: {}", tool_name).into()),
        }
    }
}