use serde_json::{json, Value};
use std::error::Error;
use crate::mcp_service::{Tool, McpService};
use super::ToolsModule;

pub struct FunctionTools;

impl ToolsModule for FunctionTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "list_function".to_string(),
                description: "List all custom validation functions".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "create_function".to_string(),
                description: "Create a new validation function".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "function_name": {"type": "string", "description": "Function name"},
                        "function": {"type": "string", "description": "Function code"},
                        "description": {"type": "string", "description": "Function description"},
                        "change_reason": {"type": "string", "description": "Reason for creating function"}
                    },
                    "required": ["function_name", "function", "description", "change_reason"]
                }),
            },
            Tool {
                name: "get_function".to_string(),
                description: "Get a function by name".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "function_name": {"type": "string", "description": "Function name"}
                    },
                    "required": ["function_name"]
                }),
            },
            Tool {
                name: "update_function".to_string(),
                description: "Update an existing function".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "function_name": {"type": "string", "description": "Function name"},
                        "function": {"type": "string", "description": "Updated function code"},
                        "description": {"type": "string", "description": "Updated function description"},
                        "change_reason": {"type": "string", "description": "Reason for updating function"}
                    },
                    "required": ["function_name", "change_reason"]
                }),
            },
            Tool {
                name: "delete_function".to_string(),
                description: "Delete a function".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "function_name": {"type": "string", "description": "Function name"}
                    },
                    "required": ["function_name"]
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
            "list_function" => {
                service
                    .superposition_client
                    .list_function()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Functions found (detailed data not available due to serialization constraints)"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "create_function" => {
                let function_name = arguments["function_name"].as_str().unwrap_or("");
                let function_code = arguments["function"].as_str().unwrap_or("");
                let description = arguments["description"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");
                
                service
                    .superposition_client
                    .create_function()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .function_name(function_name)
                    .function(function_code)
                    .description(description)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "created"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_function" => {
                let function_name = arguments["function_name"].as_str().unwrap_or("");
                
                service
                    .superposition_client
                    .get_function()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .function_name(function_name)
                    .send()
                    .await
                    .map(|_| json!({"status": "found"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_function" => {
                let function_name = arguments["function_name"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");
                
                let mut builder = service
                    .superposition_client
                    .update_function()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .function_name(function_name)
                    .change_reason(change_reason);
                
                if let Some(function_code) = arguments["function"].as_str() {
                    builder = builder.function(function_code);
                }
                if let Some(description) = arguments["description"].as_str() {
                    builder = builder.description(description);
                }
                
                builder
                    .send()
                    .await
                    .map(|_| json!({"status": "updated"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "delete_function" => {
                let function_name = arguments["function_name"].as_str().unwrap_or("");
                
                service
                    .superposition_client
                    .delete_function()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .function_name(function_name)
                    .send()
                    .await
                    .map(|_| json!({"status": "deleted"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown function tool: {}", tool_name).into()),
        }
    }
}