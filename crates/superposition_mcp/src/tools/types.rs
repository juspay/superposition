use serde_json::{json, Value};
use std::error::Error;
use crate::mcp_service::{Tool, McpService, value_to_document};
use super::ToolsModule;

pub struct TypeTools;

impl ToolsModule for TypeTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "create_type_templates".to_string(),
                description: "Create new type templates".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "type_name": {"type": "string", "description": "Type template name"},
                        "type_schema": {"type": "object", "description": "Type schema definition"},
                        "change_reason": {"type": "string", "description": "Reason for creating type template"}
                    },
                    "required": ["type_name", "type_schema", "change_reason"]
                }),
            },
            Tool {
                name: "get_type_templates_list".to_string(),
                description: "Get list of type templates".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "update_type_templates".to_string(),
                description: "Update existing type templates".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "type_name": {"type": "string", "description": "Type template name"},
                        "type_schema": {"type": "object", "description": "Updated type schema definition"},
                        "change_reason": {"type": "string", "description": "Reason for updating type template"}
                    },
                    "required": ["type_name", "change_reason"]
                }),
            },
            Tool {
                name: "delete_type_templates".to_string(),
                description: "Delete type templates".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "type_name": {"type": "string", "description": "Type template name"}
                    },
                    "required": ["type_name"]
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
            "create_type_templates" => {
                let type_name = arguments["type_name"].as_str().unwrap_or("");
                let type_schema = &arguments["type_schema"];
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");
                
                let type_schema_doc = value_to_document(type_schema);
                
                service
                    .superposition_client
                    .create_type_templates()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .type_name(type_name)
                    .type_schema(type_schema_doc)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "created"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_type_templates_list" => {
                service
                    .superposition_client
                    .get_type_templates_list()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Type templates found"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_type_templates" => {
                let type_name = arguments["type_name"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");
                
                let mut builder = service
                    .superposition_client
                    .update_type_templates()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .type_name(type_name)
                    .change_reason(change_reason);
                
                if arguments.get("type_schema").is_some() {
                    let type_schema_doc = value_to_document(&arguments["type_schema"]);
                    builder = builder.type_schema(type_schema_doc);
                }
                
                builder
                    .send()
                    .await
                    .map(|_| json!({"status": "updated"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "delete_type_templates" => {
                let type_name = arguments["type_name"].as_str().unwrap_or("");
                
                service
                    .superposition_client
                    .delete_type_templates()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .type_name(type_name)
                    .send()
                    .await
                    .map(|_| json!({"status": "deleted"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown type tool: {}", tool_name).into()),
        }
    }
}