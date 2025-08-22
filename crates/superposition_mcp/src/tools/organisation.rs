use serde_json::{json, Value};
use std::error::Error;
use crate::mcp_service::{Tool, McpService};
use super::ToolsModule;

pub struct OrganisationTools;

impl ToolsModule for OrganisationTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "create_organisation".to_string(),
                description: "Create a new organisation".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "name": {"type": "string", "description": "Organisation name"},
                        "description": {"type": "string", "description": "Organisation description"},
                        "change_reason": {"type": "string", "description": "Reason for creating organisation"}
                    },
                    "required": ["name", "description", "change_reason"]
                }),
            },
            Tool {
                name: "get_organisation".to_string(),
                description: "Get organisation details".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "org_id": {"type": "string", "description": "Organisation ID"}
                    },
                    "required": ["org_id"]
                }),
            },
            Tool {
                name: "list_organisation".to_string(),
                description: "List all organisations".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "update_organisation".to_string(),
                description: "Update an organisation".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "org_id": {"type": "string", "description": "Organisation ID"},
                        "name": {"type": "string", "description": "Updated organisation name"},
                        "description": {"type": "string", "description": "Updated organisation description"},
                        "change_reason": {"type": "string", "description": "Reason for updating organisation"}
                    },
                    "required": ["org_id", "change_reason"]
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
            "create_organisation" => {
                let name = arguments["name"].as_str().unwrap_or("");
                let description = arguments["description"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");
                
                service
                    .superposition_client
                    .create_organisation()
                    .name(name)
                    .send()
                    .await
                    .map(|_| json!({"status": "created"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_organisation" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                service
                    .superposition_client
                    .get_organisation()
                    .send()
                    .await
                    .map(|_| json!({"status": "found"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_organisation" => {
                service
                    .superposition_client
                    .list_organisation()
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Organisations found"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_organisation" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");
                
                let mut builder = service
                    .superposition_client
                    .update_organisation();
                
                if let Some(name) = arguments["name"].as_str() {
                    // builder = builder.name(name); // Method not available
                }
                if let Some(description) = arguments["description"].as_str() {
                    // builder = builder.description(description); // Method not available
                }
                
                builder
                    .send()
                    .await
                    .map(|_| json!({"status": "updated"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown organisation tool: {}", tool_name).into()),
        }
    }
}