use serde_json::{json, Value};
use std::error::Error;
use crate::mcp_service::{Tool, McpService};
use super::ToolsModule;

pub struct AuditTools;

impl ToolsModule for AuditTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "list_audit_logs".to_string(),
                description: "List audit logs".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "org_id": {"type": "string", "description": "Organization ID"},
                        "workspace_id": {"type": "string", "description": "Workspace ID"},
                        "from_date": {"type": "string", "format": "date", "description": "Start date for audit logs"},
                        "to_date": {"type": "string", "format": "date", "description": "End date for audit logs"},
                        "filters": {"type": "object", "description": "Additional filters for audit logs"}
                    },
                    "required": ["org_id", "workspace_id"]
                }),
            },
            Tool {
                name: "list_versions".to_string(),
                description: "List configuration versions".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "org_id": {"type": "string", "description": "Organization ID"},
                        "workspace_id": {"type": "string", "description": "Workspace ID"},
                        "key": {"type": "string", "description": "Configuration key to get versions for"}
                    },
                    "required": ["org_id", "workspace_id"]
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
            "list_audit_logs" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
                let builder = service
                    .superposition_client
                    .list_audit_logs()
                    .workspace_id(workspace_id)
                    .org_id(org_id);
                
                // Note: from_date and to_date would need DateTime conversion, 
                // and filters would need proper format - keeping simple for now
                if let Some(_from_date) = arguments["from_date"].as_str() {
                    // DateTime conversion would be needed here
                }
                if let Some(_to_date) = arguments["to_date"].as_str() {
                    // DateTime conversion would be needed here  
                }
                if arguments.get("filters").is_some() {
                    // Filter processing would be needed here
                }
                
                builder
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Audit logs found"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_versions" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
                service
                    .superposition_client
                    .list_versions()
                    .workspace_id(workspace_id)
                    .org_id(org_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Versions found"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown audit tool: {}", tool_name).into()),
        }
    }
}