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
                        "from_date": {"type": "string", "format": "date", "description": "Start date for audit logs"},
                        "to_date": {"type": "string", "format": "date", "description": "End date for audit logs"},
                        "filters": {"type": "object", "description": "Additional filters for audit logs"}
                    }
                }),
            },
            Tool {
                name: "list_versions".to_string(),
                description: "List configuration versions".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "key": {"type": "string", "description": "Configuration key to get versions for"}
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
            "list_audit_logs" => {
                service
                    .superposition_client
                    .list_audit_logs()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
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
                service
                    .superposition_client
                    .list_versions()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
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