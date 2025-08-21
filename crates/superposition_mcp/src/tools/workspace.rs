use serde_json::{json, Value};
use std::error::Error;
use crate::mcp_service::{Tool, McpService};
use super::ToolsModule;

pub struct WorkspaceTools;

impl ToolsModule for WorkspaceTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "create_workspace".to_string(),
                description: "Create a new workspace".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "workspace_name": {"type": "string", "description": "Workspace name"},
                        "description": {"type": "string", "description": "Workspace description"},
                        "change_reason": {"type": "string", "description": "Reason for creating workspace"}
                    },
                    "required": ["workspace_name", "description", "change_reason"]
                }),
            },
            Tool {
                name: "list_workspace".to_string(),
                description: "List all workspaces".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "update_workspace".to_string(),
                description: "Update a workspace".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "workspace_id": {"type": "string", "description": "Workspace ID"},
                        "workspace_name": {"type": "string", "description": "Updated workspace name"},
                        "description": {"type": "string", "description": "Updated workspace description"},
                        "change_reason": {"type": "string", "description": "Reason for updating workspace"}
                    },
                    "required": ["workspace_id", "change_reason"]
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
            "create_workspace" => {
                let workspace_name = arguments["workspace_name"].as_str().unwrap_or("");
                let _change_reason = arguments["change_reason"].as_str().unwrap_or("");
                
                service
                    .superposition_client
                    .create_workspace()
                    .org_id(&service.org_id)
                    .workspace_name(workspace_name)
                    .send()
                    .await
                    .map(|_| json!({"status": "created"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_workspace" => {
                service
                    .superposition_client
                    .list_workspace()
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Workspaces found"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_workspace" => {
                let _change_reason = arguments["change_reason"].as_str().unwrap_or("");
                
                service
                    .superposition_client
                    .update_workspace()
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "updated"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown workspace tool: {}", tool_name).into()),
        }
    }
}