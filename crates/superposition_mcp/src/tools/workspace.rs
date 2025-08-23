use super::ToolsModule;
use crate::mcp_service::{value_to_document, McpService, Tool};
use serde_json::{json, Value};
use std::error::Error;
use superposition_sdk::types::WorkspaceStatus;

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
                        "workspace_admin_email": {"type": "string", "description": "Admin email for the workspace"},
                        "workspace_status": {"type": "string", "enum": ["ENABLED", "DISABLED"], "description": "Workspace status"},
                        "strict_mode": {"type": "boolean", "description": "Enable strict mode"},
                        "metrics": {"type": "object", "description": "Metrics configuration"},
                        "allow_experiment_self_approval": {"type": "boolean", "description": "Allow experiment self approval"}
                    },
                    "required": ["workspace_name", "workspace_admin_email", "strict_mode", "allow_experiment_self_approval"]
                }),
            },
            Tool {
                name: "list_workspace".to_string(),
                description: "List all workspaces".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "count": {"type": "integer", "description": "Number of items per page"},
                        "page": {"type": "integer", "description": "Page number"},
                        "all": {"type": "boolean", "description": "Fetch all results"}
                    }
                }),
            },
            Tool {
                name: "update_workspace".to_string(),
                description: "Update a workspace".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "workspace_name": {"type": "string", "description": "Workspace name to update"},
                        "workspace_admin_email": {"type": "string", "description": "Updated admin email"},
                        "config_version": {"type": "string", "description": "Configuration version"},
                        "mandatory_dimensions": {"type": "array", "items": {"type": "string"}, "description": "Mandatory dimensions"},
                        "workspace_status": {"type": "string", "enum": ["ENABLED", "DISABLED"], "description": "Updated workspace status"},
                        "metrics": {"type": "object", "description": "Updated metrics configuration"},
                        "allow_experiment_self_approval": {"type": "boolean", "description": "Allow experiment self approval"},
                        "auto_populate_control": {"type": "boolean", "description": "Auto populate control variant"}
                    },
                    "required": ["workspace_name", "workspace_admin_email"]
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
                let workspace_admin_email =
                    arguments["workspace_admin_email"].as_str().unwrap_or("");
                let strict_mode = arguments["strict_mode"].as_bool().unwrap_or(false);
                let allow_experiment_self_approval = arguments
                    ["allow_experiment_self_approval"]
                    .as_bool()
                    .unwrap_or(false);

                let mut builder = service
                    .superposition_client
                    .create_workspace()
                    .org_id(&service.org_id)
                    .workspace_name(workspace_name)
                    .workspace_admin_email(workspace_admin_email)
                    .strict_mode(strict_mode)
                    .allow_experiment_self_approval(allow_experiment_self_approval);

                // Optional workspace status
                if let Some(status_str) = arguments["workspace_status"].as_str() {
                    let status = match status_str.to_uppercase().as_str() {
                        "ENABLED" => WorkspaceStatus::Enabled,
                        "DISABLED" => WorkspaceStatus::Disabled,
                        _ => WorkspaceStatus::Enabled, // Default fallback
                    };
                    builder = builder.workspace_status(status);
                }

                // Optional metrics configuration
                if let Some(metrics_value) = arguments.get("metrics") {
                    let metrics_doc = value_to_document(metrics_value);
                    builder = builder.metrics(metrics_doc);
                }

                builder
                    .send()
                    .await
                    .map(|_| json!({"status": "created"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_workspace" => {
                let mut builder = service
                    .superposition_client
                    .list_workspace()
                    .org_id(&service.org_id);

                // Optional pagination parameters
                if let Some(count) = arguments["count"].as_i64() {
                    builder = builder.count(count as i32);
                }
                if let Some(page) = arguments["page"].as_i64() {
                    builder = builder.page(page as i32);
                }
                if let Some(all) = arguments["all"].as_bool() {
                    builder = builder.all(all);
                }

                builder
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
                let workspace_name = arguments["workspace_name"].as_str().unwrap_or("");
                let workspace_admin_email =
                    arguments["workspace_admin_email"].as_str().unwrap_or("");

                let mut builder = service
                    .superposition_client
                    .update_workspace()
                    .org_id(&service.org_id)
                    .workspace_name(workspace_name)
                    .workspace_admin_email(workspace_admin_email);

                // Optional config version
                if let Some(config_version) = arguments["config_version"].as_str() {
                    builder = builder.config_version(config_version);
                }

                // Optional mandatory dimensions
                if let Some(dimensions_array) =
                    arguments["mandatory_dimensions"].as_array()
                {
                    for dimension in dimensions_array {
                        if let Some(dim_str) = dimension.as_str() {
                            builder = builder.mandatory_dimensions(dim_str);
                        }
                    }
                }

                // Optional workspace status
                if let Some(status_str) = arguments["workspace_status"].as_str() {
                    let status = match status_str.to_uppercase().as_str() {
                        "ENABLED" => WorkspaceStatus::Enabled,
                        "DISABLED" => WorkspaceStatus::Disabled,
                        _ => WorkspaceStatus::Enabled, // Default fallback
                    };
                    builder = builder.workspace_status(status);
                }

                // Optional metrics configuration
                if let Some(metrics_value) = arguments.get("metrics") {
                    let metrics_doc = value_to_document(metrics_value);
                    builder = builder.metrics(metrics_doc);
                }

                // Optional boolean flags
                if let Some(allow_self_approval) =
                    arguments["allow_experiment_self_approval"].as_bool()
                {
                    builder = builder.allow_experiment_self_approval(allow_self_approval);
                }

                builder
                    .send()
                    .await
                    .map(|_| json!({"status": "updated"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown workspace tool: {}", tool_name).into()),
        }
    }
}
