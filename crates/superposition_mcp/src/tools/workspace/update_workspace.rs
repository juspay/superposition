use crate::mcp_service::{value_to_document, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;
use superposition_sdk::types::WorkspaceStatus;

pub struct UpdateWorkspaceTool;

impl MCPTool for UpdateWorkspaceTool {
    fn get_definition() -> Tool {
        Tool {
            name: "update_workspace".to_string(),
            description: "Update a workspace".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_name": {"type": "string", "description": "Workspace name to update"},
                    "workspace_admin_email": {"type": "string", "description": "Updated admin email"},
                    "config_version": {"type": "string", "description": "Configuration version"},
                    "mandatory_dimensions": {"type": "array", "items": {"type": "string"}, "description": "Mandatory dimensions"},
                    "workspace_status": {"type": "string", "enum": ["ENABLED", "DISABLED"], "description": "Updated workspace status"},
                    "metrics": {"type": "object", "description": "Updated metrics configuration"},
                    "allow_experiment_self_approval": {"type": "boolean", "description": "Allow experiment self approval"},
                    "auto_populate_control": {"type": "boolean", "description": "Auto populate control variant"}
                },
                "required": ["org_id", "workspace_name", "workspace_admin_email"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
        token: Option<&str>,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_name = arguments["workspace_name"].as_str().unwrap_or("");
        let workspace_admin_email = arguments["workspace_admin_email"].as_str().unwrap_or("");

        let client = service.get_client(token);
        let mut builder = client
            .update_workspace()
            .org_id(org_id)
            .workspace_name(workspace_name)
            .workspace_admin_email(workspace_admin_email);

        // Optional config version
        if let Some(config_version) = arguments["config_version"].as_str() {
            builder = builder.config_version(config_version);
        }

        // Optional mandatory dimensions
        if let Some(dimensions_array) = arguments["mandatory_dimensions"].as_array() {
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
        if let Some(allow_self_approval) = arguments["allow_experiment_self_approval"].as_bool() {
            builder = builder.allow_experiment_self_approval(allow_self_approval);
        }

        builder
            .send()
            .await
            .map(|_| json!({"status": "updated"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "update_workspace"
    }
}
