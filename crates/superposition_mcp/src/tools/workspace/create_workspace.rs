use crate::mcp_service::{value_to_document, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;
use superposition_sdk::types::WorkspaceStatus;

pub struct CreateWorkspaceTool;

impl MCPTool for CreateWorkspaceTool {
    fn get_definition() -> Tool {
        Tool {
            name: "create_workspace".to_string(),
            description: "Create a new workspace".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_name": {"type": "string", "description": "Workspace name"},
                    "workspace_admin_email": {"type": "string", "description": "Admin email for the workspace"},
                    "workspace_status": {"type": "string", "enum": ["ENABLED", "DISABLED"], "description": "Workspace status"},
                    "strict_mode": {"type": "boolean", "description": "Enable strict mode"},
                    "metrics": {"type": "object", "description": "Metrics configuration"},
                    "allow_experiment_self_approval": {"type": "boolean", "description": "Allow experiment self approval"}
                },
                "required": ["org_id", "workspace_name", "workspace_admin_email", "strict_mode", "allow_experiment_self_approval"]
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
        let strict_mode = arguments["strict_mode"].as_bool().unwrap_or(false);
        let allow_experiment_self_approval = arguments["allow_experiment_self_approval"]
            .as_bool()
            .unwrap_or(false);

        let client = service.get_client(token);
        let mut builder = client
            .create_workspace()
            .org_id(org_id)
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

    fn name() -> &'static str {
        "create_workspace"
    }
}
