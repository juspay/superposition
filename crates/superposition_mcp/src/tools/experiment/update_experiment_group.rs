use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct UpdateExperimentGroupTool;

impl MCPTool for UpdateExperimentGroupTool {
    fn get_definition() -> Tool {
        Tool {
            name: "update_experiment_group".to_string(),
            description: "Update an experiment group".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "group_id": {"type": "string", "description": "Group ID"},
                    "name": {"type": "string", "description": "Updated group name"},
                    "description": {"type": "string", "description": "Updated group description"},
                    "change_reason": {"type": "string", "description": "Reason for updating group"}
                },
                "required": ["org_id", "workspace_id", "group_id", "change_reason"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
        token: Option<&str>,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let group_id = arguments["group_id"].as_str().unwrap_or("");
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");

        let client = service.get_client(token);
        client
            .update_experiment_group()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .id(group_id)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "updated"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "update_experiment_group"
    }
}
