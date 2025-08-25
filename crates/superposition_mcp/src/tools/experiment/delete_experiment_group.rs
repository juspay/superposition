use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct DeleteExperimentGroupTool;

impl MCPTool for DeleteExperimentGroupTool {
    fn get_definition() -> Tool {
        Tool {
            name: "delete_experiment_group".to_string(),
            description: "Delete an experiment group".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "group_id": {"type": "string", "description": "Group ID"}
                },
                "required": ["org_id", "workspace_id", "group_id"]
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
        
        let client = service.get_client(token);
        client
            .delete_experiment_group()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .id(group_id)
            .send()
            .await
            .map(|_| json!({"status": "deleted"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "delete_experiment_group"
    }
}
