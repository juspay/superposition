use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct CreateExperimentGroupTool;

impl MCPTool for CreateExperimentGroupTool {
    fn get_definition() -> Tool {
        Tool {
            name: "create_experiment_group".to_string(),
            description: "Create an experiment group".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "name": {"type": "string", "description": "Group name"},
                    "description": {"type": "string", "description": "Group description"},
                    "change_reason": {"type": "string", "description": "Reason for creating group"}
                },
                "required": ["org_id", "workspace_id", "name", "description", "change_reason"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let name = arguments["name"].as_str().unwrap_or("");
        let description = arguments["description"].as_str().unwrap_or("");
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .create_experiment_group()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .name(name)
            .description(description)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "created"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "create_experiment_group"
    }
}
