use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct UpdateOverridesExperimentTool;

impl MCPTool for UpdateOverridesExperimentTool {
    fn get_definition() -> Tool {
        Tool {
            name: "update_overrides_experiment".to_string(),
            description: "Update overrides for an experiment".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "experiment_id": {"type": "string", "description": "Experiment ID"},
                    "overrides": {"type": "object", "description": "Override configuration"},
                    "change_reason": {"type": "string", "description": "Reason for updating overrides"}
                },
                "required": ["org_id", "workspace_id", "experiment_id", "overrides", "change_reason"]
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
        let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");
        
        let client = service.get_client(token);
        client
            .update_overrides_experiment()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .id(experiment_id)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "overrides_updated"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "update_overrides_experiment"
    }
}
