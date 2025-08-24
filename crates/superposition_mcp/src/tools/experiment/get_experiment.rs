use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct GetExperimentTool;

impl MCPTool for GetExperimentTool {
    fn get_definition() -> Tool {
        Tool {
            name: "get_experiment".to_string(),
            description: "Get an experiment by ID".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "experiment_id": {"type": "string", "description": "Experiment ID"}
                },
                "required": ["org_id", "workspace_id", "experiment_id"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .get_experiment()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .id(experiment_id)
            .send()
            .await
            .map(|_| json!({"status": "found"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "get_experiment"
    }
}