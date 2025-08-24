use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct RampExperimentTool;

impl MCPTool for RampExperimentTool {
    fn get_definition() -> Tool {
        Tool {
            name: "ramp_experiment".to_string(),
            description: "Ramp an experiment".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "experiment_id": {"type": "string", "description": "Experiment ID"},
                    "ramp_percentage": {"type": "number", "description": "Ramp percentage"},
                    "change_reason": {"type": "string", "description": "Reason for ramping experiment"}
                },
                "required": ["org_id", "workspace_id", "experiment_id", "ramp_percentage", "change_reason"]
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
        let ramp_percentage = arguments["ramp_percentage"].as_f64().unwrap_or(0.0) as i32;
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .ramp_experiment()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .id(experiment_id)
            .traffic_percentage(ramp_percentage)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "ramped"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "ramp_experiment"
    }
}