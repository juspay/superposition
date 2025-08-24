use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct ResumeExperimentTool;

impl MCPTool for ResumeExperimentTool {
    fn get_definition() -> Tool {
        Tool {
            name: "resume_experiment".to_string(),
            description: "Resume an experiment".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "experiment_id": {"type": "string", "description": "Experiment ID"},
                    "change_reason": {"type": "string", "description": "Reason for resuming experiment"}
                },
                "required": ["org_id", "workspace_id", "experiment_id", "change_reason"]
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
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .resume_experiment()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .id(experiment_id)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "resumed"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "resume_experiment"
    }
}
