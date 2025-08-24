use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct PublishTool;

impl MCPTool for PublishTool {
    fn get_definition() -> Tool {
        Tool {
            name: "publish".to_string(),
            description: "Publish configuration changes".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "comment": {"type": "string", "description": "Comment for the publish operation"}
                },
                "required": ["org_id", "workspace_id"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let mut builder = service
            .superposition_client
            .publish()
            .workspace_id(workspace_id)
            .org_id(org_id);
        
        if let Some(comment) = arguments["comment"].as_str() {
            builder = builder.change_reason(comment);
        }
        
        builder
            .send()
            .await
            .map(|_| json!({"status": "published"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "publish"
    }
}