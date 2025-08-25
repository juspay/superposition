use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct DeleteTypeTemplatesTool;

impl MCPTool for DeleteTypeTemplatesTool {
    fn get_definition() -> Tool {
        Tool {
            name: "delete_type_templates".to_string(),
            description: "Delete type templates".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "type_name": {"type": "string", "description": "Type template name"}
                },
                "required": ["org_id", "workspace_id", "type_name"]
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
        let type_name = arguments["type_name"].as_str().unwrap_or("");
        
        let client = service.get_client(token);
        client
            .delete_type_templates()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .type_name(type_name)
            .send()
            .await
            .map(|_| json!({"status": "deleted"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "delete_type_templates"
    }
}
