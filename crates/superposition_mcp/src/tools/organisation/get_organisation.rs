use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct GetOrganisationTool;

impl MCPTool for GetOrganisationTool {
    fn get_definition() -> Tool {
        Tool {
            name: "get_organisation".to_string(),
            description: "Get organisation details".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "id": {"type": "string", "description": "Organisation ID"}
                },
                "required": ["id"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let id = arguments["id"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .get_organisation()
            .id(id)
            .send()
            .await
            .map(|_| json!({"status": "found"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "get_organisation"
    }
}