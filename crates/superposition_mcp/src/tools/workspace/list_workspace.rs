use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct ListWorkspaceTool;

impl MCPTool for ListWorkspaceTool {
    fn get_definition() -> Tool {
        Tool {
            name: "list_workspace".to_string(),
            description: "List all workspaces".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "count": {"type": "integer", "description": "Number of items per page"},
                    "page": {"type": "integer", "description": "Page number"},
                    "all": {"type": "boolean", "description": "Fetch all results"}
                },
                "required": ["org_id"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
        token: Option<&str>,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let client = service.get_client(token);
        let mut builder = client
            .list_workspace()
            .org_id(org_id);

        // Optional pagination parameters
        if let Some(count) = arguments["count"].as_i64() {
            builder = builder.count(count as i32);
        }
        if let Some(page) = arguments["page"].as_i64() {
            builder = builder.page(page as i32);
        }
        if let Some(all) = arguments["all"].as_bool() {
            builder = builder.all(all);
        }

        builder
            .send()
            .await
            .map(|output| {
                json!({
                    "count": output.data().len(),
                    "message": "Workspaces found"
                })
            })
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "list_workspace"
    }
}
