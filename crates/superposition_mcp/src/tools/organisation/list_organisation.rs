use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct ListOrganisationTool;

impl MCPTool for ListOrganisationTool {
    fn get_definition() -> Tool {
        Tool {
            name: "list_organisation".to_string(),
            description: "List all organisations".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "count": {"type": "integer", "description": "Number of items per page"},
                    "page": {"type": "integer", "description": "Page number"},
                    "all": {"type": "boolean", "description": "Fetch all results"}
                }
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let mut builder = service
            .superposition_client
            .list_organisation();

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
                    "message": "Organisations found"
                })
            })
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "list_organisation"
    }
}
