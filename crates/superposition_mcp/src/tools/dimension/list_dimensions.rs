use crate::mcp_service::{document_to_value, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct ListDimensionsTool;

impl MCPTool for ListDimensionsTool {
    fn get_definition() -> Tool {
        Tool {
            name: "list_dimensions".to_string(),
            description: "List all dimensions".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"}
                },
                "required": ["org_id", "workspace_id"]
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
        
        let client = service.get_client(token);
        client
            .list_dimensions()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .send()
            .await
            .map(|output| {
                let dimensions: Vec<Value> = output.data().iter().map(|dim| {
                    json!({
                        "dimension": dim.dimension(),
                        "schema": document_to_value(dim.schema()),
                        "position": dim.position(),
                        "description": dim.description(),
                        "created_at": dim.created_at().to_string(),
                        "created_by": dim.created_by(),
                        "last_modified_at": dim.last_modified_at().to_string(),
                        "last_modified_by": dim.last_modified_by()
                    })
                }).collect();
                json!(dimensions)
            })
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "list_dimensions"
    }
}
