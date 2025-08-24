use crate::mcp_service::{document_to_value, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct GetDimensionTool;

impl MCPTool for GetDimensionTool {
    fn get_definition() -> Tool {
        Tool {
            name: "get_dimension".to_string(),
            description: "Get a specific dimension".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "dimension": {
                        "type": "string",
                        "description": "Name of the dimension to retrieve"
                    }
                },
                "required": ["org_id", "workspace_id", "dimension"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let dimension = arguments["dimension"].as_str().unwrap_or("");
        
        service
            .superposition_client
            .get_dimension()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .dimension(dimension)
            .send()
            .await
            .map(|output| {
                json!({
                    "dimension": output.dimension(),
                    "schema": document_to_value(output.schema()),
                    "position": output.position(),
                    "description": output.description(),
                    "created_at": output.created_at().to_string(),
                    "created_by": output.created_by(),
                    "last_modified_at": output.last_modified_at().to_string(),
                    "last_modified_by": output.last_modified_by()
                })
            })
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "get_dimension"
    }
}
