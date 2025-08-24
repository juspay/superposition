use crate::mcp_service::{value_to_document, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct UpdateDimensionTool;

impl MCPTool for UpdateDimensionTool {
    fn get_definition() -> Tool {
        Tool {
            name: "update_dimension".to_string(),
            description: "Update an existing dimension".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "dimension": {
                        "type": "string",
                        "description": "Name of the dimension to update"
                    },
                    "schema": {
                        "type": "object",
                        "description": "Updated JSON schema for the dimension"
                    },
                    "description": {
                        "type": "string",
                        "description": "Updated description of the dimension"
                    },
                    "change_reason": {
                        "type": "string",
                        "description": "Reason for updating this dimension"
                    }
                },
                "required": ["org_id", "workspace_id", "dimension", "schema", "description", "change_reason"]
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
        let schema = arguments["schema"].clone();
        let description = arguments["description"].as_str().unwrap_or("no description");
        let change_reason = arguments["change_reason"].as_str().unwrap_or("no change_reason");

        let schema_doc = value_to_document(&schema);

        service
            .superposition_client
            .update_dimension()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .dimension(dimension)
            .schema(schema_doc)
            .description(description)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "updated", "dimension": dimension}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "update_dimension"
    }
}
