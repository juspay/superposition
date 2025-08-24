use crate::mcp_service::{value_to_document, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct CreateDimensionTool;

impl MCPTool for CreateDimensionTool {
    fn get_definition() -> Tool {
        Tool {
            name: "create_dimension".to_string(),
            description: "Create a new dimension".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "dimension": {
                        "type": "string",
                        "description": "Dimension name"
                    },
                    "schema": {
                        "type": "object",
                        "description": "JSON schema for the dimension"
                    },
                    "position": {
                        "type": "integer",
                        "description": "Priority for the dimension"
                    },
                    "description": {
                        "type": "string",
                        "description": "Description of the dimension"
                    },
                    "change_reason": {
                        "type": "string",
                        "description": "Reason for creating this dimension"
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
        let position64 = arguments["position"].as_i64().unwrap_or(1);
        let position = position64.try_into().unwrap();

        let schema_doc = value_to_document(&schema);

        service
            .superposition_client
            .create_dimension()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .dimension(dimension)
            .position(position)
            .schema(schema_doc)
            .description(description)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "created", "dimension": dimension}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "create_dimension"
    }
}