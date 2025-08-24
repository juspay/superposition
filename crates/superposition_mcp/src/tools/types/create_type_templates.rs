use crate::mcp_service::{value_to_document, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct CreateTypeTemplatesTool;

impl MCPTool for CreateTypeTemplatesTool {
    fn get_definition() -> Tool {
        Tool {
            name: "create_type_templates".to_string(),
            description: "Create new type templates".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "type_name": {"type": "string", "description": "Type template name"},
                    "type_schema": {"type": "object", "description": "Type schema definition"},
                    "change_reason": {"type": "string", "description": "Reason for creating type template"}
                },
                "required": ["org_id", "workspace_id", "type_name", "type_schema", "change_reason"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let type_name = arguments["type_name"].as_str().unwrap_or("");
        let type_schema = &arguments["type_schema"];
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");
        
        let type_schema_doc = value_to_document(type_schema);
        
        service
            .superposition_client
            .create_type_templates()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .type_name(type_name)
            .type_schema(type_schema_doc)
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "created"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "create_type_templates"
    }
}