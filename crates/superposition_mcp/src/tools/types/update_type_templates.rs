use crate::mcp_service::{value_to_document, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct UpdateTypeTemplatesTool;

impl MCPTool for UpdateTypeTemplatesTool {
    fn get_definition() -> Tool {
        Tool {
            name: "update_type_templates".to_string(),
            description: "Update existing type templates".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "type_name": {"type": "string", "description": "Type template name"},
                    "type_schema": {"type": "object", "description": "Updated type schema definition"},
                    "change_reason": {"type": "string", "description": "Reason for updating type template"}
                },
                "required": ["org_id", "workspace_id", "type_name", "change_reason"]
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
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");
        
        let mut builder = service
            .superposition_client
            .update_type_templates()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .type_name(type_name)
            .change_reason(change_reason);
        
        if arguments.get("type_schema").is_some() {
            let type_schema_doc = value_to_document(&arguments["type_schema"]);
            builder = builder.type_schema(type_schema_doc);
        }
        
        builder
            .send()
            .await
            .map(|_| json!({"status": "updated"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "update_type_templates"
    }
}
