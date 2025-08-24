use crate::mcp_service::{value_to_hashmap, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct ApplicableVariantsTool;

impl MCPTool for ApplicableVariantsTool {
    fn get_definition() -> Tool {
        Tool {
            name: "applicable_variants".to_string(),
            description: "Get applicable variants for a context".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "context": {"type": "object", "description": "Context to check variants for"}
                },
                "required": ["org_id", "workspace_id", "context"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let context = &arguments["context"];
        let context_hashmap = if let Some(context_map) = value_to_hashmap(context.clone()) {
            context_map
        } else {
            return Err("Invalid context format".into());
        };
        
        service
            .superposition_client
            .applicable_variants()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .set_context(Some(context_hashmap))
            .send()
            .await
            .map(|_| json!({"status": "variants found"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "applicable_variants"
    }
}