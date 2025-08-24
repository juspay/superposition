use crate::mcp_service::{value_to_hashmap, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct CreateContextTool;

impl MCPTool for CreateContextTool {
    fn get_definition() -> Tool {
        Tool {
            name: "create_context".to_string(),
            description: "Create a new context with conditions".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "context_id": {
                        "type": "string",
                        "description": "Unique identifier for the context"
                    },
                    "condition": {
                        "type": "object",
                        "description": "Condition expression for the context"
                    },
                    "override_with_keys": {
                        "type": "object",
                        "description": "List of configuration keys that can be overridden in this context"
                    },
                    "description": {
                        "type": "string",
                        "description": "Description of the context"
                    },
                    "change_reason": {
                        "type": "string",
                        "description": "Reason for creating this context"
                    }
                },
                "required": ["org_id", "workspace_id", "context_id", "condition", "override_with_keys", "description", "change_reason"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let _context_id = arguments["context_id"].as_str().unwrap_or("");
        let condition = arguments["condition"].clone();
        let condition_doc = value_to_hashmap(condition);
        let override_with_keys = value_to_hashmap(arguments["override_with_keys"].clone());
        let description = arguments["description"].as_str().unwrap_or("no description");
        let change_reason = arguments["change_reason"].as_str().unwrap_or("no change_reason");

        service
            .superposition_client
            .create_context()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .description(description)
            .change_reason(change_reason)
            .set_context(condition_doc)
            .set_override(override_with_keys)
            .send()
            .await
            .map(|_| json!({"status": "created context"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "create_context"
    }
}