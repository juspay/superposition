use crate::mcp_service::{value_to_hashmap, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct CreateWebhookTool;

impl MCPTool for CreateWebhookTool {
    fn get_definition() -> Tool {
        Tool {
            name: "create_webhook".to_string(),
            description: "Create a new webhook".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "url": {"type": "string", "description": "Webhook URL"},
                    "headers": {"type": "object", "description": "Webhook headers"},
                    "events": {"type": "array", "items": {"type": "string"}, "description": "Event types"},
                    "change_reason": {"type": "string", "description": "Reason for creating webhook"}
                },
                "required": ["org_id", "workspace_id", "url", "headers", "events", "change_reason"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let url = arguments["url"].as_str().unwrap_or("");
        let headers = &arguments["headers"];
        let empty_vec = vec![];
        let events = arguments["events"].as_array().unwrap_or(&empty_vec);
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");
        
        let event_strings: Vec<String> = events
            .iter()
            .filter_map(|v| v.as_str())
            .map(|s| s.to_string())
            .collect();
        
        // Convert headers to hashmap
        let headers_hashmap = value_to_hashmap(headers.clone());
        
        service
            .superposition_client
            .create_webhook()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .url(url)
            .set_custom_headers(headers_hashmap)
            .set_events(Some(event_strings))
            .change_reason(change_reason)
            .send()
            .await
            .map(|_| json!({"status": "created"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "create_webhook"
    }
}