use crate::mcp_service::{value_to_hashmap, McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct UpdateWebhookTool;

impl MCPTool for UpdateWebhookTool {
    fn get_definition() -> Tool {
        Tool {
            name: "update_webhook".to_string(),
            description: "Update a webhook".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "org_id": {"type": "string", "description": "Organization ID"},
                    "workspace_id": {"type": "string", "description": "Workspace ID"},
                    "webhook_id": {"type": "string", "description": "Webhook ID"},
                    "url": {"type": "string", "description": "Updated webhook URL"},
                    "headers": {"type": "object", "description": "Updated webhook headers"},
                    "events": {"type": "array", "items": {"type": "string"}, "description": "Updated event types"},
                    "change_reason": {"type": "string", "description": "Reason for updating webhook"}
                },
                "required": ["org_id", "workspace_id", "webhook_id", "change_reason"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let org_id = arguments["org_id"].as_str().unwrap_or("");
        let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
        let webhook_id = arguments["webhook_id"].as_str().unwrap_or("");
        let change_reason = arguments["change_reason"].as_str().unwrap_or("");
        
        let mut builder = service
            .superposition_client
            .update_webhook()
            .workspace_id(workspace_id)
            .org_id(org_id)
            .name(webhook_id)
            .change_reason(change_reason);
        
        if let Some(url) = arguments["url"].as_str() {
            builder = builder.url(url);
        }
        if let Some(headers) = arguments.get("headers") {
            let headers_hashmap = value_to_hashmap(headers.clone());
            builder = builder.set_custom_headers(headers_hashmap);
        }
        if let Some(events_array) = arguments["events"].as_array() {
            let event_strings: Vec<String> = events_array
                .iter()
                .filter_map(|v| v.as_str())
                .map(|s| s.to_string())
                .collect();
            builder = builder.set_events(Some(event_strings));
        }
        
        builder
            .send()
            .await
            .map(|_| json!({"status": "updated"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "update_webhook"
    }
}