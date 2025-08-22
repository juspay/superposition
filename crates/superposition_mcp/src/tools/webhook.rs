use serde_json::{json, Value};
use std::error::Error;
use crate::mcp_service::{Tool, McpService, value_to_hashmap};
use super::ToolsModule;

pub struct WebhookTools;

impl ToolsModule for WebhookTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "create_webhook".to_string(),
                description: "Create a new webhook".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "url": {"type": "string", "description": "Webhook URL"},
                        "headers": {"type": "object", "description": "Webhook headers"},
                        "events": {"type": "array", "items": {"type": "string"}, "description": "Event types"},
                        "change_reason": {"type": "string", "description": "Reason for creating webhook"}
                    },
                    "required": ["url", "headers", "events", "change_reason"]
                }),
            },
            Tool {
                name: "get_webhook".to_string(),
                description: "Get a webhook by ID".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "webhook_id": {"type": "string", "description": "Webhook ID"}
                    },
                    "required": ["webhook_id"]
                }),
            },
            Tool {
                name: "list_webhook".to_string(),
                description: "List all webhooks".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "update_webhook".to_string(),
                description: "Update a webhook".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "webhook_id": {"type": "string", "description": "Webhook ID"},
                        "url": {"type": "string", "description": "Updated webhook URL"},
                        "headers": {"type": "object", "description": "Updated webhook headers"},
                        "events": {"type": "array", "items": {"type": "string"}, "description": "Updated event types"},
                        "change_reason": {"type": "string", "description": "Reason for updating webhook"}
                    },
                    "required": ["webhook_id", "change_reason"]
                }),
            },
        ]
    }

    async fn execute_tool(
        service: &McpService,
        tool_name: &str,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        match tool_name {
            "create_webhook" => {
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
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .url(url)
                    .set_custom_headers(headers_hashmap)
                    .set_events(Some(event_strings))
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "created"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_webhook" => {
                let webhook_id = arguments["webhook_id"].as_str().unwrap_or("");
                service
                    .superposition_client
                    .get_webhook()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .name(webhook_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "found"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_webhook" => {
                service
                    .superposition_client
                    .list_webhook()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Webhooks found"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_webhook" => {
                let webhook_id = arguments["webhook_id"].as_str().unwrap_or("");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("");
                
                let mut builder = service
                    .superposition_client
                    .update_webhook()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
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
            _ => Err(format!("Unknown webhook tool: {}", tool_name).into()),
        }
    }
}