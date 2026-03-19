use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::helpers::*;
use crate::SuperpositionMcpServer;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateWebhookParams {
    /// Webhook name
    pub name: String,
    /// Human-readable description
    pub description: String,
    /// Whether the webhook is enabled
    pub enabled: bool,
    /// Target URL for the webhook
    pub url: String,
    /// HTTP method: GET, POST, PUT, PATCH, DELETE, HEAD
    pub method: String,
    /// List of event names that trigger the webhook
    pub events: Vec<String>,
    /// Reason for this change
    pub change_reason: String,
    /// Optional custom headers (JSON object)
    pub custom_headers: Option<serde_json::Value>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetWebhookParams {
    /// Webhook name to retrieve
    pub name: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListWebhooksParams {
    /// Number of items per page
    pub count: Option<i32>,
    /// Page number (starting from 1)
    pub page: Option<i32>,
    /// If true, returns all items ignoring pagination
    pub all: Option<bool>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateWebhookParams {
    /// Webhook name to update
    pub name: String,
    /// Reason for this change
    pub change_reason: String,
    /// Updated description
    pub description: Option<String>,
    /// Updated enabled status
    pub enabled: Option<bool>,
    /// Updated target URL
    pub url: Option<String>,
    /// Updated HTTP method
    pub method: Option<String>,
    /// Updated events list
    pub events: Option<Vec<String>>,
    /// Updated custom headers
    pub custom_headers: Option<serde_json::Value>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct DeleteWebhookParams {
    /// Webhook name to delete
    pub name: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetWebhookByEventParams {
    /// Event name to look up
    pub event: String,
}

fn parse_http_method(s: &str) -> superposition_sdk::types::HttpMethod {
    match s.to_uppercase().as_str() {
        "GET" => superposition_sdk::types::HttpMethod::Get,
        "PUT" => superposition_sdk::types::HttpMethod::Put,
        "PATCH" => superposition_sdk::types::HttpMethod::Patch,
        "DELETE" => superposition_sdk::types::HttpMethod::Delete,
        "HEAD" => superposition_sdk::types::HttpMethod::Head,
        _ => superposition_sdk::types::HttpMethod::Post,
    }
}


impl SuperpositionMcpServer {
    pub async fn create_webhook_impl(
        &self,
        args: CreateWebhookParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let method = parse_http_method(&args.method);
        let mut req = self
            .client
            .create_webhook()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .description(args.description)
            .enabled(args.enabled)
            .url(args.url)
            .method(method)
            .change_reason(args.change_reason);
        for e in args.events {
            req = req.events(e);
        }
        if let Some(ch) = args.custom_headers {
            let headers_map = json_to_doc_map(ch).map_err(mcp_err)?;
            req = req.set_custom_headers(Some(headers_map));
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&webhook_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn get_webhook_impl(
        &self,
        args: GetWebhookParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_webhook()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&webhook_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn list_webhooks_impl(
        &self,
        args: ListWebhooksParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .list_webhook()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id);
        if let Some(c) = args.count {
            req = req.count(c);
        }
        if let Some(p) = args.page {
            req = req.page(p);
        }
        if let Some(a) = args.all {
            req = req.all(a);
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let items: Vec<serde_json::Value> = resp.data.iter().map(|r| webhook_to_json!(r)).collect();
        let result = serde_json::json!({
            "total_pages": resp.total_pages,
            "total_items": resp.total_items,
            "data": items,
        });
        let json = serde_json::to_string_pretty(&result).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn update_webhook_impl(
        &self,
        args: UpdateWebhookParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .update_webhook()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .change_reason(args.change_reason);
        if let Some(d) = args.description {
            req = req.description(d);
        }
        if let Some(e) = args.enabled {
            req = req.enabled(e);
        }
        if let Some(u) = args.url {
            req = req.url(u);
        }
        if let Some(m) = args.method {
            req = req.method(parse_http_method(&m));
        }
        if let Some(events) = args.events {
            for e in events {
                req = req.events(e);
            }
        }
        if let Some(ch) = args.custom_headers {
            let headers_map = json_to_doc_map(ch).map_err(mcp_err)?;
            req = req.set_custom_headers(Some(headers_map));
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&webhook_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn delete_webhook_impl(
        &self,
        args: DeleteWebhookParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.client
            .delete_webhook()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        Ok(CallToolResult::success(vec![Content::text(
            "Webhook deleted successfully",
        )]))
    }

    pub async fn get_webhook_by_event_impl(
        &self,
        args: GetWebhookByEventParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_webhook_by_event()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .event(args.event)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&webhook_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }
}
