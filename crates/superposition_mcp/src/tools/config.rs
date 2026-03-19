use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;
use crate::helpers::*;
use crate::SuperpositionMcpServer;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetConfigParams { pub context: Option<serde_json::Value>, pub prefix: Option<Vec<String>>, pub version: Option<String> }
#[derive(Debug, Deserialize, JsonSchema)]
pub struct ResolveConfigParams { pub context: Option<serde_json::Value>, pub prefix: Option<Vec<String>>, pub version: Option<String>, pub show_reasoning: Option<bool>, pub merge_strategy: Option<String>, pub context_id: Option<String>, pub resolve_remote: Option<bool> }
#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetVersionParams { pub id: String }
#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListVersionsParams { pub count: Option<i32>, pub page: Option<i32> }

impl SuperpositionMcpServer {
    pub async fn get_config_impl(&self, args: GetConfigParams) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self.client.get_config().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id);
        if let Some(ctx) = args.context { req = req.set_context(Some(json_to_doc_map(ctx).map_err(mcp_err)?)); }
        if let Some(prefix) = args.prefix { for p in prefix { req = req.prefix(p); } }
        if let Some(v) = args.version { req = req.version(v); }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let result = serde_json::json!({
            "contexts": resp.contexts.iter().map(|c| serde_json::json!({"id": c.id, "condition": doc_map_to_json(&c.condition), "priority": c.priority, "weight": c.weight, "override_with_keys": c.override_with_keys})).collect::<Vec<_>>(),
            "overrides": resp.overrides.iter().map(|(k, v)| (k.clone(), doc_map_to_json(v))).collect::<serde_json::Map<String, serde_json::Value>>(),
            "default_configs": doc_map_to_json(&resp.default_configs),
            "version": resp.version,
            "last_modified": format_datetime(&resp.last_modified),
        });
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&result).map_err(mcp_err)?)]))
    }
    pub async fn resolve_config_impl(&self, args: ResolveConfigParams) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self.client.get_resolved_config().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id);
        if let Some(ctx) = args.context { req = req.set_context(Some(json_to_doc_map(ctx).map_err(mcp_err)?)); }
        if let Some(prefix) = args.prefix { for p in prefix { req = req.prefix(p); } }
        if let Some(v) = args.version { req = req.version(v); }
        if let Some(sr) = args.show_reasoning { req = req.show_reasoning(sr); }
        if let Some(ms) = args.merge_strategy {
            req = req.merge_strategy(if ms.to_uppercase() == "REPLACE" { superposition_sdk::types::MergeStrategy::Replace } else { superposition_sdk::types::MergeStrategy::Merge });
        }
        if let Some(cid) = args.context_id { req = req.context_id(cid); }
        if let Some(rr) = args.resolve_remote { req = req.resolve_remote(rr); }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let result = serde_json::json!({"config": doc_to_json(&resp.config), "version": resp.version, "last_modified": format_datetime(&resp.last_modified)});
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&result).map_err(mcp_err)?)]))
    }
    pub async fn get_config_fast_impl(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self.client.get_config_fast().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id).send().await.map_err(|e| mcp_err(e))?;
        let result = serde_json::json!({"config": resp.config.as_ref().map(doc_to_json), "version": resp.version, "last_modified": resp.last_modified.as_ref().map(format_datetime)});
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&result).map_err(mcp_err)?)]))
    }
    pub async fn get_config_toml_impl(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self.client.get_config_toml().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id).send().await.map_err(|e| mcp_err(e))?;
        Ok(CallToolResult::success(vec![Content::text(resp.toml_config)]))
    }
    pub async fn get_config_json_impl(&self) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self.client.get_config_json().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id).send().await.map_err(|e| mcp_err(e))?;
        Ok(CallToolResult::success(vec![Content::text(resp.json_config)]))
    }
    pub async fn get_version_impl(&self, args: GetVersionParams) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self.client.get_version().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id).id(args.id).send().await.map_err(|e| mcp_err(e))?;
        let result = serde_json::json!({"id": resp.id, "config": doc_to_json(&resp.config), "config_hash": resp.config_hash, "created_at": format_datetime(&resp.created_at), "description": resp.description, "tags": resp.tags});
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&result).map_err(mcp_err)?)]))
    }
    pub async fn list_versions_impl(&self, args: ListVersionsParams) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self.client.list_versions().workspace_id(&self.config.workspace_id).org_id(&self.config.org_id);
        if let Some(c) = args.count { req = req.count(c); }
        if let Some(p) = args.page { req = req.page(p); }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let items: Vec<serde_json::Value> = resp.data.iter().map(|v| serde_json::json!({"id": v.id, "config": doc_to_json(&v.config), "created_at": format_datetime(&v.created_at), "description": v.description, "tags": v.tags})).collect();
        let result = serde_json::json!({"total_pages": resp.total_pages, "total_items": resp.total_items, "data": items});
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&result).map_err(mcp_err)?)]))
    }
}
