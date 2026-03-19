use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::SuperpositionMcpServer;
use crate::helpers::*;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListAuditLogsParams {
    /// Number of items per page
    pub count: Option<i32>,
    /// Page number (starting from 1)
    pub page: Option<i32>,
    /// If true, returns all items ignoring pagination
    pub all: Option<bool>,
    /// Filter by table names
    pub tables: Option<Vec<String>>,
    /// Filter by actions: INSERT, UPDATE, DELETE
    pub action: Option<Vec<String>>,
    /// Filter by username
    pub username: Option<String>,
    /// Sort order: asc or desc
    pub sort_by: Option<String>,
    /// Start date filter (ISO 8601)
    pub from_date: Option<String>,
    /// End date filter (ISO 8601)
    pub to_date: Option<String>,
}

impl SuperpositionMcpServer {
    pub async fn list_audit_logs_impl(
        &self,
        args: ListAuditLogsParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .list_audit_logs()
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
        if let Some(tables) = args.tables {
            for t in tables {
                req = req.tables(t);
            }
        }
        if let Some(actions) = args.action {
            for a in actions {
                let act = match a.to_uppercase().as_str() {
                    "INSERT" => superposition_sdk::types::AuditAction::Insert,
                    "UPDATE" => superposition_sdk::types::AuditAction::Update,
                    "DELETE" => superposition_sdk::types::AuditAction::Delete,
                    _ => continue,
                };
                req = req.action(act);
            }
        }
        if let Some(u) = args.username {
            req = req.username(u);
        }
        if let Some(sb) = args.sort_by {
            let sort = match sb.as_str() {
                "asc" => superposition_sdk::types::SortBy::Asc,
                _ => superposition_sdk::types::SortBy::Desc,
            };
            req = req.sort_by(sort);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        let items: Vec<serde_json::Value> =
            resp.data.iter().map(|r| audit_log_to_json!(r)).collect();
        let result = serde_json::json!({
            "total_pages": resp.total_pages,
            "total_items": resp.total_items,
            "data": items,
        });
        let json = serde_json::to_string_pretty(&result).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }
}
