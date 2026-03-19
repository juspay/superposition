use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::helpers::*;
use crate::SuperpositionMcpServer;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateWorkspaceParams {
    /// Workspace name
    pub workspace_name: String,
    /// Workspace admin email
    pub workspace_admin_email: String,
    /// Optional workspace status: ENABLED or DISABLED
    pub workspace_status: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetWorkspaceParams {
    /// Workspace name to retrieve
    pub workspace_name: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListWorkspacesParams {
    /// Number of items per page
    pub count: Option<i32>,
    /// Page number (starting from 1)
    pub page: Option<i32>,
    /// If true, returns all items ignoring pagination
    pub all: Option<bool>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateWorkspaceParams {
    /// Workspace name to update
    pub workspace_name: String,
    /// Updated admin email
    pub workspace_admin_email: Option<String>,
    /// Updated status: ENABLED or DISABLED
    pub workspace_status: Option<String>,
    /// Updated config version (pass "null" to unset)
    pub config_version: Option<String>,
    /// Updated mandatory dimensions list
    pub mandatory_dimensions: Option<Vec<String>>,
}

impl SuperpositionMcpServer {
    pub async fn create_workspace_impl(
        &self,
        args: CreateWorkspaceParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .create_workspace()
            .org_id(&self.config.org_id)
            .workspace_name(args.workspace_name)
            .workspace_admin_email(args.workspace_admin_email);
        if let Some(ws) = args.workspace_status {
            let status = match ws.to_uppercase().as_str() {
                "DISABLED" => superposition_sdk::types::WorkspaceStatus::Disabled,
                _ => superposition_sdk::types::WorkspaceStatus::Enabled,
            };
            req = req.workspace_status(status);
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&workspace_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn get_workspace_impl(
        &self,
        args: GetWorkspaceParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_workspace()
            .org_id(&self.config.org_id)
            .workspace_name(args.workspace_name)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&workspace_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn list_workspaces_impl(
        &self,
        args: ListWorkspacesParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self.client.list_workspace().org_id(&self.config.org_id);
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
        let items: Vec<serde_json::Value> = resp.data.iter().map(|r| workspace_to_json!(r)).collect();
        let result = serde_json::json!({
            "total_pages": resp.total_pages,
            "total_items": resp.total_items,
            "data": items,
        });
        let json = serde_json::to_string_pretty(&result).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn update_workspace_impl(
        &self,
        args: UpdateWorkspaceParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .update_workspace()
            .org_id(&self.config.org_id)
            .workspace_name(args.workspace_name);
        if let Some(ae) = args.workspace_admin_email {
            req = req.workspace_admin_email(ae);
        }
        if let Some(ws) = args.workspace_status {
            let status = match ws.to_uppercase().as_str() {
                "DISABLED" => superposition_sdk::types::WorkspaceStatus::Disabled,
                _ => superposition_sdk::types::WorkspaceStatus::Enabled,
            };
            req = req.workspace_status(status);
        }
        if let Some(cv) = args.config_version {
            req = req.config_version(cv);
        }
        if let Some(md) = args.mandatory_dimensions {
            for d in md {
                req = req.mandatory_dimensions(d);
            }
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&workspace_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }
}
