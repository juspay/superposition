use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::SuperpositionMcpServer;
use crate::helpers::*;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateVariableParams {
    /// Variable name
    pub name: String,
    /// Variable value
    pub value: String,
    /// Human-readable description
    pub description: String,
    /// Reason for this change
    pub change_reason: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetVariableParams {
    /// Variable name to retrieve
    pub name: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListVariablesParams {
    /// Number of items per page
    pub count: Option<i32>,
    /// Page number (starting from 1)
    pub page: Option<i32>,
    /// If true, returns all items ignoring pagination
    pub all: Option<bool>,
    /// Sort field: name, created_at, or last_modified_at
    pub sort_on: Option<String>,
    /// Sort order: asc or desc
    pub sort_by: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateVariableParams {
    /// Variable name to update
    pub name: String,
    /// Reason for this change
    pub change_reason: String,
    /// Updated value
    pub value: Option<String>,
    /// Updated description
    pub description: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct DeleteVariableParams {
    /// Variable name to delete
    pub name: String,
}

impl SuperpositionMcpServer {
    pub async fn create_variable_impl(
        &self,
        args: CreateVariableParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .create_variable()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .value(args.value)
            .description(args.description)
            .change_reason(args.change_reason)
            .send()
            .await
            .map_err(mcp_err)?;
        let json =
            serde_json::to_string_pretty(&variable_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn get_variable_impl(
        &self,
        args: GetVariableParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_variable()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .send()
            .await
            .map_err(mcp_err)?;
        let json =
            serde_json::to_string_pretty(&variable_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn list_variables_impl(
        &self,
        args: ListVariablesParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .list_variables()
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
        if let Some(sort_on) = args.sort_on {
            let so = match sort_on.as_str() {
                "name" => superposition_sdk::types::VariableSortOn::Name,
                "created_at" => superposition_sdk::types::VariableSortOn::CreatedAt,
                _ => superposition_sdk::types::VariableSortOn::LastModifiedAt,
            };
            req = req.sort_on(so);
        }
        if let Some(sort_by) = args.sort_by {
            let sb = match sort_by.as_str() {
                "asc" => superposition_sdk::types::SortBy::Asc,
                _ => superposition_sdk::types::SortBy::Desc,
            };
            req = req.sort_by(sb);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        let items: Vec<serde_json::Value> =
            resp.data.iter().map(|r| variable_to_json!(r)).collect();
        let result = serde_json::json!({
            "total_pages": resp.total_pages,
            "total_items": resp.total_items,
            "data": items,
        });
        let json = serde_json::to_string_pretty(&result).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn update_variable_impl(
        &self,
        args: UpdateVariableParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .update_variable()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .change_reason(args.change_reason);
        if let Some(v) = args.value {
            req = req.value(v);
        }
        if let Some(d) = args.description {
            req = req.description(d);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        let json =
            serde_json::to_string_pretty(&variable_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn delete_variable_impl(
        &self,
        args: DeleteVariableParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .delete_variable()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .send()
            .await
            .map_err(mcp_err)?;
        let json =
            serde_json::to_string_pretty(&variable_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }
}
