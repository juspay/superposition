use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::helpers::*;
use crate::SuperpositionMcpServer;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateSecretParams {
    /// Secret name
    pub name: String,
    /// Plaintext value to encrypt and store
    pub value: String,
    /// Human-readable description
    pub description: String,
    /// Reason for this change
    pub change_reason: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetSecretParams {
    /// Secret name to retrieve
    pub name: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListSecretsParams {
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
pub struct UpdateSecretParams {
    /// Secret name to update
    pub name: String,
    /// Reason for this change
    pub change_reason: String,
    /// New plaintext value to encrypt
    pub value: Option<String>,
    /// Updated description
    pub description: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct DeleteSecretParams {
    /// Secret name to delete
    pub name: String,
}


impl SuperpositionMcpServer {
    pub async fn create_secret_impl(
        &self,
        args: CreateSecretParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .create_secret()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .value(args.value)
            .description(args.description)
            .change_reason(args.change_reason)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&secret_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn get_secret_impl(
        &self,
        args: GetSecretParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_secret()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&secret_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn list_secrets_impl(
        &self,
        args: ListSecretsParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .list_secrets()
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
                "name" => superposition_sdk::types::SecretSortOn::Name,
                "created_at" => superposition_sdk::types::SecretSortOn::CreatedAt,
                _ => superposition_sdk::types::SecretSortOn::LastModifiedAt,
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
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let items: Vec<serde_json::Value> = resp.data.iter().map(|r| secret_to_json!(r)).collect();
        let result = serde_json::json!({
            "total_pages": resp.total_pages,
            "total_items": resp.total_items,
            "data": items,
        });
        let json = serde_json::to_string_pretty(&result).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn update_secret_impl(
        &self,
        args: UpdateSecretParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .update_secret()
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
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&secret_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn delete_secret_impl(
        &self,
        args: DeleteSecretParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .delete_secret()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&secret_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }
}
