use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::SuperpositionMcpServer;
use crate::helpers::*;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateTypeTemplateParams {
    /// Type template name
    pub type_name: String,
    /// JSON schema defining the type
    pub type_schema: serde_json::Value,
    /// Human-readable description
    pub description: String,
    /// Reason for this change
    pub change_reason: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetTypeTemplateParams {
    /// Type template name to retrieve
    pub type_name: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListTypeTemplatesParams {
    /// Number of items per page
    pub count: Option<i32>,
    /// Page number (starting from 1)
    pub page: Option<i32>,
    /// If true, returns all items ignoring pagination
    pub all: Option<bool>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateTypeTemplateParams {
    /// Type template name to update
    pub type_name: String,
    /// Updated JSON schema
    pub type_schema: serde_json::Value,
    /// Reason for this change
    pub change_reason: String,
    /// Optional updated description
    pub description: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct DeleteTypeTemplateParams {
    /// Type template name to delete
    pub type_name: String,
}

impl SuperpositionMcpServer {
    pub async fn create_type_template_impl(
        &self,
        args: CreateTypeTemplateParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let schema_map = json_to_doc_map(args.type_schema).map_err(mcp_err)?;
        let resp = self
            .client
            .create_type_templates()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .type_name(args.type_name)
            .set_type_schema(Some(schema_map))
            .description(args.description)
            .change_reason(args.change_reason)
            .send()
            .await
            .map_err(mcp_err)?;
        let json = serde_json::to_string_pretty(&type_template_to_json!(resp))
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn get_type_template_impl(
        &self,
        args: GetTypeTemplateParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_type_template()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .type_name(args.type_name)
            .send()
            .await
            .map_err(mcp_err)?;
        let json = serde_json::to_string_pretty(&type_template_to_json!(resp))
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn list_type_templates_impl(
        &self,
        args: ListTypeTemplatesParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .get_type_templates_list()
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
        let resp = req.send().await.map_err(mcp_err)?;
        let items: Vec<serde_json::Value> = resp
            .data
            .iter()
            .map(|r| type_template_to_json!(r))
            .collect();
        let result = serde_json::json!({
            "total_pages": resp.total_pages,
            "total_items": resp.total_items,
            "data": items,
        });
        let json = serde_json::to_string_pretty(&result).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn update_type_template_impl(
        &self,
        args: UpdateTypeTemplateParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let schema_map = json_to_doc_map(args.type_schema).map_err(mcp_err)?;
        let mut req = self
            .client
            .update_type_templates()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .type_name(args.type_name)
            .set_type_schema(Some(schema_map))
            .change_reason(args.change_reason);
        if let Some(d) = args.description {
            req = req.description(d);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        let json = serde_json::to_string_pretty(&type_template_to_json!(resp))
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn delete_type_template_impl(
        &self,
        args: DeleteTypeTemplateParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .delete_type_templates()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .type_name(args.type_name)
            .send()
            .await
            .map_err(mcp_err)?;
        let json = serde_json::to_string_pretty(&type_template_to_json!(resp))
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }
}
