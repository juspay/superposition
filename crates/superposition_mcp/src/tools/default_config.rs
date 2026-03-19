use crate::SuperpositionMcpServer;
use crate::helpers::*;
use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateDefaultConfigParams {
    pub key: String,
    pub value: serde_json::Value,
    pub schema: serde_json::Value,
    pub description: String,
    pub change_reason: String,
    pub value_validation_function_name: Option<String>,
    pub value_compute_function_name: Option<String>,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetDefaultConfigParams {
    pub key: String,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListDefaultConfigsParams {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub name: Option<String>,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateDefaultConfigParams {
    pub key: String,
    pub change_reason: String,
    pub value: Option<serde_json::Value>,
    pub schema: Option<serde_json::Value>,
    pub description: Option<String>,
    pub value_validation_function_name: Option<String>,
    pub value_compute_function_name: Option<String>,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct DeleteDefaultConfigParams {
    pub key: String,
}

impl SuperpositionMcpServer {
    pub async fn create_default_config_impl(
        &self,
        args: CreateDefaultConfigParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let doc_val = json_to_doc(args.value);
        let schema_map = json_to_doc_map(args.schema).map_err(mcp_err)?;
        let mut req = self
            .client
            .create_default_config()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .key(args.key)
            .value(doc_val)
            .set_schema(Some(schema_map))
            .description(args.description)
            .change_reason(args.change_reason);
        if let Some(f) = args.value_validation_function_name {
            req = req.value_validation_function_name(f);
        }
        if let Some(f) = args.value_compute_function_name {
            req = req.value_compute_function_name(f);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&default_config_to_json!(resp))
                .map_err(mcp_err)?,
        )]))
    }
    pub async fn get_default_config_impl(
        &self,
        args: GetDefaultConfigParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_default_config()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .key(args.key)
            .send()
            .await
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&default_config_to_json!(resp))
                .map_err(mcp_err)?,
        )]))
    }
    pub async fn list_default_configs_impl(
        &self,
        args: ListDefaultConfigsParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .list_default_configs()
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
        if let Some(n) = args.name {
            req = req.name(n);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        let items: Vec<serde_json::Value> = resp
            .data
            .iter()
            .map(|r| default_config_to_json!(r))
            .collect();
        let result = serde_json::json!({"total_pages": resp.total_pages, "total_items": resp.total_items, "data": items});
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&result).map_err(mcp_err)?,
        )]))
    }
    pub async fn update_default_config_impl(
        &self,
        args: UpdateDefaultConfigParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .update_default_config()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .key(args.key)
            .change_reason(args.change_reason);
        if let Some(v) = args.value {
            req = req.value(json_to_doc(v));
        }
        if let Some(s) = args.schema {
            req = req.set_schema(Some(json_to_doc_map(s).map_err(mcp_err)?));
        }
        if let Some(d) = args.description {
            req = req.description(d);
        }
        if let Some(f) = args.value_validation_function_name {
            req = req.value_validation_function_name(f);
        }
        if let Some(f) = args.value_compute_function_name {
            req = req.value_compute_function_name(f);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&default_config_to_json!(resp))
                .map_err(mcp_err)?,
        )]))
    }
    pub async fn delete_default_config_impl(
        &self,
        args: DeleteDefaultConfigParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.client
            .delete_default_config()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .key(args.key)
            .send()
            .await
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            "Default config deleted successfully",
        )]))
    }
}
