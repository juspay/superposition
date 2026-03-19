use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::SuperpositionMcpServer;
use crate::helpers::*;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateDimensionParams {
    pub dimension: String,
    pub position: i32,
    pub schema: serde_json::Value,
    pub description: String,
    pub change_reason: String,
    pub value_validation_function_name: Option<String>,
    pub value_compute_function_name: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetDimensionParams {
    pub dimension: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListDimensionsParams {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateDimensionParams {
    pub dimension: String,
    pub change_reason: String,
    pub schema: Option<serde_json::Value>,
    pub position: Option<i32>,
    pub description: Option<String>,
    pub value_validation_function_name: Option<String>,
    pub value_compute_function_name: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct DeleteDimensionParams {
    pub dimension: String,
}

impl SuperpositionMcpServer {
    pub async fn create_dimension_impl(
        &self,
        args: CreateDimensionParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let schema_map = json_to_doc_map(args.schema).map_err(mcp_err)?;
        let mut req = self
            .client
            .create_dimension()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .dimension(args.dimension)
            .position(args.position)
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
        let json =
            serde_json::to_string_pretty(&dimension_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn get_dimension_impl(
        &self,
        args: GetDimensionParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_dimension()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .dimension(args.dimension)
            .send()
            .await
            .map_err(mcp_err)?;
        let json =
            serde_json::to_string_pretty(&dimension_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn list_dimensions_impl(
        &self,
        args: ListDimensionsParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .list_dimensions()
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
        let items: Vec<serde_json::Value> =
            resp.data.iter().map(|r| dimension_to_json!(r)).collect();
        let result = serde_json::json!({"total_pages": resp.total_pages, "total_items": resp.total_items, "data": items});
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&result).map_err(mcp_err)?,
        )]))
    }

    pub async fn update_dimension_impl(
        &self,
        args: UpdateDimensionParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .update_dimension()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .dimension(args.dimension)
            .change_reason(args.change_reason);
        if let Some(s) = args.schema {
            req = req.set_schema(Some(json_to_doc_map(s).map_err(mcp_err)?));
        }
        if let Some(p) = args.position {
            req = req.position(p);
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
        let json =
            serde_json::to_string_pretty(&dimension_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn delete_dimension_impl(
        &self,
        args: DeleteDimensionParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.client
            .delete_dimension()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .dimension(args.dimension)
            .send()
            .await
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            "Dimension deleted successfully",
        )]))
    }
}
