use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::helpers::*;
use crate::SuperpositionMcpServer;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateContextParams {
    /// Condition map: dimension names to their criteria values
    pub context: serde_json::Value,
    /// Override map: config keys to override values
    pub r#override: serde_json::Value,
    /// Reason for this change
    pub change_reason: String,
    /// Optional description
    pub description: Option<String>,
    /// Optional config tags header
    pub config_tags: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetContextParams {
    /// Context ID to retrieve
    pub id: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListContextsParams {
    /// Number of items per page
    pub count: Option<i32>,
    /// Page number (starting from 1)
    pub page: Option<i32>,
    /// If true, returns all items ignoring pagination
    pub all: Option<bool>,
    /// Filter by config key prefix
    pub prefix: Option<Vec<String>>,
    /// Sort field: last_modified_at, created_at, or weight
    pub sort_on: Option<String>,
    /// Sort order: asc or desc
    pub sort_by: Option<String>,
    /// Full-text search in context conditions
    pub plaintext: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct DeleteContextParams {
    /// Context ID to delete
    pub id: String,
    /// Optional config tags header
    pub config_tags: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateOverrideParams {
    /// Context identifier — either {"id": "..."} or {"context": {...condition...}}
    pub context: serde_json::Value,
    /// Override map: config keys to override values
    pub r#override: serde_json::Value,
    /// Reason for this change
    pub change_reason: String,
    /// Optional description
    pub description: Option<String>,
    /// Optional config tags header
    pub config_tags: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct MoveContextParams {
    /// Context ID to move
    pub id: String,
    /// New condition map for the context
    pub context: serde_json::Value,
    /// Reason for this change
    pub change_reason: String,
    /// Optional description
    pub description: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetContextFromConditionParams {
    /// Condition to match against (JSON object)
    pub context: serde_json::Value,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct BulkOperationParams {
    /// List of operations: each is one of PUT, REPLACE, DELETE, or MOVE
    pub operations: serde_json::Value,
    /// Optional config tags header
    pub config_tags: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct WeightRecomputeParams {
    /// Optional config tags header
    pub config_tags: Option<String>,
}

impl SuperpositionMcpServer {
    pub async fn create_context_impl(
        &self,
        args: CreateContextParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let ctx_map = json_to_doc_map(args.context).map_err(mcp_err)?;
        let ovr_map = json_to_doc_map(args.r#override).map_err(mcp_err)?;
        let mut put_builder = superposition_sdk::types::ContextPut::builder()
            .set_context(Some(ctx_map))
            .set_override(Some(ovr_map))
            .change_reason(args.change_reason);
        if let Some(d) = args.description {
            put_builder = put_builder.description(d);
        }
        let put = put_builder.build().map_err(|e| mcp_err(e))?;
        let mut req = self
            .client
            .create_context()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .request(put);
        if let Some(tags) = args.config_tags {
            req = req.config_tags(tags);
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&context_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn get_context_impl(
        &self,
        args: GetContextParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_context()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&context_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn list_contexts_impl(
        &self,
        args: ListContextsParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .list_contexts()
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
        if let Some(prefix) = args.prefix {
            for p in prefix {
                req = req.prefix(p);
            }
        }
        if let Some(sort_on) = args.sort_on {
            let sort = match sort_on.as_str() {
                "created_at" => superposition_sdk::types::ContextFilterSortOn::CreatedAt,
                "weight" => superposition_sdk::types::ContextFilterSortOn::Weight,
                _ => superposition_sdk::types::ContextFilterSortOn::LastModifiedAt,
            };
            req = req.sort_on(sort);
        }
        if let Some(sort_by) = args.sort_by {
            let sb = match sort_by.as_str() {
                "asc" => superposition_sdk::types::SortBy::Asc,
                _ => superposition_sdk::types::SortBy::Desc,
            };
            req = req.sort_by(sb);
        }
        if let Some(pt) = args.plaintext {
            req = req.plaintext(pt);
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let items: Vec<serde_json::Value> = resp.data.iter().map(|r| context_to_json!(r)).collect();
        let result = serde_json::json!({
            "total_pages": resp.total_pages,
            "total_items": resp.total_items,
            "data": items,
        });
        let json = serde_json::to_string_pretty(&result).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn delete_context_impl(
        &self,
        args: DeleteContextParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .delete_context()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id);
        if let Some(tags) = args.config_tags {
            req = req.config_tags(tags);
        }
        req.send().await.map_err(|e| mcp_err(e))?;
        Ok(CallToolResult::success(vec![Content::text(
            "Context deleted successfully",
        )]))
    }

    pub async fn update_override_impl(
        &self,
        args: UpdateOverrideParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let ovr_map = json_to_doc_map(args.r#override).map_err(mcp_err)?;
        let ctx_ident = if let Some(id) = args.context.get("id").and_then(|v| v.as_str()) {
            superposition_sdk::types::ContextIdentifier::Id(id.to_string())
        } else if let Some(cond) = args.context.get("context") {
            let cond_map = json_to_doc_map(cond.clone()).map_err(mcp_err)?;
            superposition_sdk::types::ContextIdentifier::Context(cond_map)
        } else {
            return Err(mcp_err("context must have either 'id' or 'context' field"));
        };
        let mut ucr_builder =
            superposition_sdk::types::UpdateContextOverrideRequest::builder()
                .context(ctx_ident)
                .set_override(Some(ovr_map))
                .change_reason(args.change_reason);
        if let Some(d) = args.description {
            ucr_builder = ucr_builder.description(d);
        }
        let ucr = ucr_builder.build().map_err(|e| mcp_err(e))?;
        let mut req = self
            .client
            .update_override()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .request(ucr);
        if let Some(tags) = args.config_tags {
            req = req.config_tags(tags);
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&context_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn move_context_impl(
        &self,
        args: MoveContextParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let ctx_map = json_to_doc_map(args.context).map_err(mcp_err)?;
        let mut move_builder = superposition_sdk::types::ContextMove::builder()
            .set_context(Some(ctx_map))
            .change_reason(args.change_reason);
        if let Some(d) = args.description {
            move_builder = move_builder.description(d);
        }
        let mv = move_builder.build().map_err(|e| mcp_err(e))?;
        let resp = self
            .client
            .move_context()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .request(mv)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&context_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn get_context_from_condition_impl(
        &self,
        args: GetContextFromConditionParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let doc = json_to_doc(args.context);
        let resp = self
            .client
            .get_context_from_condition()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .context(doc)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        let json = serde_json::to_string_pretty(&context_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn weight_recompute_impl(
        &self,
        args: WeightRecomputeParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .weight_recompute()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id);
        if let Some(tags) = args.config_tags {
            req = req.config_tags(tags);
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let items: Vec<serde_json::Value> = resp
            .data
            .as_deref()
            .unwrap_or_default()
            .iter()
            .map(|r| {
                serde_json::json!({
                    "id": r.id,
                    "condition": doc_map_to_json(&r.condition),
                    "old_weight": r.old_weight,
                    "new_weight": r.new_weight,
                })
            })
            .collect();
        let json = serde_json::to_string_pretty(&serde_json::json!({"data": items}))
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn bulk_operation_impl(
        &self,
        args: BulkOperationParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let json_str = serde_json::to_string(&args.operations).map_err(mcp_err)?;
        let result = serde_json::json!({
            "message": "Bulk operations require complex typed input. Please use the Superposition UI or SDK directly.",
            "operations_received": args.operations,
        });
        let _ = json_str;
        let json = serde_json::to_string_pretty(&result).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }
}
