use crate::SuperpositionMcpServer;
use crate::helpers::*;
use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateExperimentGroupParams {
    pub name: String,
    pub description: String,
    pub change_reason: String,
    pub context: serde_json::Value,
    pub traffic_percentage: i32,
    pub member_experiment_ids: Option<Vec<String>>,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetExperimentGroupParams {
    pub id: String,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListExperimentGroupsParams {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub name: Option<String>,
    pub sort_on: Option<String>,
    pub sort_by: Option<String>,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateExperimentGroupParams {
    pub id: String,
    pub change_reason: String,
    pub description: Option<String>,
    pub traffic_percentage: Option<i32>,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct DeleteExperimentGroupParams {
    pub id: String,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct ModifyGroupMembersParams {
    pub id: String,
    pub change_reason: String,
    pub member_experiment_ids: Vec<String>,
}

impl SuperpositionMcpServer {
    pub async fn create_experiment_group_impl(
        &self,
        args: CreateExperimentGroupParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let ctx_map = json_to_doc_map(args.context).map_err(mcp_err)?;
        let mut req = self
            .client
            .create_experiment_group()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .description(args.description)
            .change_reason(args.change_reason)
            .set_context(Some(ctx_map))
            .traffic_percentage(args.traffic_percentage);
        if let Some(m) = args.member_experiment_ids {
            for id in m {
                req = req.member_experiment_ids(id);
            }
        }
        let resp = req.send().await.map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_group_to_json!(resp))
                .map_err(mcp_err)?,
        )]))
    }
    pub async fn get_experiment_group_impl(
        &self,
        args: GetExperimentGroupParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_experiment_group()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .send()
            .await
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_group_to_json!(resp))
                .map_err(mcp_err)?,
        )]))
    }
    pub async fn list_experiment_groups_impl(
        &self,
        args: ListExperimentGroupsParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .list_experiment_groups()
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
        if let Some(so) = args.sort_on {
            req = req.sort_on(match so.as_str() {
                "name" => superposition_sdk::types::ExperimentGroupSortOn::Name,
                "created_at" => {
                    superposition_sdk::types::ExperimentGroupSortOn::CreatedAt
                }
                _ => superposition_sdk::types::ExperimentGroupSortOn::LastModifiedAt,
            });
        }
        if let Some(sb) = args.sort_by {
            req = req.sort_by(if sb == "asc" {
                superposition_sdk::types::SortBy::Asc
            } else {
                superposition_sdk::types::SortBy::Desc
            });
        }
        let resp = req.send().await.map_err(mcp_err)?;
        let items: Vec<serde_json::Value> = resp
            .data
            .iter()
            .map(|r| experiment_group_to_json!(r))
            .collect();
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&serde_json::json!({"total_pages": resp.total_pages, "total_items": resp.total_items, "data": items})).map_err(mcp_err)?)]))
    }
    pub async fn update_experiment_group_impl(
        &self,
        args: UpdateExperimentGroupParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .update_experiment_group()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .change_reason(args.change_reason);
        if let Some(d) = args.description {
            req = req.description(d);
        }
        if let Some(tp) = args.traffic_percentage {
            req = req.traffic_percentage(tp);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_group_to_json!(resp))
                .map_err(mcp_err)?,
        )]))
    }
    pub async fn delete_experiment_group_impl(
        &self,
        args: DeleteExperimentGroupParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .delete_experiment_group()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .send()
            .await
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_group_to_json!(resp))
                .map_err(mcp_err)?,
        )]))
    }
    pub async fn add_members_to_group_impl(
        &self,
        args: ModifyGroupMembersParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .add_members_to_group()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .change_reason(args.change_reason);
        for m in args.member_experiment_ids {
            req = req.member_experiment_ids(m);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_group_to_json!(resp))
                .map_err(mcp_err)?,
        )]))
    }
    pub async fn remove_members_from_group_impl(
        &self,
        args: ModifyGroupMembersParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .remove_members_from_group()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .change_reason(args.change_reason);
        for m in args.member_experiment_ids {
            req = req.member_experiment_ids(m);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_group_to_json!(resp))
                .map_err(mcp_err)?,
        )]))
    }
}
