use crate::SuperpositionMcpServer;
use crate::helpers::*;
use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateExperimentParams {
    pub name: String,
    pub context: serde_json::Value,
    pub variants: Vec<VariantParam>,
    pub description: String,
    pub change_reason: String,
    pub experiment_type: Option<String>,
    pub metrics: Option<serde_json::Value>,
    pub experiment_group_id: Option<String>,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct VariantParam {
    pub id: String,
    pub variant_type: String,
    pub overrides: serde_json::Value,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetExperimentParams {
    pub id: String,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListExperimentsParams {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub status: Option<Vec<String>>,
    pub experiment_name: Option<String>,
    pub sort_on: Option<String>,
    pub sort_by: Option<String>,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct ConcludeExperimentParams {
    pub id: String,
    pub chosen_variant: String,
    pub change_reason: String,
    pub description: Option<String>,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct DiscardExperimentParams {
    pub id: String,
    pub change_reason: String,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct RampExperimentParams {
    pub id: String,
    pub traffic_percentage: i32,
    pub change_reason: String,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct PauseResumeExperimentParams {
    pub id: String,
    pub change_reason: String,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateExperimentOverridesParams {
    pub id: String,
    pub variants: Vec<VariantUpdateParam>,
    pub change_reason: String,
    pub description: Option<String>,
    pub metrics: Option<serde_json::Value>,
    pub experiment_group_id: Option<String>,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct VariantUpdateParam {
    pub id: String,
    pub overrides: serde_json::Value,
}
#[derive(Debug, Deserialize, JsonSchema)]
pub struct ApplicableVariantsParams {
    pub context: serde_json::Value,
    pub identifier: String,
}

impl SuperpositionMcpServer {
    pub async fn create_experiment_impl(
        &self,
        args: CreateExperimentParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let ctx_map = json_to_doc_map(args.context).map_err(mcp_err)?;
        let mut req = self
            .client
            .create_experiment()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .name(args.name)
            .set_context(Some(ctx_map))
            .description(args.description)
            .change_reason(args.change_reason);
        for v in args.variants {
            let ovr = json_to_doc_map(v.overrides).map_err(mcp_err)?;
            let vt = if v.variant_type.to_uppercase() == "CONTROL" {
                superposition_sdk::types::VariantType::Control
            } else {
                superposition_sdk::types::VariantType::Experimental
            };
            req = req.variants(
                superposition_sdk::types::Variant::builder()
                    .id(v.id)
                    .variant_type(vt)
                    .set_overrides(Some(ovr))
                    .build()
                    .map_err(mcp_err)?,
            );
        }
        if let Some(et) = args.experiment_type {
            req = req.experiment_type(if et.to_uppercase() == "DELETE_OVERRIDES" {
                superposition_sdk::types::ExperimentType::DeleteOverrides
            } else {
                superposition_sdk::types::ExperimentType::Default
            });
        }
        if let Some(m) = args.metrics {
            req = req.metrics(json_to_doc(m));
        }
        if let Some(gid) = args.experiment_group_id {
            req = req.experiment_group_id(gid);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_to_json!(resp)).map_err(mcp_err)?,
        )]))
    }
    pub async fn get_experiment_impl(
        &self,
        args: GetExperimentParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_experiment()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .send()
            .await
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_to_json!(resp)).map_err(mcp_err)?,
        )]))
    }
    pub async fn list_experiments_impl(
        &self,
        args: ListExperimentsParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .list_experiment()
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
        if let Some(statuses) = args.status {
            for s in statuses {
                let st = match s.to_uppercase().as_str() {
                    "CREATED" => superposition_sdk::types::ExperimentStatusType::Created,
                    "INPROGRESS" => {
                        superposition_sdk::types::ExperimentStatusType::Inprogress
                    }
                    "CONCLUDED" => {
                        superposition_sdk::types::ExperimentStatusType::Concluded
                    }
                    "DISCARDED" => {
                        superposition_sdk::types::ExperimentStatusType::Discarded
                    }
                    "PAUSED" => superposition_sdk::types::ExperimentStatusType::Paused,
                    _ => continue,
                };
                req = req.status(st);
            }
        }
        if let Some(name) = args.experiment_name {
            req = req.experiment_name(name);
        }
        if let Some(so) = args.sort_on {
            req = req.sort_on(if so == "created_at" {
                superposition_sdk::types::ExperimentSortOn::CreatedAt
            } else {
                superposition_sdk::types::ExperimentSortOn::LastModifiedAt
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
        let items: Vec<serde_json::Value> =
            resp.data.iter().map(|r| experiment_to_json!(r)).collect();
        Ok(CallToolResult::success(vec![Content::text(serde_json::to_string_pretty(&serde_json::json!({"total_pages": resp.total_pages, "total_items": resp.total_items, "data": items})).map_err(mcp_err)?)]))
    }
    pub async fn conclude_experiment_impl(
        &self,
        args: ConcludeExperimentParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .conclude_experiment()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .chosen_variant(args.chosen_variant)
            .change_reason(args.change_reason);
        if let Some(d) = args.description {
            req = req.description(d);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_to_json!(resp)).map_err(mcp_err)?,
        )]))
    }
    pub async fn discard_experiment_impl(
        &self,
        args: DiscardExperimentParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .discard_experiment()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .change_reason(args.change_reason)
            .send()
            .await
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_to_json!(resp)).map_err(mcp_err)?,
        )]))
    }
    pub async fn ramp_experiment_impl(
        &self,
        args: RampExperimentParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .ramp_experiment()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .traffic_percentage(args.traffic_percentage)
            .change_reason(args.change_reason)
            .send()
            .await
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_to_json!(resp)).map_err(mcp_err)?,
        )]))
    }
    pub async fn pause_experiment_impl(
        &self,
        args: PauseResumeExperimentParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .pause_experiment()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .change_reason(args.change_reason)
            .send()
            .await
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_to_json!(resp)).map_err(mcp_err)?,
        )]))
    }
    pub async fn resume_experiment_impl(
        &self,
        args: PauseResumeExperimentParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .resume_experiment()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .change_reason(args.change_reason)
            .send()
            .await
            .map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_to_json!(resp)).map_err(mcp_err)?,
        )]))
    }
    pub async fn update_experiment_overrides_impl(
        &self,
        args: UpdateExperimentOverridesParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .update_overrides_experiment()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .id(args.id)
            .change_reason(args.change_reason);
        for v in args.variants {
            let ovr = json_to_doc_map(v.overrides).map_err(mcp_err)?;
            req = req.variant_list(
                superposition_sdk::types::VariantUpdateRequest::builder()
                    .id(v.id)
                    .set_overrides(Some(ovr))
                    .build()
                    .map_err(mcp_err)?,
            );
        }
        if let Some(d) = args.description {
            req = req.description(d);
        }
        if let Some(m) = args.metrics {
            req = req.metrics(json_to_doc(m));
        }
        if let Some(gid) = args.experiment_group_id {
            req = req.experiment_group_id(gid);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&experiment_to_json!(resp)).map_err(mcp_err)?,
        )]))
    }
    pub async fn applicable_variants_impl(
        &self,
        args: ApplicableVariantsParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let ctx_map = json_to_doc_map(args.context).map_err(mcp_err)?;
        let resp = self
            .client
            .applicable_variants()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .set_context(Some(ctx_map))
            .identifier(args.identifier)
            .send()
            .await
            .map_err(mcp_err)?;
        let items: Vec<serde_json::Value> = resp.data.iter().map(|v| serde_json::json!({"id": v.id, "variant_type": format!("{:?}", v.variant_type), "overrides": doc_map_to_json(&v.overrides), "context_id": v.context_id, "override_id": v.override_id})).collect();
        Ok(CallToolResult::success(vec![Content::text(
            serde_json::to_string_pretty(&serde_json::json!({"data": items}))
                .map_err(mcp_err)?,
        )]))
    }
}
