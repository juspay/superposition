use rmcp::model::*;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::helpers::*;
use crate::SuperpositionMcpServer;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateOrganisationParams {
    /// Organisation name
    pub name: String,
    /// Admin email address
    pub admin_email: String,
    /// Optional country code
    pub country_code: Option<String>,
    /// Optional contact email
    pub contact_email: Option<String>,
    /// Optional contact phone
    pub contact_phone: Option<String>,
    /// Optional business sector
    pub sector: Option<String>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct GetOrganisationParams {
    /// Organisation ID
    pub id: String,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ListOrganisationsParams {
    /// Number of items per page
    pub count: Option<i32>,
    /// Page number (starting from 1)
    pub page: Option<i32>,
    /// If true, returns all items ignoring pagination
    pub all: Option<bool>,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct UpdateOrganisationParams {
    /// Organisation ID
    pub id: String,
    /// Updated status: Active, Inactive, or PendingKyb
    pub status: Option<String>,
    /// Updated country code
    pub country_code: Option<String>,
    /// Updated contact email
    pub contact_email: Option<String>,
    /// Updated contact phone
    pub contact_phone: Option<String>,
    /// Updated admin email
    pub admin_email: Option<String>,
    /// Updated sector
    pub sector: Option<String>,
}

impl SuperpositionMcpServer {
    pub async fn create_organisation_impl(
        &self,
        args: CreateOrganisationParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self
            .client
            .create_organisation()
            .name(args.name)
            .admin_email(args.admin_email);
        if let Some(cc) = args.country_code {
            req = req.country_code(cc);
        }
        if let Some(ce) = args.contact_email {
            req = req.contact_email(ce);
        }
        if let Some(cp) = args.contact_phone {
            req = req.contact_phone(cp);
        }
        if let Some(s) = args.sector {
            req = req.sector(s);
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let json =
            serde_json::to_string_pretty(&organisation_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn get_organisation_impl(
        &self,
        args: GetOrganisationParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let resp = self
            .client
            .get_organisation()
            .id(args.id)
            .send()
            .await
            .map_err(|e| mcp_err(e))?;
        let json =
            serde_json::to_string_pretty(&organisation_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn list_organisations_impl(
        &self,
        args: ListOrganisationsParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self.client.list_organisation();
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
        let items: Vec<serde_json::Value> =
            resp.data.iter().map(|r| organisation_to_json!(r)).collect();
        let result = serde_json::json!({
            "total_pages": resp.total_pages,
            "total_items": resp.total_items,
            "data": items,
        });
        let json = serde_json::to_string_pretty(&result).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }

    pub async fn update_organisation_impl(
        &self,
        args: UpdateOrganisationParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let mut req = self.client.update_organisation().id(args.id);
        if let Some(st) = args.status {
            let status = match st.as_str() {
                "Inactive" => superposition_sdk::types::OrgStatus::Inactive,
                "PendingKyb" => superposition_sdk::types::OrgStatus::PendingKyb,
                _ => superposition_sdk::types::OrgStatus::Active,
            };
            req = req.status(status);
        }
        if let Some(cc) = args.country_code {
            req = req.country_code(cc);
        }
        if let Some(ce) = args.contact_email {
            req = req.contact_email(ce);
        }
        if let Some(cp) = args.contact_phone {
            req = req.contact_phone(cp);
        }
        if let Some(ae) = args.admin_email {
            req = req.admin_email(ae);
        }
        if let Some(s) = args.sector {
            req = req.sector(s);
        }
        let resp = req.send().await.map_err(|e| mcp_err(e))?;
        let json =
            serde_json::to_string_pretty(&organisation_to_json!(resp)).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }
}
