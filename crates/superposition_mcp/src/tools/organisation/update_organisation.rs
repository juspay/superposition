use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;
use superposition_sdk::types::OrgStatus;

pub struct UpdateOrganisationTool;

impl MCPTool for UpdateOrganisationTool {
    fn get_definition() -> Tool {
        Tool {
            name: "update_organisation".to_string(),
            description: "Update an organisation".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "id": {"type": "string", "description": "Organisation ID"},
                    "admin_email": {"type": "string", "description": "Updated admin email"},
                    "country_code": {"type": "string", "description": "Updated country code"},
                    "contact_email": {"type": "string", "description": "Updated contact email"},
                    "contact_phone": {"type": "string", "description": "Updated contact phone number"},
                    "sector": {"type": "string", "description": "Updated business sector"},
                    "status": {"type": "string", "enum": ["ACTIVE", "INACTIVE", "PENDING_KYB"], "description": "Organisation status"}
                },
                "required": ["id"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let id = arguments["id"].as_str().unwrap_or("");

        let mut builder = service
            .superposition_client
            .update_organisation()
            .id(id);

        // Optional parameters
        if let Some(contact_email) = arguments["contact_email"].as_str() {
            builder = builder.contact_email(contact_email);
        }
        if let Some(contact_phone) = arguments["contact_phone"].as_str() {
            builder = builder.contact_phone(contact_phone);
        }
        if let Some(admin_email) = arguments["admin_email"].as_str() {
            builder = builder.admin_email(admin_email);
        }
        if let Some(country_code) = arguments["country_code"].as_str() {
            builder = builder.country_code(country_code);
        }
        if let Some(sector) = arguments["sector"].as_str() {
            builder = builder.sector(sector);
        }
        if let Some(status_str) = arguments["status"].as_str() {
            let status = match status_str.to_uppercase().as_str() {
                "ACTIVE" => OrgStatus::Active,
                "INACTIVE" => OrgStatus::Inactive,
                "PENDING_KYB" => OrgStatus::PendingKyb,
                _ => OrgStatus::Active, // Default fallback
            };
            builder = builder.status(status);
        }

        builder
            .send()
            .await
            .map(|_| json!({"status": "updated"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "update_organisation"
    }
}