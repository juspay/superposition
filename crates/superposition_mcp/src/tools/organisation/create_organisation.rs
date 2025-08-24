use crate::mcp_service::{McpService, Tool};
use crate::tools::MCPTool;
use serde_json::{json, Value};
use std::error::Error;

pub struct CreateOrganisationTool;

impl MCPTool for CreateOrganisationTool {
    fn get_definition() -> Tool {
        Tool {
            name: "create_organisation".to_string(),
            description: "Create a new organisation".to_string(),
            input_schema: json!({
                "type": "object",
                "properties": {
                    "name": {"type": "string", "description": "Organisation name"},
                    "admin_email": {"type": "string", "description": "Admin email for the organisation"},
                    "contact_email": {"type": "string", "description": "Contact email"},
                    "contact_phone": {"type": "string", "description": "Contact phone number"},
                    "country_code": {"type": "string", "description": "Country code"},
                    "sector": {"type": "string", "description": "Business sector"}
                },
                "required": ["name", "admin_email"]
            }),
        }
    }

    async fn execute(
        service: &McpService,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        let name = arguments["name"].as_str().unwrap_or("");
        let admin_email = arguments["admin_email"].as_str().unwrap_or("");

        let mut builder = service
            .superposition_client
            .create_organisation()
            .name(name)
            .admin_email(admin_email);

        if let Some(contact_email) = arguments["contact_email"].as_str() {
            builder = builder.contact_email(contact_email);
        }
        if let Some(contact_phone) = arguments["contact_phone"].as_str() {
            builder = builder.contact_phone(contact_phone);
        }
        if let Some(country_code) = arguments["country_code"].as_str() {
            builder = builder.country_code(country_code);
        }
        if let Some(sector) = arguments["sector"].as_str() {
            builder = builder.sector(sector);
        }

        builder
            .send()
            .await
            .map(|_| json!({"status": "created"}))
            .map_err(|e| format!("SDK error: {}", e).into())
    }

    fn name() -> &'static str {
        "create_organisation"
    }
}