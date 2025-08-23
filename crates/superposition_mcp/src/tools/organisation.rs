use super::ToolsModule;
use crate::mcp_service::{McpService, Tool};
use serde_json::{json, Value};
use std::error::Error;
use superposition_sdk::types::OrgStatus;

pub struct OrganisationTools;

impl ToolsModule for OrganisationTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "create_organisation".to_string(),
                description: "Create a new organisation".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "name": {"type": "string", "description": "Organisation name"},
                        "admin_email": {"type": "string", "description": "Admin email for the organisation"},
                        "country_code": {"type": "string", "description": "Country code"},
                        "contact_email": {"type": "string", "description": "Contact email"},
                        "contact_phone": {"type": "string", "description": "Contact phone number"},
                        "sector": {"type": "string", "description": "Business sector"}
                    },
                    "required": ["name", "admin_email"]
                }),
            },
            Tool {
                name: "get_organisation".to_string(),
                description: "Get organisation details".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "id": {"type": "string", "description": "Organisation ID"}
                    },
                    "required": ["id"]
                }),
            },
            Tool {
                name: "list_organisation".to_string(),
                description: "List all organisations".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "count": {"type": "integer", "description": "Number of items per page"},
                        "page": {"type": "integer", "description": "Page number"},
                        "all": {"type": "boolean", "description": "Fetch all results"}
                    }
                }),
            },
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
            },
        ]
    }

    async fn execute_tool(
        service: &McpService,
        tool_name: &str,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        match tool_name {
            "create_organisation" => {
                let name = arguments["name"].as_str().unwrap_or("");
                let admin_email = arguments["admin_email"].as_str().unwrap_or("");
                
                let mut builder = service
                    .superposition_client
                    .create_organisation()
                    .name(name)
                    .admin_email(admin_email);

                // Optional parameters
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
            "get_organisation" => {
                let id = arguments["id"].as_str().unwrap_or("");
                service
                    .superposition_client
                    .get_organisation()
                    .id(id)
                    .send()
                    .await
                    .map(|_| json!({"status": "found"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_organisation" => {
                let mut builder = service
                    .superposition_client
                    .list_organisation();

                // Optional pagination parameters
                if let Some(count) = arguments["count"].as_i64() {
                    builder = builder.count(count as i32);
                }
                if let Some(page) = arguments["page"].as_i64() {
                    builder = builder.page(page as i32);
                }
                if let Some(all) = arguments["all"].as_bool() {
                    builder = builder.all(all);
                }

                builder
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Organisations found"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_organisation" => {
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
            _ => Err(format!("Unknown organisation tool: {}", tool_name).into()),
        }
    }
}
