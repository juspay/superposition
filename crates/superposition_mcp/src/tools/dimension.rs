use serde_json::{json, Value};
use std::error::Error;
use crate::mcp_service::{Tool, McpService, value_to_document, document_to_value};
use super::ToolsModule;

pub struct DimensionTools;

impl ToolsModule for DimensionTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "create_dimension".to_string(),
                description: "Create a new dimension".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "dimension": {
                            "type": "string",
                            "description": "Dimension name"
                        },
                        "schema": {
                            "type": "object",
                            "description": "JSON schema for the dimension"
                        },
                        "position": {
                            "type": "integer",
                            "description": "Priority for the dimension"
                        },
                        "description": {
                            "type": "string",
                            "description": "Description of the dimension"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for creating this dimension"
                        }
                    },
                    "required": ["dimension", "schema", "description", "change_reason"]
                }),
            },
            Tool {
                name: "get_dimension".to_string(),
                description: "Get a specific dimension".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "dimension": {
                            "type": "string",
                            "description": "Name of the dimension to retrieve"
                        }
                    },
                    "required": ["dimension"]
                }),
            },
            Tool {
                name: "list_dimensions".to_string(),
                description: "List all dimensions".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "update_dimension".to_string(),
                description: "Update an existing dimension".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "dimension": {
                            "type": "string",
                            "description": "Name of the dimension to update"
                        },
                        "schema": {
                            "type": "object",
                            "description": "Updated JSON schema for the dimension"
                        },
                        "description": {
                            "type": "string",
                            "description": "Updated description of the dimension"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for updating this dimension"
                        }
                    },
                    "required": ["dimension", "schema", "description", "change_reason"]
                }),
            },
            Tool {
                name: "delete_dimension".to_string(),
                description: "Delete a dimension".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "dimension": {
                            "type": "string",
                            "description": "Name of the dimension to delete"
                        }
                    },
                    "required": ["dimension"]
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
            "create_dimension" => {
                let dimension = arguments["dimension"].as_str().unwrap_or("");
                let schema = arguments["schema"].clone();
                let description = arguments["description"].as_str().unwrap_or("no description");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("no change_reason");
                let position64 = arguments["position"].as_i64().unwrap_or(1);
                let position = position64.try_into().unwrap();

                let schema_doc = value_to_document(&schema);

                service
                    .superposition_client
                    .create_dimension()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .dimension(dimension)
                    .position(position)
                    .schema(schema_doc)
                    .description(description)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "created", "dimension": dimension}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_dimension" => {
                let dimension = arguments["dimension"].as_str().unwrap_or("");
                service
                    .superposition_client
                    .get_dimension()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .dimension(dimension)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "dimension": output.dimension(),
                            "schema": document_to_value(output.schema()),
                            "position": output.position(),
                            "description": output.description(),
                            "created_at": output.created_at().to_string(),
                            "created_by": output.created_by(),
                            "last_modified_at": output.last_modified_at().to_string(),
                            "last_modified_by": output.last_modified_by()
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_dimensions" => {
                service
                    .superposition_client
                    .list_dimensions()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .send()
                    .await
                    .map(|output| {
                        let dimensions: Vec<Value> = output.data().iter().map(|dim| {
                            json!({
                                "dimension": dim.dimension(),
                                "schema": document_to_value(dim.schema()),
                                "position": dim.position(),
                                "description": dim.description(),
                                "created_at": dim.created_at().to_string(),
                                "created_by": dim.created_by(),
                                "last_modified_at": dim.last_modified_at().to_string(),
                                "last_modified_by": dim.last_modified_by()
                            })
                        }).collect();
                        json!(dimensions)
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_dimension" => {
                let dimension = arguments["dimension"].as_str().unwrap_or("");
                let schema = arguments["schema"].clone();
                let description = arguments["description"].as_str().unwrap_or("no description");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("no change_reason");

                let schema_doc = value_to_document(&schema);

                service
                    .superposition_client
                    .update_dimension()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .dimension(dimension)
                    .schema(schema_doc)
                    .description(description)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "updated", "dimension": dimension}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "delete_dimension" => {
                let dimension = arguments["dimension"].as_str().unwrap_or("");
                service
                    .superposition_client
                    .delete_dimension()
                    .workspace_id(&service.workspace_id)
                    .org_id(&service.org_id)
                    .dimension(dimension)
                    .send()
                    .await
                    .map(|_| json!({"status": "deleted", "dimension": dimension}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown dimension tool: {}", tool_name).into()),
        }
    }
}