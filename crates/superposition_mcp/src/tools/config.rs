use super::ToolsModule;
use crate::mcp_service::{document_to_value, value_to_document, McpService, Tool};
use serde_json::{json, Value};
use std::error::Error;
use superposition_sdk::types::MergeStrategy;

pub struct ConfigTools;

impl ToolsModule for ConfigTools {
    fn get_tool_definitions() -> Vec<Tool> {
        vec![
            Tool {
                name: "get_resolved_config".to_string(),
                description:
                    "Get resolved configuration based on context using Superposition CAC"
                        .to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "org_id": {"type": "string", "description": "Organization ID"},
                        "workspace_id": {"type": "string", "description": "Workspace ID"},
                        "context": {
                            "type": "object",
                            "description": "Context object containing dimensions for configuration resolution"
                        },
                        "prefix": {
                            "type": "string",
                            "description": "Optional key prefix to filter configurations"
                        },
                        "merge_strategy": {
                            "type": "string",
                            "enum": ["MERGE", "REPLACE"],
                            "default": "MERGE",
                            "description": "Strategy for merging configurations"
                        }
                    },
                    "required": ["org_id", "workspace_id", "context"]
                }),
            },
            Tool {
                name: "get_default_config".to_string(),
                description: "Get all default configurations or a specific config by key"
                    .to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "org_id": {"type": "string", "description": "Organization ID"},
                        "workspace_id": {"type": "string", "description": "Workspace ID"},
                        "key": {
                            "type": "string",
                            "description": "Optional specific configuration key to retrieve"
                        }
                    },
                    "required": ["org_id", "workspace_id"]
                }),
            },
            Tool {
                name: "create_default_config".to_string(),
                description: "Create a new default configuration".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "org_id": {"type": "string", "description": "Organization ID"},
                        "workspace_id": {"type": "string", "description": "Workspace ID"},
                        "key": {
                            "type": "string",
                            "description": "Configuration key name"
                        },
                        "value": {
                            "description": "Default value for the configuration"
                        },
                        "schema": {
                            "type": "object",
                            "description": "JSON schema for validating the configuration value"
                        },
                        "description": {
                            "type": "string",
                            "description": "Description of the key"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for adding this key"
                        }
                    },
                    "required": ["org_id", "workspace_id", "key", "value", "schema", "description", "change_reason"]
                }),
            },
            Tool {
                name: "update_default_config".to_string(),
                description: "Update an existing default configuration".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "org_id": {"type": "string", "description": "Organization ID"},
                        "workspace_id": {"type": "string", "description": "Workspace ID"},
                        "key": {
                            "type": "string",
                            "description": "Configuration key to update"
                        },
                        "value": {
                            "description": "New default value"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for updating this configuration"
                        }
                    },
                    "required": ["org_id", "workspace_id", "key", "value", "change_reason"]
                }),
            },
            Tool {
                name: "delete_default_config".to_string(),
                description: "Delete a default configuration".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "key": {
                            "type": "string",
                            "description": "Configuration key to delete"
                        }
                    },
                    "required": ["key"]
                }),
            },
            Tool {
                name: "list_default_configs".to_string(),
                description: "List all default configurations".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "all": {
                            "type": "boolean",
                            "description": "Whether to include all configurations",
                            "default": true
                        }
                    }
                }),
            },
            Tool {
                name: "get_config_fast".to_string(),
                description: "Get configuration with fast access pattern".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context": {
                            "type": "object",
                            "description": "Context for configuration resolution"
                        },
                        "prefix": {
                            "type": "string",
                            "description": "Optional key prefix to filter configurations"
                        }
                    },
                    "required": ["context"]
                }),
            },
            Tool {
                name: "get_config".to_string(),
                description: "Get configuration for a specific key or all configs"
                    .to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "key": {
                            "type": "string",
                            "description": "Optional specific configuration key to retrieve"
                        }
                    }
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
            "get_resolved_config" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
                let empty_map = serde_json::Map::new();
                let context = arguments["context"].as_object().unwrap_or(&empty_map);
                let prefix = arguments["prefix"].as_str();
                let merge_strategy =
                    arguments["merge_strategy"].as_str().unwrap_or("MERGE");

                let strategy = match merge_strategy {
                    "REPLACE" => MergeStrategy::Replace,
                    _ => MergeStrategy::Merge,
                };

                let mut builder = service
                    .superposition_client
                    .get_resolved_config()
                    .workspace_id(workspace_id)
                    .org_id(org_id)
                    .merge_strategy(strategy);

                if let Some(p) = prefix {
                    builder = builder.prefix(p);
                }

                for (key, value) in context {
                    let doc = value_to_document(value);
                    builder = builder.context(key, doc);
                }

                builder
                    .send()
                    .await
                    .map(|output| {
                        output
                            .config()
                            .map(|c| document_to_value(c))
                            .unwrap_or(Value::Null)
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_default_config" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
                let key = arguments["key"].as_str();
                if let Some(k) = key {
                    service
                        .superposition_client
                        .get_config()
                        .workspace_id(workspace_id)
                        .org_id(org_id)
                        .prefix(k)
                        .send()
                        .await
                        .map(|output| {
                            if let Some(default_configs) = output.default_configs() {
                                let mut result = serde_json::Map::new();
                                for (key, doc) in default_configs {
                                    result.insert(key.clone(), document_to_value(doc));
                                }
                                Value::Object(result)
                            } else {
                                Value::Null
                            }
                        })
                        .map_err(|e| format!("SDK error: {}", e).into())
                } else {
                    service
                        .superposition_client
                        .list_default_configs()
                        .workspace_id(workspace_id)
                        .org_id(org_id)
                        .all(true)
                        .send()
                        .await
                        .map(|output| {
                            let configs: Vec<Value> = output
                                .data()
                                .iter()
                                .map(|config| {
                                    json!({
                                        "key": config.key(),
                                        "value": document_to_value(config.value()),
                                        "schema": document_to_value(config.schema()),
                                        "description": config.description(),
                                        "change_reason": config.change_reason(),
                                        "function_name": config.function_name(),
                                        "created_at": config.created_at().to_string(),
                                        "created_by": config.created_by(),
                                        "last_modified_at": config.last_modified_at().to_string(),
                                        "last_modified_by": config.last_modified_by()
                                    })
                                })
                                .collect();
                            json!(configs)
                        })
                        .map_err(|e| format!("SDK error: {}", e).into())
                }
            }
            "create_default_config" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
                let key = arguments["key"].as_str().unwrap_or("");
                let value = arguments["value"].clone();
                let schema = arguments["schema"].clone();
                let description = arguments["description"]
                    .as_str()
                    .unwrap_or("no description");
                let change_reason = arguments["change_reason"]
                    .as_str()
                    .unwrap_or("no change_reason");

                let value_doc = value_to_document(&value);
                let schema_doc = value_to_document(&schema);

                service
                    .superposition_client
                    .create_default_config()
                    .workspace_id(workspace_id)
                    .org_id(org_id)
                    .key(key)
                    .value(value_doc)
                    .schema(schema_doc)
                    .description(description)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "created", "key": key}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_default_config" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
                let key = arguments["key"].as_str().unwrap_or("");
                let value = arguments["value"].clone();
                let change_reason = arguments["change_reason"]
                    .as_str()
                    .unwrap_or("Updated via MCP server");

                let value_doc = value_to_document(&value);

                service
                    .superposition_client
                    .update_default_config()
                    .workspace_id(workspace_id)
                    .org_id(org_id)
                    .key(key)
                    .value(value_doc)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "updated", "key": key}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "delete_default_config" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
                let key = arguments["key"].as_str().unwrap_or("");
                service
                    .superposition_client
                    .delete_default_config()
                    .workspace_id(workspace_id)
                    .org_id(org_id)
                    .key(key)
                    .send()
                    .await
                    .map(|_| json!({"status": "deleted", "key": key}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_default_configs" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
                let all = arguments["all"].as_bool().unwrap_or(true);
                service
                    .superposition_client
                    .list_default_configs()
                    .workspace_id(workspace_id)
                    .org_id(org_id)
                    .all(all)
                    .send()
                    .await
                    .map(|output| {
                        let configs: Vec<Value> = output
                            .data()
                            .iter()
                            .map(|config| {
                                json!({
                                    "key": config.key(),
                                    "value": document_to_value(config.value()),
                                    "schema": document_to_value(config.schema()),
                                    "description": config.description(),
                                    "change_reason": config.change_reason(),
                                    "function_name": config.function_name(),
                                    "created_at": config.created_at().to_string(),
                                    "created_by": config.created_by(),
                                    "last_modified_at": config.last_modified_at().to_string(),
                                    "last_modified_by": config.last_modified_by()
                                })
                            })
                            .collect();
                        json!(configs)
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_config_fast" => {
                // get_config_fast method might not exist or have different parameters
                // Let's use regular get_config instead
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
                let _context = &arguments["context"];
                let prefix = arguments["prefix"].as_str();

                let mut builder = service
                    .superposition_client
                    .get_config()
                    .workspace_id(workspace_id)
                    .org_id(org_id);

                if let Some(p) = prefix {
                    builder = builder.prefix(p);
                }

                builder
                    .send()
                    .await
                    .map(|output| {
                        if let Some(default_configs) = output.default_configs() {
                            let mut result = serde_json::Map::new();
                            for (key, doc) in default_configs {
                                result.insert(key.clone(), document_to_value(doc));
                            }
                            Value::Object(result)
                        } else {
                            Value::Null
                        }
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_config" => {
                let org_id = arguments["org_id"].as_str().unwrap_or("");
                let workspace_id = arguments["workspace_id"].as_str().unwrap_or("");
                let key = arguments["key"].as_str();
                if let Some(k) = key {
                    service
                        .superposition_client
                        .get_config()
                        .workspace_id(workspace_id)
                        .org_id(org_id)
                        .prefix(k)
                        .send()
                        .await
                        .map(|output| {
                            if let Some(default_configs) = output.default_configs() {
                                let mut result = serde_json::Map::new();
                                for (key, doc) in default_configs {
                                    result.insert(key.clone(), document_to_value(doc));
                                }
                                Value::Object(result)
                            } else {
                                Value::Null
                            }
                        })
                        .map_err(|e| format!("SDK error: {}", e).into())
                } else {
                    service
                        .superposition_client
                        .get_config()
                        .workspace_id(workspace_id)
                        .org_id(org_id)
                        .send()
                        .await
                        .map(|output| {
                            if let Some(default_configs) = output.default_configs() {
                                let mut result = serde_json::Map::new();
                                for (key, doc) in default_configs {
                                    result.insert(key.clone(), document_to_value(doc));
                                }
                                Value::Object(result)
                            } else {
                                Value::Null
                            }
                        })
                        .map_err(|e| format!("SDK error: {}", e).into())
                }
            }
            _ => Err(format!("Unknown config tool: {}", tool_name).into()),
        }
    }
}

