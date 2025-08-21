use aws_smithy_types::Document;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::error::Error;
use std::io::{self, stdin, BufRead, BufReader, Write};
use superposition_sdk::types::{ExperimentType, MergeStrategy, Variant, VariantType};
use superposition_sdk::{Client, Config};
use tokio;

// Utility function to convert Document to serde_json::Value
fn document_to_value(doc: &Document) -> Value {
    match doc {
        Document::Object(map) => {
            let mut json_map = serde_json::Map::new();
            for (key, value) in map {
                json_map.insert(key.clone(), document_to_value(value));
            }
            Value::Object(json_map)
        }
        Document::Array(arr) => Value::Array(arr.iter().map(document_to_value).collect()),
        Document::Number(num) => {
            // Convert aws_smithy_types::Number to serde_json Value
            match num {
                aws_smithy_types::Number::PosInt(i) => Value::Number(serde_json::Number::from(*i)),
                aws_smithy_types::Number::NegInt(i) => Value::Number(serde_json::Number::from(*i)),
                aws_smithy_types::Number::Float(f) => Value::Number(
                    serde_json::Number::from_f64(*f).unwrap_or(serde_json::Number::from(0)),
                ),
            }
        }
        Document::String(s) => Value::String(s.clone()),
        Document::Bool(b) => Value::Bool(*b),
        Document::Null => Value::Null,
    }
}

// Utility function to convert serde_json::Value to Document
fn value_to_document(value: &Value) -> Document {
    match value {
        Value::Object(map) => {
            let mut doc_map = std::collections::HashMap::new();
            for (key, val) in map {
                doc_map.insert(key.clone(), value_to_document(val));
            }
            Document::Object(doc_map)
        }
        Value::Array(arr) => {
            let doc_arr: Vec<Document> = arr.iter().map(value_to_document).collect();
            Document::Array(doc_arr)
        }
        Value::Number(num) => {
            if let Some(i) = num.as_i64() {
                if i >= 0 {
                    Document::Number(aws_smithy_types::Number::PosInt(i as u64))
                } else {
                    Document::Number(aws_smithy_types::Number::NegInt(i))
                }
            } else if let Some(i) = num.as_u64() {
                Document::Number(aws_smithy_types::Number::PosInt(i))
            } else if let Some(f) = num.as_f64() {
                Document::Number(aws_smithy_types::Number::Float(f))
            } else {
                Document::Null
            }
        }
        Value::String(s) => Document::String(s.clone()),
        Value::Bool(b) => Document::Bool(*b),
        Value::Null => Document::Null,
    }
}

fn value_to_hashmap(value: Value) -> Option<HashMap<String, Document>> {
    match value {
        Value::Object(map) => Some(
            map.into_iter()
                .map(|(key, val)| (key, value_to_document(&val)))
                .collect(),
        ),
        _ => None,
    }
}

// MCP Protocol Types
#[derive(Debug, Serialize, Deserialize)]
struct JsonRpcRequest {
    jsonrpc: String,
    id: Option<Value>,
    method: String,
    params: Option<Value>,
}

#[derive(Debug, Serialize, Deserialize)]
struct JsonRpcResponse {
    jsonrpc: String,
    id: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<JsonRpcError>,
}

#[derive(Debug, Serialize, Deserialize)]
struct JsonRpcError {
    code: i32,
    message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    data: Option<Value>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Tool {
    name: String,
    description: String,
    #[serde(rename = "inputSchema")]
    input_schema: Value,
}

#[derive(Debug, Serialize, Deserialize)]
struct Resource {
    uri: String,
    name: String,
    description: Option<String>,
    #[serde(rename = "mimeType")]
    mime_type: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ServerCapabilities {
    tools: Option<ToolsCapability>,
    resources: Option<ResourcesCapability>,
    prompts: Option<PromptsCapability>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ToolsCapability {
    #[serde(rename = "listChanged")]
    list_changed: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ResourcesCapability {
    subscribe: Option<bool>,
    #[serde(rename = "listChanged")]
    list_changed: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize)]
struct PromptsCapability {
    #[serde(rename = "listChanged")]
    list_changed: Option<bool>,
}

// MCP Server implementation
struct McpServer {
    superposition_client: Client,
    server_info: HashMap<String, Value>,
    workspace_id: String,
    org_id: String,
}

impl McpServer {
    async fn new(
        workspace: String,
        org: String,
        host: String,
        token: String,
    ) -> Result<Self, Box<dyn Error>> {
        let mut server_info = HashMap::new();
        server_info.insert("name".to_string(), json!("superposition-mcp-server"));
        server_info.insert("version".to_string(), json!("1.0.0"));

        let config = Config::builder()
            .endpoint_url(&host)
            .bearer_token(token.clone().into())
            .build();
        let client = Client::from_conf(config);

        Ok(Self {
            superposition_client: client,
            server_info,
            workspace_id: workspace,
            org_id: org,
        })
    }

    async fn handle_request(&self, request: JsonRpcRequest) -> JsonRpcResponse {
        match request.method.as_str() {
            "initialize" => self.handle_initialize(request.id),
            "tools/list" => self.handle_tools_list(request.id).await,
            "tools/call" => self.handle_tools_call(request.id, request.params).await,
            "resources/list" => self.handle_resources_list(request.id).await,
            "resources/read" => self.handle_resources_read(request.id, request.params).await,
            "prompts/list" => self.handle_prompts_list(request.id),
            _ => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32601,
                    message: "Method not found".to_string(),
                    data: None,
                }),
            },
        }
    }

    fn handle_initialize(&self, id: Option<Value>) -> JsonRpcResponse {
        let capabilities = ServerCapabilities {
            tools: Some(ToolsCapability {
                list_changed: Some(false),
            }),
            resources: Some(ResourcesCapability {
                subscribe: Some(false),
                list_changed: Some(false),
            }),
            prompts: Some(PromptsCapability {
                list_changed: Some(false),
            }),
        };

        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(json!({
                "protocolVersion": "2025-06-18",
                "capabilities": capabilities,
                "serverInfo": self.server_info
            })),
            error: None,
        }
    }

    async fn handle_tools_list(&self, id: Option<Value>) -> JsonRpcResponse {
        let tools = vec![
            // Configuration Resolution
            Tool {
                name: "get_resolved_config".to_string(),
                description: "Get resolved configuration based on context using Superposition CAC"
                    .to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
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
                    "required": ["context"]
                }),
            },
            // Default Config Operations
            Tool {
                name: "get_default_config".to_string(),
                description: "Get all default configurations or a specific config by key"
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
            Tool {
                name: "create_default_config".to_string(),
                description: "Create a new default configuration".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
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
                            "description": "description of the key"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "reason for adding for this key"
                        }
                    },
                    "required": ["key", "value", "schema", "description", "change_reason"]
                }),
            },
            Tool {
                name: "update_default_config".to_string(),
                description: "Update an existing default configuration".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
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
                    "required": ["key", "value", "change_reason"]
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
            // Dimension Operations
            Tool {
                name: "get_dimensions".to_string(),
                description: "Get all dimensions defined in Superposition".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
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
            // Context Operations
            Tool {
                name: "get_contexts".to_string(),
                description: "Get all contexts defined in Superposition".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "create_context".to_string(),
                description: "Create a new context with conditions".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context_id": {
                            "type": "string",
                            "description": "Unique identifier for the context"
                        },
                        "condition": {
                            "type": "object",
                            "description": "Condition expression for the context"
                        },
                        "override_with_keys": {
                            "type": "object",
                            "description": "List of configuration keys that can be overridden in this context"
                        },
                        "description": {
                            "type": "string",
                            "description": "Description of the context"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for creating this context"
                        }
                    },
                    "required": ["context_id", "condition", "override_with_keys", "description", "change_reason"]
                }),
            },
            Tool {
                name: "update_context".to_string(),
                description: "Update an existing context".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context_id": {
                            "type": "string",
                            "description": "Context ID to update"
                        },
                        "condition": {
                            "type": "object",
                            "description": "New condition expression"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for updating this context"
                        }
                    },
                    "required": ["context_id", "condition", "change_reason"]
                }),
            },
            Tool {
                name: "delete_context".to_string(),
                description: "Delete a context".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context_id": {
                            "type": "string",
                            "description": "Context ID to delete"
                        }
                    },
                    "required": ["context_id"]
                }),
            },
            // Experiment Operations
            Tool {
                name: "get_experiments".to_string(),
                description: "Get all experiments".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "get_experiment".to_string(),
                description: "Get a specific experiment by ID".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {
                            "type": "string",
                            "description": "ID of the experiment to retrieve"
                        }
                    },
                    "required": ["experiment_id"]
                }),
            },
            Tool {
                name: "create_experiment".to_string(),
                description: "Create a new A/B experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_name": {
                            "type": "string",
                            "description": "Name of the experiment"
                        },
                        "context": {
                            "type": "object",
                            "description": "Context conditions for the experiment"
                        },
                        "variants": {
                            "type": "array",
                            "items": {"type": "object"},
                            "description": "List of experiment variants (control and experimental)"
                        },
                        "description": {
                            "type": "string",
                            "description": "Description of the experiment"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for creating this experiment"
                        }
                    },
                    "required": ["experiment_name", "context", "variants", "description", "change_reason"]
                }),
            },
            Tool {
                name: "conclude_experiment".to_string(),
                description: "Conclude an experiment by choosing a winning variant".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {
                            "type": "string",
                            "description": "ID of the experiment to conclude"
                        },
                        "chosen_variant": {
                            "type": "string",
                            "description": "ID of the winning variant"
                        }
                    },
                    "required": ["experiment_id", "chosen_variant"]
                }),
            },
            Tool {
                name: "update_experiment".to_string(),
                description: "Update experiment variant overrides".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {
                            "type": "string",
                            "description": "ID of the experiment to update"
                        },
                        "variants": {
                            "type": "array",
                            "items": {
                                "type": "object",
                                "properties": {
                                    "id": {
                                        "type": "string",
                                        "description": "Variant ID"
                                    },
                                    "overrides": {
                                        "type": "object",
                                        "description": "Override values for this variant"
                                    }
                                },
                                "required": ["id", "overrides"]
                            },
                            "description": "List of variants to update with their override values"
                        },
                        "description": {
                            "type": "string",
                            "description": "Description of the update"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for updating this experiment"
                        }
                    },
                    "required": ["experiment_id", "variants", "change_reason"]
                }),
            },
            // Function/Validator Operations
            Tool {
                name: "get_functions".to_string(),
                description: "Get all custom validation functions".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "create_function".to_string(),
                description: "Create a new custom validation function".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "function_name": {
                            "type": "string",
                            "description": "Name of the function"
                        },
                        "function_code": {
                            "type": "string",
                            "description": "JavaScript code for the validation function"
                        },
                        "description": {
                            "type": "string",
                            "description": "Description of the function"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for creating this function"
                        }
                    },
                    "required": ["function_name", "function_code", "description", "change_reason"]
                }),
            },
            // Additional Function Operations
            Tool {
                name: "get_function".to_string(),
                description: "Get a specific function by name".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "function_name": {
                            "type": "string",
                            "description": "Name of the function to retrieve"
                        }
                    },
                    "required": ["function_name"]
                }),
            },
            Tool {
                name: "update_function".to_string(),
                description: "Update an existing function".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "function_name": {
                            "type": "string",
                            "description": "Name of the function to update"
                        },
                        "function_code": {
                            "type": "string",
                            "description": "Updated JavaScript code for the function"
                        },
                        "description": {
                            "type": "string",
                            "description": "Updated description of the function"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for updating this function"
                        }
                    },
                    "required": ["function_name", "function_code", "description", "change_reason"]
                }),
            },
            Tool {
                name: "delete_function".to_string(),
                description: "Delete a function".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "function_name": {
                            "type": "string",
                            "description": "Name of the function to delete"
                        }
                    },
                    "required": ["function_name"]
                }),
            },
            // Config Operations
            Tool {
                name: "get_config".to_string(),
                description: "Get configuration for a specific key or all configs".to_string(),
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
            // Dimension Operations  
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
            // Context Operations
            Tool {
                name: "get_context".to_string(),
                description: "Get a specific context".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context_id": {
                            "type": "string",
                            "description": "ID of the context to retrieve"
                        }
                    },
                    "required": ["context_id"]
                }),
            },
            Tool {
                name: "list_contexts".to_string(),
                description: "List all contexts".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "move_context".to_string(),
                description: "Move a context to a different position".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "context_id": {
                            "type": "string",
                            "description": "ID of the context to move"
                        },
                        "dest_position": {
                            "type": "integer",
                            "description": "Destination position for the context"
                        }
                    },
                    "required": ["context_id", "dest_position"]
                }),
            },
            // Experiment Management Operations
            Tool {
                name: "pause_experiment".to_string(),
                description: "Pause a running experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {
                            "type": "string",
                            "description": "ID of the experiment to pause"
                        }
                    },
                    "required": ["experiment_id"]
                }),
            },
            Tool {
                name: "resume_experiment".to_string(),
                description: "Resume a paused experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {
                            "type": "string",
                            "description": "ID of the experiment to resume"
                        }
                    },
                    "required": ["experiment_id"]
                }),
            },
            Tool {
                name: "ramp_experiment".to_string(),
                description: "Ramp up experiment traffic".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {
                            "type": "string",
                            "description": "ID of the experiment to ramp"
                        },
                        "traffic_percentage": {
                            "type": "integer",
                            "description": "Traffic percentage to ramp to"
                        }
                    },
                    "required": ["experiment_id", "traffic_percentage"]
                }),
            },
            Tool {
                name: "discard_experiment".to_string(),
                description: "Discard an experiment".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "experiment_id": {
                            "type": "string",
                            "description": "ID of the experiment to discard"
                        }
                    },
                    "required": ["experiment_id"]
                }),
            },
            // Type Operations
            Tool {
                name: "get_types".to_string(),
                description: "Get all custom types defined".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {}
                }),
            },
            Tool {
                name: "create_type".to_string(),
                description: "Create a new custom type".to_string(),
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "type_name": {
                            "type": "string",
                            "description": "Name of the custom type"
                        },
                        "type_schema": {
                            "type": "object",
                            "description": "JSON schema defining the type"
                        },
                        "description": {
                            "type": "string",
                            "description": "Description of the type"
                        },
                        "change_reason": {
                            "type": "string",
                            "description": "Reason for creating this type"
                        }
                    },
                    "required": ["type_name", "type_schema", "description", "change_reason"]
                }),
            },
        ];

        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(json!({ "tools": tools })),
            error: None,
        }
    }

    async fn handle_tools_call(&self, id: Option<Value>, params: Option<Value>) -> JsonRpcResponse {
        let params = match params {
            Some(p) => p,
            None => {
                return JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    id,
                    result: None,
                    error: Some(JsonRpcError {
                        code: -32602,
                        message: "Invalid params".to_string(),
                        data: None,
                    }),
                };
            }
        };

        let tool_name = params["name"].as_str().unwrap_or("");
        let arguments = &params["arguments"];

        let result: Result<Value, Box<dyn Error>> = match tool_name {
            "get_resolved_config" => {
                let empty_map = serde_json::Map::new();
                let context = arguments["context"].as_object().unwrap_or(&empty_map);
                let prefix = arguments["prefix"].as_str();
                let merge_strategy = arguments["merge_strategy"].as_str().unwrap_or("MERGE");

                let strategy = match merge_strategy {
                    "REPLACE" => MergeStrategy::Replace,
                    _ => MergeStrategy::Merge,
                };

                let mut builder = self
                    .superposition_client
                    .get_resolved_config()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
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
                let key = arguments["key"].as_str();
                if let Some(k) = key {
                    self.superposition_client
                        .get_config()
                        .workspace_id(&self.workspace_id)
                        .org_id(&self.org_id)
                        .prefix(k)
                        .send()
                        .await
                        .map(|output| {
                            // For get_config, we want to return the default_configs HashMap
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
                    self.superposition_client
                        .list_default_configs()
                        .workspace_id(&self.workspace_id)
                        .org_id(&self.org_id)
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

                println!(
                    "org_id: {}, workspace_id: {}, key: {}, value: {}, schema: {}",
                    self.org_id, self.workspace_id, key, value, schema
                );

                self.superposition_client
                    .create_default_config()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
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
                let key = arguments["key"].as_str().unwrap_or("");
                let value = arguments["value"].clone();
                let change_reason = arguments["change_reason"]
                    .as_str()
                    .unwrap_or("Updated via MCP server");

                let value_doc = value_to_document(&value);

                self.superposition_client
                    .update_default_config()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .key(key)
                    .value(value_doc)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "updated", "key": key}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "delete_default_config" => {
                let key = arguments["key"].as_str().unwrap_or("");
                self.superposition_client
                    .delete_default_config()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .key(key)
                    .send()
                    .await
                    .map(|_| json!({"status": "deleted", "key": key}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_dimensions" => {
                // List all dimensions - we'll need to implement this differently as the SDK has get_dimension (singular)
                Err(format!("get_dimensions not directly supported by SDK - use get_dimension with specific dimension name").into())
            }
            "create_dimension" => {
                let dimension = arguments["dimension"].as_str().unwrap_or("");
                let schema = arguments["schema"].clone();
                let description = arguments["description"]
                    .as_str()
                    .unwrap_or("no description");
                let change_reason = arguments["change_reason"]
                    .as_str()
                    .unwrap_or("no change_reason");
                let position64 = arguments["position"].as_i64().unwrap_or(1);
                let position = position64.try_into().unwrap();

                let schema_doc = value_to_document(&schema);

                self.superposition_client
                    .create_dimension()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
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
            "get_contexts" => {
                // List all contexts - SDK has get_context (singular)
                Err(format!("get_contexts not directly supported by SDK - use get_context with specific context_id").into())
            }
            "create_context" => {
                let condition = arguments["condition"].clone();
                let condition_doc = value_to_hashmap(condition);
                let override_with_keys = value_to_hashmap(arguments["override_with_keys"].clone());
                let description = arguments["description"]
                    .as_str()
                    .unwrap_or("no description");
                let change_reason = arguments["change_reason"]
                    .as_str()
                    .unwrap_or("no change_reason");

                self.superposition_client
                    .create_context()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .description(description)
                    .change_reason(change_reason)
                    .set_context(condition_doc)
                    .set_override(override_with_keys)
                    .send()
                    .await
                    .map(|_| json!({"status": "created context"}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_context" => {
                // update_context method doesn't exist in the SDK
                Err(format!("update_context not available in current SDK version").into())
            }
            "delete_context" => {
                let context_id = arguments["context_id"].as_str().unwrap_or("");
                // Looking at the API, delete_context uses a different parameter pattern
                self.superposition_client
                    .delete_context()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "deleted", "context_id": context_id}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_experiments" => {
                // List experiments - SDK has get_experiment (singular) and list_experiment
                self.superposition_client
                    .list_experiment()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .send()
                    .await
                    .map(|output| {
                        // ExperimentResponse doesn't implement Serialize, so we'll return a simple message
                        json!({
                            "count": output.data().len(),
                            "message": "Experiments found (detailed data not available due to serialization constraints)"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_experiment" => {
                let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
                
                self.superposition_client
                    .get_experiment()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .id(experiment_id)
                    .send()
                    .await
                    .map(|output| {
                        let variants: Vec<Value> = output.variants().iter().map(|variant| {
                            json!({
                                "id": variant.id(),
                                "variant_type": format!("{:?}", variant.variant_type()),
                                "overrides": document_to_value(variant.overrides())
                            })
                        }).collect();
                        
                        let context_map: serde_json::Map<String, Value> = output.context().iter().map(|(k, v)| {
                            (k.clone(), document_to_value(v))
                        }).collect();
                        
                        json!({
                            "experiment_id": output.id(),
                            "name": output.name(),
                            "description": output.description(),
                            "context": Value::Object(context_map),
                            "variants": variants,
                            "status": format!("{:?}", output.status()),
                            "experiment_type": format!("{:?}", output.experiment_type()),
                            "created_at": output.created_at().to_string(),
                            "created_by": output.created_by(),
                            "last_modified": output.last_modified().to_string(),
                            "last_modified_by": output.last_modified_by(),
                            "change_reason": output.change_reason(),
                            "traffic_percentage": output.traffic_percentage(),
                            "override_keys": output.override_keys()
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "create_experiment" => {
                async move {
                    let experiment_name = arguments["experiment_name"].as_str().unwrap_or("");
                    let context = arguments["context"].clone();
                    let empty_vec = vec![];
                    let variants = arguments["variants"].as_array().unwrap_or(&empty_vec);
                    let description = arguments["description"]
                        .as_str()
                        .unwrap_or("no description");
                    let change_reason = arguments["change_reason"]
                        .as_str()
                        .unwrap_or("no change_reason");

                    let context_doc = value_to_hashmap(context);

                    let mut variant_objects = Vec::new();
                    for (index, variant_value) in variants.iter().enumerate() {
                        let default_id = format!("variant_{}", index);
                        let variant_id = variant_value["id"].as_str().unwrap_or(&default_id);

                        let variant_type_str = variant_value["variant_type"]
                            .as_str()
                            .or_else(|| {
                                // If variant_type is not provided, infer from id
                                if variant_id.to_lowercase().contains("control") {
                                    Some("CONTROL")
                                } else {
                                    Some("EXPERIMENTAL")
                                }
                            })
                            .unwrap_or("EXPERIMENTAL");

                        let variant_type = match variant_type_str {
                            "CONTROL" => VariantType::Control,
                            _ => VariantType::Experimental,
                        };

                        let overrides = if let Some(overrides) = variant_value.get("overrides") {
                            value_to_document(overrides)
                        } else {
                            value_to_document(&json!({}))
                        };

                        let variant = Variant::builder()
                            .id(variant_id)
                            .variant_type(variant_type)
                            .overrides(overrides)
                            .build()
                            .map_err(|e| format!("Failed to build variant: {}", e))?;

                        variant_objects.push(variant);
                    }

                    let mut builder = self
                        .superposition_client
                        .create_experiment()
                        .workspace_id(&self.workspace_id)
                        .org_id(&self.org_id)
                        .name(experiment_name)
                        .experiment_type(ExperimentType::Default)
                        .description(description)
                        .change_reason(change_reason);

                    if let Some(context_map) = context_doc {
                        builder = builder.set_context(Some(context_map));
                    }

                    builder = builder.set_variants(Some(variant_objects));

                    builder
                        .send()
                        .await
                        .map(|_| json!({"status": "created", "experiment_name": experiment_name}))
                        .map_err(|e| format!("SDK error: {}", e).into())
                }
                .await
            }
            "conclude_experiment" => {
                let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
                let chosen_variant = arguments["chosen_variant"].as_str().unwrap_or("");

                // The parameters might be different for conclude_experiment
                self.superposition_client
                    .conclude_experiment()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .send()
                    .await
                    .map(|_| json!({"status": "concluded", "experiment_id": experiment_id, "chosen_variant": chosen_variant}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_experiment" => {
                async move {
                    let experiment_id = arguments["experiment_id"].as_str().unwrap_or("");
                    let empty_vec = vec![];
                    let variants = arguments["variants"].as_array().unwrap_or(&empty_vec);
                    let description = arguments["description"].as_str();
                    let change_reason = arguments["change_reason"]
                        .as_str()
                        .unwrap_or("Updated via MCP server");

                    let mut variant_updates = Vec::new();
                    for variant_value in variants.iter() {
                        let variant_id = variant_value["id"].as_str().unwrap_or("");
                        let overrides = variant_value["overrides"].clone();
                        let overrides_doc = value_to_document(&overrides);

                        let variant_update =
                            superposition_sdk::types::VariantUpdateRequest::builder()
                                .id(variant_id)
                                .overrides(overrides_doc)
                                .build()
                                .map_err(|e| format!("Failed to build variant update: {}", e))?;

                        variant_updates.push(variant_update);
                    }

                    let mut builder = self
                        .superposition_client
                        .update_overrides_experiment()
                        .workspace_id(&self.workspace_id)
                        .org_id(&self.org_id)
                        .id(experiment_id)
                        .change_reason(change_reason);

                    if let Some(desc) = description {
                        builder = builder.description(desc);
                    }

                    builder = builder.set_variant_list(Some(variant_updates));

                    builder
                        .send()
                        .await
                        .map(|_| json!({"status": "updated", "experiment_id": experiment_id}))
                        .map_err(|e| format!("SDK error: {}", e).into())
                }
                .await
            }
            "get_functions" => {
                // List functions
                self.superposition_client
                    .list_function()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .send()
                    .await
                    .map(|output| {
                        // FunctionResponse doesn't implement Serialize, so we'll return a simple message
                        json!({
                            "count": output.data().len(),
                            "message": "Functions found (detailed data not available due to serialization constraints)"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "create_function" => {
                let function_name = arguments["function_name"].as_str().unwrap_or("");
                let function_code = arguments["function_code"].as_str().unwrap_or("");
                let description = arguments["description"]
                    .as_str()
                    .unwrap_or("no description");
                let change_reason = arguments["change_reason"]
                    .as_str()
                    .unwrap_or("no change_reason");

                self.superposition_client
                    .create_function()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .function_name(function_name)
                    .function(function_code)
                    .description(description)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "created", "function_name": function_name}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "get_types" => {
                // Note: get_types might not be available in current SDK
                Err(format!("get_types not available in current SDK version").into())
            }
            "create_type" => {
                // Note: create_type might not be available in current SDK, only create_type_templates
                Err(format!(
                    "create_type not available in current SDK version - use create_type_templates"
                )
                .into())
            }
            // Additional Function Operations
            "get_function" => {
                let function_name = arguments["function_name"].as_str().unwrap_or("");
                self.superposition_client
                    .get_function()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .function_name(function_name)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "function_name": output.function_name(),
                            "published_code": output.published_code().unwrap_or_default(),
                            "draft_code": output.draft_code(),
                            "description": output.description(),
                            "published_at": output.published_at().map(|dt| dt.to_string()).unwrap_or_default(),
                            "function_type": format!("{:?}", output.function_type())
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "update_function" => {
                let function_name = arguments["function_name"].as_str().unwrap_or("");
                let function_code = arguments["function_code"].as_str().unwrap_or("");
                let description = arguments["description"].as_str().unwrap_or("no description");
                let change_reason = arguments["change_reason"].as_str().unwrap_or("no change_reason");

                self.superposition_client
                    .update_function()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .function_name(function_name)
                    .function(function_code)
                    .description(description)
                    .change_reason(change_reason)
                    .send()
                    .await
                    .map(|_| json!({"status": "updated", "function_name": function_name}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "delete_function" => {
                let function_name = arguments["function_name"].as_str().unwrap_or("");
                self.superposition_client
                    .delete_function()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .function_name(function_name)
                    .send()
                    .await
                    .map(|_| json!({"status": "deleted", "function_name": function_name}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            // Config Operations
            "get_config" => {
                let key = arguments["key"].as_str();
                if let Some(k) = key {
                    self.superposition_client
                        .get_config()
                        .workspace_id(&self.workspace_id)
                        .org_id(&self.org_id)
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
                    self.superposition_client
                        .get_config()
                        .workspace_id(&self.workspace_id)
                        .org_id(&self.org_id)
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
            // Dimension Operations
            "get_dimension" => {
                let dimension = arguments["dimension"].as_str().unwrap_or("");
                self.superposition_client
                    .get_dimension()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
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
                self.superposition_client
                    .list_dimensions()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
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

                self.superposition_client
                    .update_dimension()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
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
                self.superposition_client
                    .delete_dimension()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .dimension(dimension)
                    .send()
                    .await
                    .map(|_| json!({"status": "deleted", "dimension": dimension}))
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            // Context Operations
            "get_context" => {
                let context_id = arguments["context_id"].as_str().unwrap_or("");
                self.superposition_client
                    .get_context()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .id(context_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "id": output.id(),
                            "message": "Context found (detailed output serialization needs to be implemented)"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "list_contexts" => {
                self.superposition_client
                    .list_contexts()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Contexts found (detailed data not available due to serialization constraints)"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "move_context" => {
                // Note: move_context parameters need to be checked for correct SDK usage
                Err(format!("move_context implementation needs parameter verification").into())
            }
            // Experiment Management Operations - Note: parameter names need verification
            "pause_experiment" => {
                // Note: pause_experiment parameters need to be checked for correct SDK usage
                Err(format!("pause_experiment implementation needs parameter verification").into())
            }
            "resume_experiment" => {
                // Note: resume_experiment parameters need to be checked for correct SDK usage
                Err(format!("resume_experiment implementation needs parameter verification").into())
            }
            "ramp_experiment" => {
                // Note: ramp_experiment parameters need to be checked for correct SDK usage
                Err(format!("ramp_experiment implementation needs parameter verification").into())
            }
            "discard_experiment" => {
                // Note: discard_experiment parameters need to be checked for correct SDK usage
                Err(format!("discard_experiment implementation needs parameter verification").into())
            }
            _ => Err(format!("Unknown tool").into()),
        };

        match result {
            Ok(content) => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id,
                result: Some(json!({
                    "content": [
                        {
                            "type": "text",
                            "text": serde_json::to_string_pretty(&content).unwrap_or_else(|_| content.to_string())
                        }
                    ]
                })),
                error: None,
            },
            Err(e) => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32603,
                    message: format!("Internal error: {}", e),
                    data: None,
                }),
            },
        }
    }

    async fn handle_resources_list(&self, id: Option<Value>) -> JsonRpcResponse {
        let resources = vec![
            Resource {
                uri: "superposition://config".to_string(),
                name: "Current Configuration".to_string(),
                description: Some("Current resolved configuration state".to_string()),
                mime_type: Some("application/json".to_string()),
            },
            Resource {
                uri: "superposition://default-config".to_string(),
                name: "Default Configuration".to_string(),
                description: Some("All default configurations".to_string()),
                mime_type: Some("application/json".to_string()),
            },
            Resource {
                uri: "superposition://dimensions".to_string(),
                name: "Dimensions".to_string(),
                description: Some("All defined dimensions".to_string()),
                mime_type: Some("application/json".to_string()),
            },
            Resource {
                uri: "superposition://contexts".to_string(),
                name: "Contexts".to_string(),
                description: Some("All defined contexts".to_string()),
                mime_type: Some("application/json".to_string()),
            },
            Resource {
                uri: "superposition://experiments".to_string(),
                name: "Experiments".to_string(),
                description: Some("All experiments".to_string()),
                mime_type: Some("application/json".to_string()),
            },
        ];

        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(json!({ "resources": resources })),
            error: None,
        }
    }

    async fn handle_resources_read(
        &self,
        id: Option<Value>,
        params: Option<Value>,
    ) -> JsonRpcResponse {
        let params = match params {
            Some(p) => p,
            None => {
                return JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    id,
                    result: None,
                    error: Some(JsonRpcError {
                        code: -32602,
                        message: "Invalid params".to_string(),
                        data: None,
                    }),
                };
            }
        };

        let uri = params["uri"].as_str().unwrap_or("");

        let result: Result<Value, Box<dyn Error>> = match uri {
            "superposition://default-config" => {
                self.superposition_client
                    .list_default_configs()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .all(true)
                    .send()
                    .await
                    .map(|output| {
                        let configs: Vec<Value> = output.data().iter().map(|config| {
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
                        }).collect();
                        json!(configs)
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            "superposition://dimensions" => {
                Err(format!("List dimensions not directly supported - use get_dimension with specific dimension name").into())
            }
            "superposition://contexts" => {
                Err(format!("List contexts not directly supported - use get_context with specific context_id").into())
            }
            "superposition://experiments" => {
                self.superposition_client
                    .list_experiment()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .send()
                    .await
                    .map(|output| {
                        // ExperimentResponse doesn't implement Serialize, so we'll return a simple message
                        json!({
                            "count": output.data().len(),
                            "message": "Experiments found (detailed data not available due to serialization constraints)"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown resource").into()),
        };

        match result {
            Ok(content) => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id,
                result: Some(json!({
                    "contents": [
                        {
                            "uri": uri,
                            "mimeType": "application/json",
                            "text": serde_json::to_string_pretty(&content).unwrap_or_else(|_| content.to_string())
                        }
                    ]
                })),
                error: None,
            },
            Err(e) => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32603,
                    message: format!("Internal error: {}", e),
                    data: None,
                }),
            },
        }
    }

    fn handle_prompts_list(&self, id: Option<Value>) -> JsonRpcResponse {
        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(json!({ "prompts": [] })),
            error: None,
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    // Configuration - you can get these from environment variables
    let workspace =
        std::env::var("SUPERPOSITION_DEFAULT_WORKSPACE").unwrap_or_else(|_| "dev".to_string());
    let org = std::env::var("SUPERPOSITION_DEFAULT_ORG").unwrap_or_else(|_| "localorg".to_string());
    let host =
        std::env::var("SUPERPOSITION_HOST").unwrap_or_else(|_| "http://localhost:8080".to_string());
    let token =
        std::env::var("SUPERPOSITION_DEFAULT_TOKEN").unwrap_or_else(|_| "12345".to_string());

    let server = McpServer::new(workspace, org, host, token).await?;

    eprintln!("MCP Server initialized successfully");

    let stdin = stdin();
    let mut reader = BufReader::new(stdin.lock());

    eprintln!("Starting message loop...");

    // Add error handling for the loop
    loop {
        let mut line = String::new();

        // Read line with proper error handling
        eprintln!("Waiting for next message...");
        match reader.read_line(&mut line) {
            Ok(0) => {
                // EOF reached - client disconnected
                eprintln!("Client disconnected (EOF) - stdin closed");
                // Add a small delay to see if this is immediate
                std::thread::sleep(std::time::Duration::from_millis(100));
                break;
            }
            Ok(_) => {
                // Successfully read a line
                let line = line.trim();

                if line.is_empty() {
                    continue;
                }

                let request: JsonRpcRequest = match serde_json::from_str(line) {
                    Ok(req) => req,
                    Err(e) => {
                        eprintln!("Parse error: {}", e);
                        let error_response = JsonRpcResponse {
                            jsonrpc: "2.0".to_string(),
                            id: None,
                            result: None,
                            error: Some(JsonRpcError {
                                code: -32700,
                                message: format!("Parse error: {}", e),
                                data: None,
                            }),
                        };

                        let response_json = serde_json::to_string(&error_response)?;
                        println!("{}", response_json);
                        io::stdout().flush()?;
                        continue;
                    }
                };

                eprintln!("Processing method: {}", request.method);
                let response = server.handle_request(request).await;
                let response_json = serde_json::to_string(&response)?;

                eprintln!("Sending response: {}", response_json);
                println!("{}", response_json);
                io::stdout().flush()?;
            }
            Err(e) => {
                // Error reading from stdin
                eprintln!("Error reading from stdin: {}", e);
                break;
            }
        }
    }

    eprintln!("MCP server shutting down");
    Ok(())
}
