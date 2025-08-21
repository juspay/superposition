use aws_smithy_types::Document;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::error::Error;
use superposition_sdk::{Client, Config};

// Utility function to convert Document to serde_json::Value
pub fn document_to_value(doc: &Document) -> Value {
    match doc {
        Document::Object(map) => {
            let mut json_map = serde_json::Map::new();
            for (key, value) in map {
                json_map.insert(key.clone(), document_to_value(value));
            }
            Value::Object(json_map)
        }
        Document::Array(arr) => Value::Array(arr.iter().map(document_to_value).collect()),
        Document::Number(num) => match num {
            aws_smithy_types::Number::PosInt(i) => {
                Value::Number(serde_json::Number::from(*i))
            }
            aws_smithy_types::Number::NegInt(i) => {
                Value::Number(serde_json::Number::from(*i))
            }
            aws_smithy_types::Number::Float(f) => Value::Number(
                serde_json::Number::from_f64(*f).unwrap_or(serde_json::Number::from(0)),
            ),
        },
        Document::String(s) => Value::String(s.clone()),
        Document::Bool(b) => Value::Bool(*b),
        Document::Null => Value::Null,
    }
}

// Utility function to convert serde_json::Value to Document
pub fn value_to_document(value: &Value) -> Document {
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

pub fn value_to_hashmap(value: Value) -> Option<HashMap<String, Document>> {
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
pub struct JsonRpcRequest {
    pub jsonrpc: String,
    pub id: Option<Value>,
    pub method: String,
    pub params: Option<Value>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonRpcResponse {
    pub jsonrpc: String,
    pub id: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonRpcError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Tool {
    pub name: String,
    pub description: String,
    #[serde(rename = "inputSchema")]
    pub input_schema: Value,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Resource {
    pub uri: String,
    pub name: String,
    pub description: Option<String>,
    #[serde(rename = "mimeType")]
    pub mime_type: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ServerCapabilities {
    pub tools: Option<ToolsCapability>,
    pub resources: Option<ResourcesCapability>,
    pub prompts: Option<PromptsCapability>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ToolsCapability {
    #[serde(rename = "listChanged")]
    pub list_changed: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ResourcesCapability {
    pub subscribe: Option<bool>,
    #[serde(rename = "listChanged")]
    pub list_changed: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PromptsCapability {
    #[serde(rename = "listChanged")]
    pub list_changed: Option<bool>,
}

// MCP Service implementation
#[derive(Clone)]
pub struct McpService {
    pub superposition_client: Client,
    pub server_info: HashMap<String, Value>,
    pub workspace_id: String,
    pub org_id: String,
}

impl McpService {
    pub async fn new(
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

    pub fn handle_initialize(&self, id: Option<Value>) -> JsonRpcResponse {
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

    pub async fn handle_tools_list(&self, id: Option<Value>) -> JsonRpcResponse {
        let tools = self.get_tools_definition();

        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(json!({ "tools": tools })),
            error: None,
        }
    }

    pub async fn handle_tools_call(
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

        let tool_name = params["name"].as_str().unwrap_or("");
        let arguments = &params["arguments"];

        let result = self.execute_tool(tool_name, arguments).await;

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

    pub async fn handle_resources_list(&self, id: Option<Value>) -> JsonRpcResponse {
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

    pub async fn handle_resources_read(
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
        let result = self.read_resource(uri).await;

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

    pub fn handle_prompts_list(&self, id: Option<Value>) -> JsonRpcResponse {
        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(json!({ "prompts": [] })),
            error: None,
        }
    }

    // Tool definitions from modular system
    fn get_tools_definition(&self) -> Vec<Tool> {
        crate::tools::get_all_tools()
    }

    // Tool execution logic using modular system
    async fn execute_tool(
        &self,
        tool_name: &str,
        arguments: &Value,
    ) -> Result<Value, Box<dyn Error>> {
        crate::tools::execute_any_tool(self, tool_name, arguments).await
    }

    // Resource reading logic
    async fn read_resource(&self, uri: &str) -> Result<Value, Box<dyn Error>> {
        match uri {
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
            "superposition://experiments" => {
                self.superposition_client
                    .list_experiment()
                    .workspace_id(&self.workspace_id)
                    .org_id(&self.org_id)
                    .send()
                    .await
                    .map(|output| {
                        json!({
                            "count": output.data().len(),
                            "message": "Experiments found (detailed data not available due to serialization constraints)"
                        })
                    })
                    .map_err(|e| format!("SDK error: {}", e).into())
            }
            _ => Err(format!("Unknown resource: {}", uri).into()),
        }
    }
}
