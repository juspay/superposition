use actix_web::{web, HttpResponse, Result, HttpRequest};
use serde_json::{json, Value};
use std::sync::Arc;

use crate::mcp_service::{McpService, JsonRpcRequest, JsonRpcResponse, JsonRpcError};

// Shared application state
pub struct AppState {
    pub mcp_service: Arc<McpService>,
}

// Health check endpoint
pub async fn health() -> Result<HttpResponse> {
    Ok(HttpResponse::Ok().json(json!({
        "status": "healthy",
        "service": "superposition-mcp-server"
    })))
}

// MCP Initialize endpoint
pub async fn mcp_initialize(
    data: web::Data<AppState>,
    payload: web::Json<JsonRpcRequest>,
) -> Result<HttpResponse> {
    let response = data.mcp_service.handle_initialize(payload.id.clone());
    Ok(HttpResponse::Ok().json(response))
}

// MCP Tools List endpoint
pub async fn mcp_tools_list(
    data: web::Data<AppState>,
    payload: web::Json<JsonRpcRequest>,
) -> Result<HttpResponse> {
    let response = data.mcp_service.handle_tools_list(payload.id.clone()).await;
    Ok(HttpResponse::Ok().json(response))
}

// MCP Tools Call endpoint
pub async fn mcp_tools_call(
    data: web::Data<AppState>,
    payload: web::Json<JsonRpcRequest>,
) -> Result<HttpResponse> {
    let response = data.mcp_service
        .handle_tools_call(payload.id.clone(), payload.params.clone())
        .await;
    Ok(HttpResponse::Ok().json(response))
}

// MCP Resources List endpoint
pub async fn mcp_resources_list(
    data: web::Data<AppState>,
    payload: web::Json<JsonRpcRequest>,
) -> Result<HttpResponse> {
    let response = data.mcp_service.handle_resources_list(payload.id.clone()).await;
    Ok(HttpResponse::Ok().json(response))
}

// MCP Resources Read endpoint
pub async fn mcp_resources_read(
    data: web::Data<AppState>,
    payload: web::Json<JsonRpcRequest>,
) -> Result<HttpResponse> {
    let response = data.mcp_service
        .handle_resources_read(payload.id.clone(), payload.params.clone())
        .await;
    Ok(HttpResponse::Ok().json(response))
}

// MCP Prompts List endpoint
pub async fn mcp_prompts_list(
    data: web::Data<AppState>,
    payload: web::Json<JsonRpcRequest>,
) -> Result<HttpResponse> {
    let response = data.mcp_service.handle_prompts_list(payload.id.clone());
    Ok(HttpResponse::Ok().json(response))
}

// Generic MCP handler that routes based on method
pub async fn mcp_handler(
    data: web::Data<AppState>,
    payload: web::Json<JsonRpcRequest>,
) -> Result<HttpResponse> {
    let response = match payload.method.as_str() {
        "initialize" => data.mcp_service.handle_initialize(payload.id.clone()),
        "tools/list" => data.mcp_service.handle_tools_list(payload.id.clone()).await,
        "tools/call" => {
            data.mcp_service
                .handle_tools_call(payload.id.clone(), payload.params.clone())
                .await
        }
        "resources/list" => data.mcp_service.handle_resources_list(payload.id.clone()).await,
        "resources/read" => {
            data.mcp_service
                .handle_resources_read(payload.id.clone(), payload.params.clone())
                .await
        }
        "prompts/list" => data.mcp_service.handle_prompts_list(payload.id.clone()),
        _ => JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: payload.id.clone(),
            result: None,
            error: Some(JsonRpcError {
                code: -32601,
                message: "Method not found".to_string(),
                data: None,
            }),
        },
    };

    Ok(HttpResponse::Ok().json(response))
}

// Error handler for invalid JSON
pub async fn handle_json_error() -> Result<HttpResponse> {
    let error_response = JsonRpcResponse {
        jsonrpc: "2.0".to_string(),
        id: None,
        result: None,
        error: Some(JsonRpcError {
            code: -32700,
            message: "Parse error".to_string(),
            data: None,
        }),
    };
    Ok(HttpResponse::BadRequest().json(error_response))
}

// CORS preflight handler
pub async fn cors_preflight() -> Result<HttpResponse> {
    Ok(HttpResponse::Ok()
        .insert_header(("Access-Control-Allow-Origin", "*"))
        .insert_header(("Access-Control-Allow-Methods", "POST, GET, OPTIONS"))
        .insert_header(("Access-Control-Allow-Headers", "Content-Type, Authorization"))
        .finish())
}