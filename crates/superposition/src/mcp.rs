use actix_web::{web, HttpResponse, Result as ActixResult, Scope};
use std::sync::Arc;
use superposition_mcp::{mcp_service::McpService, http_handlers};
use log::{info, error};

// Initialize MCP service for the main application
pub async fn initialize_mcp_service(base_url: String) -> Option<Arc<McpService>> {
    info!("Initializing MCP service for integration...");
    
    // Use environment variables or defaults for MCP configuration
    let workspace = std::env::var("SUPERPOSITION_DEFAULT_WORKSPACE")
        .unwrap_or_else(|_| "dev".to_string());
    let org = std::env::var("SUPERPOSITION_DEFAULT_ORG")
        .unwrap_or_else(|_| "localorg".to_string());
    
    // Construct the host URL - use the same host as the main application
    let api_hostname = std::env::var("API_HOSTNAME")
        .unwrap_or_else(|_| "http://localhost:8080".to_string());
    let host = format!("{}{}", api_hostname, base_url);
    
    let token = std::env::var("SUPERPOSITION_DEFAULT_TOKEN")
        .unwrap_or_else(|_| "12345".to_string());

    info!("MCP Configuration - workspace: {}, org: {}, host: {}", workspace, org, host);

    // Initialize the MCP service
    match McpService::new(workspace, org, host, token).await {
        Ok(service) => {
            info!("MCP service initialized successfully");
            Some(Arc::new(service))
        }
        Err(e) => {
            error!("Failed to initialize MCP service: {}", e);
            None
        }
    }
}

// Create MCP routes with proper handlers and app state
pub fn create_mcp_routes(mcp_service: Arc<McpService>) -> Scope {
    let mcp_app_state = web::Data::new(http_handlers::AppState {
        mcp_service: mcp_service.clone(),
    });

    web::scope("/mcp")
        .app_data(mcp_app_state)
        .route("/health", web::get().to(mcp_health))
        .route("", web::post().to(http_handlers::mcp_handler))
        .route("/initialize", web::post().to(http_handlers::mcp_initialize))
        .route("/tools/list", web::post().to(http_handlers::mcp_tools_list))
        .route("/tools/call", web::post().to(http_handlers::mcp_tools_call))
        .route("/resources/list", web::post().to(http_handlers::mcp_resources_list))
        .route("/resources/read", web::post().to(http_handlers::mcp_resources_read))
        .route("/prompts/list", web::post().to(http_handlers::mcp_prompts_list))
}

// Create empty MCP routes when service is unavailable
pub fn create_empty_mcp_routes() -> Scope {
    web::scope("/mcp")
        .route("/health", web::get().to(mcp_health_unavailable))
        .default_service(web::route().to(mcp_service_unavailable))
}

// Health check handler
async fn mcp_health() -> ActixResult<HttpResponse> {
    Ok(HttpResponse::Ok()
        .content_type("application/json")
        .body(r#"{"status": "healthy", "service": "mcp"}"#))
}

// Health check for unavailable service
async fn mcp_health_unavailable() -> ActixResult<HttpResponse> {
    Ok(HttpResponse::ServiceUnavailable()
        .content_type("application/json")
        .body(r#"{"status": "unavailable", "service": "mcp", "error": "MCP service failed to initialize"}"#))
}

// Handler for when MCP service is unavailable
async fn mcp_service_unavailable() -> ActixResult<HttpResponse> {
    Ok(HttpResponse::ServiceUnavailable()
        .content_type("application/json")
        .body(r#"{"error": "MCP service is currently unavailable"}"#))
}