pub mod http_handlers;
pub mod mcp_service;
pub mod streaming;
pub mod tools;

use actix_cors::Cors;
use actix_web::{http, middleware::Logger, web, App, HttpServer, Scope};
use std::error::Error;
use std::sync::Arc;
use std::time::Duration;

use http_handlers::AppState;
use mcp_service::McpService;

pub use mcp_service::{JsonRpcError, JsonRpcRequest, JsonRpcResponse};

/// Create MCP service routes that can be mounted under any path
pub fn create_mcp_routes(mcp_service: Arc<McpService>) -> Scope {
    let app_state = web::Data::new(AppState {
        mcp_service: mcp_service.clone(),
    });

    web::scope("")
        .app_data(app_state)
        .route("/health", web::get().to(http_handlers::health))
        .route("", web::post().to(http_handlers::mcp_handler))
        .route("/initialize", web::post().to(http_handlers::mcp_initialize))
        .route("/tools/list", web::post().to(http_handlers::mcp_tools_list))
        .route("/tools/call", web::post().to(http_handlers::mcp_tools_call))
        .route("/resources/list", web::post().to(http_handlers::mcp_resources_list))
        .route("/resources/read", web::post().to(http_handlers::mcp_resources_read))
        .route("/prompts/list", web::post().to(http_handlers::mcp_prompts_list))
        .route("/stream", web::get().to(streaming::mcp_stream))
        .route("/stream/config", web::get().to(streaming::config_changes_stream))
        .route("/stream/experiments", web::get().to(streaming::experiment_status_stream))
        .route("/options", web::method(http::Method::OPTIONS).to(http_handlers::cors_preflight))
}

/// Create a standalone MCP HTTP server (for backward compatibility)
pub async fn create_http_server(mcp_service: Arc<McpService>) -> Result<(), Box<dyn Error>> {
    let port = std::env::var("MCP_HTTP_PORT")
        .unwrap_or_else(|_| "8081".to_string())
        .parse::<u16>()
        .unwrap_or(8081);

    println!("Starting MCP HTTP server on port {}", port);

    HttpServer::new(move || {
        let cors = Cors::default()
            .allow_any_origin()
            .allow_any_method()
            .allow_any_header()
            .supports_credentials();

        App::new()
            .wrap(cors)
            .wrap(Logger::default())
            .service(create_mcp_routes(mcp_service.clone()).configure(|cfg| {
                cfg.route("/mcp", web::post().to(http_handlers::mcp_handler));
            }))
    })
    .bind(("0.0.0.0", port))?
    .keep_alive(Duration::from_secs(120))
    .run()
    .await?;

    Ok(())
}

/// Initialize MCP service with default configuration
pub async fn initialize_mcp_service() -> Result<Arc<McpService>, Box<dyn Error>> {
    let workspace = std::env::var("SUPERPOSITION_DEFAULT_WORKSPACE")
        .unwrap_or_else(|_| "dev".to_string());
    let org = std::env::var("SUPERPOSITION_DEFAULT_ORG")
        .unwrap_or_else(|_| "localorg".to_string());
    let host = std::env::var("SUPERPOSITION_HOST")
        .unwrap_or_else(|_| "http://localhost:8080".to_string());
    let token = std::env::var("SUPERPOSITION_DEFAULT_TOKEN")
        .unwrap_or_else(|_| "12345".to_string());

    Ok(Arc::new(McpService::new(workspace, org, host, token).await?))
}

/// Initialize MCP service with custom configuration
pub async fn initialize_mcp_service_with_config(
    workspace: String,
    org: String,
    host: String,
    token: String,
) -> Result<Arc<McpService>, Box<dyn Error>> {
    Ok(Arc::new(McpService::new(workspace, org, host, token).await?))
}