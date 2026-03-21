//! Actix-web integration for mounting the MCP server as an HTTP endpoint.

use std::sync::Arc;
use std::time::Duration;

use actix_web::web;
use rmcp::transport::streamable_http_server::session::local::LocalSessionManager;
use rmcp_actix_web::transport::StreamableHttpService;

use crate::{McpServerConfig, SuperpositionMcpServer};

/// Creates an actix-web `Scope` that serves the MCP server at the mounted path.
///
/// The returned scope handles POST (JSON-RPC requests), GET (SSE stream resumption),
/// and DELETE (session termination) for the Streamable HTTP MCP transport.
///
/// # Example
///
/// ```ignore
/// use actix_web::{App, HttpServer, web};
/// use superposition_mcp::actix::mcp_scope;
///
/// let mcp_service = mcp_scope(mcp_config);
/// HttpServer::new(move || {
///     App::new()
///         .service(web::scope("/mcp").service(mcp_service.clone()))
/// })
/// ```
pub fn mcp_scope(
    config: McpServerConfig,
) -> StreamableHttpService<SuperpositionMcpServer> {
    StreamableHttpService::builder()
        .service_factory(Arc::new(move || {
            Ok(SuperpositionMcpServer::new(config.clone()))
        }))
        .session_manager(Arc::new(LocalSessionManager::default()))
        .stateful_mode(true)
        .sse_keep_alive(Duration::from_secs(30))
        .build()
}

/// Creates an actix-web `Scope` mounted at `/mcp`.
///
/// This is a convenience wrapper that creates the scope with the standard path.
pub fn mcp_service(config: McpServerConfig) -> actix_web::Scope {
    let service = mcp_scope(config);
    web::scope("/mcp").service(service.scope())
}
