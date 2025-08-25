mod http_handlers;
mod mcp_service;
mod streaming;
mod tools;

use actix_cors::Cors;
use actix_web::{http, middleware::Logger, web, App, HttpServer};
use std::error::Error;
use std::io::{self, stdin, BufRead, BufReader, Write};
use std::sync::Arc;
use tokio;

use http_handlers::AppState;
use mcp_service::{JsonRpcError, JsonRpcRequest, JsonRpcResponse, McpService};

// HTTP server configuration
async fn create_http_server(mcp_service: Arc<McpService>) -> Result<(), Box<dyn Error>> {
    let port = std::env::var("MCP_HTTP_PORT")
        .unwrap_or_else(|_| "8081".to_string())
        .parse::<u16>()
        .unwrap_or(8081);

    let app_state = web::Data::new(AppState {
        mcp_service: mcp_service.clone(),
    });

    println!("Starting MCP HTTP server on port {}", port);

    HttpServer::new(move || {
        let cors = Cors::default()
            .allow_any_origin()
            .allow_any_method()
            .allow_any_header()
            .supports_credentials();

        App::new()
            .app_data(app_state.clone())
            .wrap(cors)
            .wrap(Logger::default())
            .route("/health", web::get().to(http_handlers::health))
            .route("/mcp", web::post().to(http_handlers::mcp_handler))
            .route(
                "/mcp/initialize",
                web::post().to(http_handlers::mcp_initialize),
            )
            .route(
                "/mcp/tools/list",
                web::post().to(http_handlers::mcp_tools_list),
            )
            .route(
                "/mcp/tools/call",
                web::post().to(http_handlers::mcp_tools_call),
            )
            .route(
                "/mcp/resources/list",
                web::post().to(http_handlers::mcp_resources_list),
            )
            .route(
                "/mcp/resources/read",
                web::post().to(http_handlers::mcp_resources_read),
            )
            .route(
                "/mcp/prompts/list",
                web::post().to(http_handlers::mcp_prompts_list),
            )
            .route("/mcp/stream", web::get().to(streaming::mcp_stream))
            .route(
                "/mcp/stream/config",
                web::get().to(streaming::config_changes_stream),
            )
            .route(
                "/mcp/stream/experiments",
                web::get().to(streaming::experiment_status_stream),
            )
            .route(
                "/mcp/options",
                web::method(http::Method::OPTIONS).to(http_handlers::cors_preflight),
            )
    })
    .bind(("0.0.0.0", port))?
    .run()
    .await?;

    Ok(())
}

// Original stdio server implementation
async fn run_stdio_server(mcp_service: Arc<McpService>) -> Result<(), Box<dyn Error>> {
    eprintln!("MCP stdio server initialized successfully");

    let stdin = stdin();
    let mut reader = BufReader::new(stdin.lock());

    eprintln!("Starting stdio message loop...");

    loop {
        let mut line = String::new();

        eprintln!("Waiting for next message...");
        match reader.read_line(&mut line) {
            Ok(0) => {
                eprintln!("Client disconnected (EOF) - stdin closed");
                std::thread::sleep(std::time::Duration::from_millis(100));
                break;
            }
            Ok(_) => {
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
                let response = handle_stdio_request(&mcp_service, request).await;
                let response_json = serde_json::to_string(&response)?;

                eprintln!("Sending response: {}", response_json);
                println!("{}", response_json);
                io::stdout().flush()?;
            }
            Err(e) => {
                eprintln!("Error reading from stdin: {}", e);
                break;
            }
        }
    }

    eprintln!("MCP stdio server shutting down");
    Ok(())
}

// Handle stdio requests using the MCP service
async fn handle_stdio_request(
    mcp_service: &McpService,
    request: JsonRpcRequest,
) -> JsonRpcResponse {
    match request.method.as_str() {
        "initialize" => mcp_service.handle_initialize(request.id),
        "tools/list" => mcp_service.handle_tools_list(request.id).await,
        "tools/call" => {
            mcp_service
                .handle_tools_call(request.id, request.params, None)
                .await
        }
        "resources/list" => mcp_service.handle_resources_list(request.id).await,
        "resources/read" => {
            mcp_service
                .handle_resources_read(request.id, request.params)
                .await
        }
        "prompts/list" => mcp_service.handle_prompts_list(request.id),
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

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    // Configuration - you can get these from environment variables
    let workspace = std::env::var("SUPERPOSITION_DEFAULT_WORKSPACE")
        .unwrap_or_else(|_| "dev".to_string());
    let org = std::env::var("SUPERPOSITION_DEFAULT_ORG")
        .unwrap_or_else(|_| "localorg".to_string());
    let host = std::env::var("SUPERPOSITION_HOST")
        .unwrap_or_else(|_| "http://localhost:8080".to_string());
    let token = std::env::var("SUPERPOSITION_DEFAULT_TOKEN")
        .unwrap_or_else(|_| "12345".to_string());

    // Check transport mode from environment variable
    let transport_mode =
        std::env::var("MCP_TRANSPORT").unwrap_or_else(|_| "stdio".to_string());

    // Create MCP service
    let mcp_service = Arc::new(McpService::new(workspace, org, host, token).await?);

    match transport_mode.as_str() {
        "http" => {
            println!("Starting MCP server in HTTP mode");
            create_http_server(mcp_service).await?;
        }
        "stdio" | _ => {
            println!("Starting MCP server in stdio mode");
            run_stdio_server(mcp_service).await?;
        }
    }

    Ok(())
}

