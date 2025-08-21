use actix_web::{web, HttpResponse, Result, HttpRequest};
use futures_util::{Stream, StreamExt};
use bytes::Bytes;
use serde_json::json;
use std::time::Duration;
use std::sync::Arc;
use async_stream::stream;

use crate::http_handlers::AppState;

// Server-Sent Events endpoint for streaming MCP notifications
pub async fn mcp_stream(
    _req: HttpRequest,
    _data: web::Data<AppState>,
) -> Result<HttpResponse> {
    let stream = create_mcp_event_stream();
    
    Ok(HttpResponse::Ok()
        .insert_header(("Content-Type", "text/event-stream"))
        .insert_header(("Cache-Control", "no-cache"))
        .insert_header(("Connection", "keep-alive"))
        .insert_header(("Access-Control-Allow-Origin", "*"))
        .streaming(stream))
}

// Create a stream of Server-Sent Events for MCP notifications
fn create_mcp_event_stream() -> impl Stream<Item = Result<Bytes, actix_web::Error>> {
    stream! {
        // Send initial connection event
        let initial_event = format!(
            "event: connected\ndata: {}\n\n",
            json!({
                "type": "connection_established",
                "timestamp": chrono::Utc::now().to_rfc3339(),
                "message": "MCP streaming connection established"
            })
        );
        yield Ok(Bytes::from(initial_event));

        // Send periodic keepalive events
        let mut interval = tokio::time::interval(Duration::from_secs(30));
        loop {
            interval.tick().await;
            
            let keepalive_event = format!(
                "event: keepalive\ndata: {}\n\n",
                json!({
                    "type": "keepalive",
                    "timestamp": chrono::Utc::now().to_rfc3339(),
                    "message": "Connection alive"
                })
            );
            yield Ok(Bytes::from(keepalive_event));
        }
    }
}

// Endpoint for real-time configuration changes notifications
pub async fn config_changes_stream(
    _req: HttpRequest,
    _data: web::Data<AppState>,
) -> Result<HttpResponse> {
    let stream = create_config_changes_stream();
    
    Ok(HttpResponse::Ok()
        .insert_header(("Content-Type", "text/event-stream"))
        .insert_header(("Cache-Control", "no-cache"))
        .insert_header(("Connection", "keep-alive"))
        .insert_header(("Access-Control-Allow-Origin", "*"))
        .streaming(stream))
}

// Create a stream for configuration change notifications
fn create_config_changes_stream() -> impl Stream<Item = Result<Bytes, actix_web::Error>> {
    stream! {
        // Send initial event
        let initial_event = format!(
            "event: config_stream_start\ndata: {}\n\n",
            json!({
                "type": "config_stream_initialized",
                "timestamp": chrono::Utc::now().to_rfc3339(),
                "message": "Configuration changes stream started"
            })
        );
        yield Ok(Bytes::from(initial_event));

        // Simulate configuration change events
        // In a real implementation, this would listen to actual configuration changes
        let mut interval = tokio::time::interval(Duration::from_secs(60));
        loop {
            interval.tick().await;
            
            let config_event = format!(
                "event: config_changed\ndata: {}\n\n",
                json!({
                    "type": "configuration_updated",
                    "timestamp": chrono::Utc::now().to_rfc3339(),
                    "change_type": "default_config",
                    "affected_keys": ["example_key"],
                    "message": "Default configuration has been updated"
                })
            );
            yield Ok(Bytes::from(config_event));
        }
    }
}

// Endpoint for experiment status notifications
pub async fn experiment_status_stream(
    _req: HttpRequest,
    _data: web::Data<AppState>,
) -> Result<HttpResponse> {
    let stream = create_experiment_status_stream();
    
    Ok(HttpResponse::Ok()
        .insert_header(("Content-Type", "text/event-stream"))
        .insert_header(("Cache-Control", "no-cache"))
        .insert_header(("Connection", "keep-alive"))
        .insert_header(("Access-Control-Allow-Origin", "*"))
        .streaming(stream))
}

// Create a stream for experiment status notifications
fn create_experiment_status_stream() -> impl Stream<Item = Result<Bytes, actix_web::Error>> {
    stream! {
        // Send initial event
        let initial_event = format!(
            "event: experiment_stream_start\ndata: {}\n\n",
            json!({
                "type": "experiment_stream_initialized",
                "timestamp": chrono::Utc::now().to_rfc3339(),
                "message": "Experiment status stream started"
            })
        );
        yield Ok(Bytes::from(initial_event));

        // Simulate experiment status events
        let mut interval = tokio::time::interval(Duration::from_secs(120));
        loop {
            interval.tick().await;
            
            let experiment_event = format!(
                "event: experiment_status\ndata: {}\n\n",
                json!({
                    "type": "experiment_status_update",
                    "timestamp": chrono::Utc::now().to_rfc3339(),
                    "experiment_id": "exp_123",
                    "status": "running",
                    "traffic_percentage": 50,
                    "message": "Experiment status updated"
                })
            );
            yield Ok(Bytes::from(experiment_event));
        }
    }
}