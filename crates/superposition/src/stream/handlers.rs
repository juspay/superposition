use actix_web::{HttpResponse, Scope, web::Data};
use futures_util::stream::{self, StreamExt};
use service_utils::service::types::{AppState, WorkspaceContext};
use tokio::time::{Duration, interval};

pub fn endpoints() -> Scope {
    Scope::new("").service(sse_stream)
}

#[actix_web::get("")]
async fn sse_stream(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
) -> HttpResponse {
    let schema_name = workspace_context.schema_name.0.clone();
    let mut rx = state.subscribe_sse(&schema_name);
    let mut keepalive = interval(Duration::from_secs(15));
    keepalive.tick().await; // skip the immediate first tick

    let event_stream = stream::unfold(
        (rx, keepalive),
        |(mut rx, mut keepalive)| async move {
            tokio::select! {
                result = rx.changed() => match result {
                    Ok(()) => {
                        let payload = "event: config_change\ndata: {}\n\n";
                        Some((Ok::<_, actix_web::Error>(actix_web::web::Bytes::from(payload)), (rx, keepalive)))
                    }
                    Err(_) => None,
                },
                _ = keepalive.tick() => {
                    Some((Ok(actix_web::web::Bytes::from(": keepalive\n\n")), (rx, keepalive)))
                }
            }
        },
    );

    HttpResponse::Ok()
        .content_type("text/event-stream")
        .insert_header(("Cache-Control", "no-cache"))
        .insert_header(("X-Accel-Buffering", "no"))
        .streaming(event_stream)
}
