use std::net::SocketAddr;
use std::sync::Arc;

use axum::body::Body;
use axum::extract::Request;
use axum::http::{header, StatusCode};
use axum::middleware::{self, Next};
use axum::response::Response;
use axum::Router as AxumRouter;
use rmcp::transport::streamable_http_server::session::local::LocalSessionManager;
use rmcp::transport::streamable_http_server::{
    StreamableHttpServerConfig, StreamableHttpService,
};
use smithy_mcp_runtime::Router;
use tracing::{error, info, warn};

use crate::auth::{AuthParseError, AuthValue, SUPERPOSITION_AUTH};
use crate::config::Mode;

pub async fn serve(
    addr: SocketAddr,
    mode: Mode,
    static_fallback: Option<AuthValue>,
    router: Router,
) -> anyhow::Result<()> {
    // smithy_mcp_runtime::Router is not Clone. We wrap it in an Arc and rely on
    // rmcp's blanket `impl<T: ServerHandler> ServerHandler for Arc<T>` so the
    // service factory can hand out cheap clones (Arc::clone) per session.
    let router = Arc::new(router);

    let svc = StreamableHttpService::new(
        {
            let router = router.clone();
            move || Ok::<_, std::io::Error>(router.clone())
        },
        Arc::new(LocalSessionManager::default()),
        StreamableHttpServerConfig::default(),
    );

    let allow_static = matches!(mode, Mode::HttpWithStaticFallback);
    let fallback = Arc::new(static_fallback);

    let app = AxumRouter::new()
        .nest_service("/mcp", svc)
        .layer(middleware::from_fn(move |req: Request, next: Next| {
            let fallback = fallback.clone();
            async move { auth_layer(req, next, allow_static, fallback).await }
        }));

    info!(%addr, "MCP server listening (HTTP)");
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;
    Ok(())
}

async fn auth_layer(
    req: Request,
    next: Next,
    allow_static: bool,
    fallback: Arc<Option<AuthValue>>,
) -> Response {
    let header_value = req
        .headers()
        .get(header::AUTHORIZATION)
        .and_then(|v| v.to_str().ok())
        .map(str::to_owned);

    let auth = match AuthValue::parse_header(header_value.as_deref()) {
        Ok(a) => a,
        Err(AuthParseError::Missing) if allow_static => match fallback.as_ref() {
            Some(a) => {
                warn!("falling back to static credentials (--allow-static-auth)");
                a.clone()
            }
            None => return unauthorized("no Authorization header and no static fallback"),
        },
        Err(e) => return unauthorized(&format!("{}", e)),
    };

    let fut = next.run(req);
    SUPERPOSITION_AUTH.scope(auth, fut).await
}

fn unauthorized(detail: &str) -> Response {
    error!("auth rejected: {detail}");
    Response::builder()
        .status(StatusCode::UNAUTHORIZED)
        .header(header::WWW_AUTHENTICATE, "Bearer realm=\"superposition-mcp\"")
        .body(Body::from(detail.to_string()))
        .unwrap()
}
