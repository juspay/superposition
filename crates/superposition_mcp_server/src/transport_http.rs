use std::net::SocketAddr;
use std::sync::Arc;

use axum::body::Body;
use axum::extract::Request;
use axum::http::{header, StatusCode};
use axum::middleware::{self, Next};
use axum::response::Response;
use axum::Router as AxumRouter;
use rmcp::handler::server::ServerHandler;
use rmcp::model::{
    CallToolRequestParams, CallToolResult, ListToolsResult, PaginatedRequestParams, ServerInfo,
};
use rmcp::service::RequestContext;
use rmcp::transport::streamable_http_server::session::local::LocalSessionManager;
use rmcp::transport::streamable_http_server::{
    StreamableHttpServerConfig, StreamableHttpService,
};
use rmcp::{ErrorData, RoleServer};
use smithy_mcp_runtime::Router;
use tracing::{error, info, warn};

use crate::auth::{AuthParseError, AuthValue, SUPERPOSITION_AUTH};
use crate::config::Mode;

/// `AuthRouter` wraps `smithy_mcp_runtime::Router` and re-enters the
/// `SUPERPOSITION_AUTH` task-local on every `call_tool` based on the inbound
/// HTTP `Authorization` header, which rmcp stores in `RequestContext::extensions`
/// as `http::request::Parts`.
///
/// **Why a wrapper and not the Axum middleware's `scope`?**
/// rmcp's stateful HTTP transport spawns a long-running session task during the
/// `initialize` request. Subsequent requests (`tools/call`, `tools/list`, …)
/// are routed into that session task's queue — *not* dispatched in the Axum
/// middleware's task. So a `SUPERPOSITION_AUTH.scope(...)` wrapped around
/// `next.run(req)` in the middleware applies only to the initialize, and the
/// session task carries the initialize-time auth for the rest of its life.
/// Multi-tenant HTTP usage with per-request credentials needs the scope to
/// happen *inside* the handler that runs in whatever task rmcp dispatches us
/// in — that's this wrapper.
struct AuthRouter {
    inner: Arc<Router>,
    /// Used when the inbound request has no Authorization header but the
    /// operator started the binary with `--allow-static-auth` and provided env
    /// credentials. None in pure passthrough mode.
    static_fallback: Option<AuthValue>,
}

impl ServerHandler for AuthRouter {
    fn get_info(&self) -> ServerInfo {
        self.inner.get_info()
    }

    fn list_tools(
        &self,
        request: Option<PaginatedRequestParams>,
        context: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<ListToolsResult, ErrorData>> + Send + '_ {
        self.inner.list_tools(request, context)
    }

    fn call_tool(
        &self,
        request: CallToolRequestParams,
        context: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<CallToolResult, ErrorData>> + Send + '_ {
        let inner = self.inner.clone();
        let fallback = self.static_fallback.clone();
        async move {
            let auth = extract_auth_from_context(&context).or(fallback);
            match auth {
                Some(a) => {
                    SUPERPOSITION_AUTH
                        .scope(a, async move { inner.call_tool(request, context).await })
                        .await
                }
                None => {
                    // Resolver will surface a clearer error; this branch should be
                    // unreachable for HTTP (the Axum middleware rejects unauthenticated
                    // requests at the transport layer).
                    inner.call_tool(request, context).await
                }
            }
        }
    }
}

/// Pull the parsed `AuthValue` out of `RequestContext`. rmcp stores
/// `http::request::Parts` in the context's extensions; we re-parse the
/// `Authorization` header from it. Cheap, and avoids coupling between the
/// Axum middleware and the rmcp handler.
fn extract_auth_from_context(context: &RequestContext<RoleServer>) -> Option<AuthValue> {
    let parts = context.extensions.get::<http::request::Parts>()?;
    let raw = parts
        .headers
        .get(http::header::AUTHORIZATION)
        .and_then(|v| v.to_str().ok())?;
    AuthValue::parse_header(Some(raw)).ok()
}

pub async fn serve(
    addr: SocketAddr,
    mode: Mode,
    static_fallback: Option<AuthValue>,
    router: Router,
) -> anyhow::Result<()> {
    let router = Arc::new(router);
    let static_fallback_for_handler = if matches!(mode, Mode::HttpWithStaticFallback) {
        static_fallback.clone()
    } else {
        None
    };

    let svc = StreamableHttpService::new(
        {
            let router = router.clone();
            let fallback = static_fallback_for_handler.clone();
            move || {
                Ok::<_, std::io::Error>(AuthRouter {
                    inner: router.clone(),
                    static_fallback: fallback.clone(),
                })
            }
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

/// Axum middleware that rejects requests without a usable Authorization
/// header before they reach rmcp. Cheap pre-filter at the HTTP layer.
///
/// The actual `SUPERPOSITION_AUTH.scope` happens inside [`AuthRouter::call_tool`],
/// not here — see the doc on [`AuthRouter`] for why.
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

    match AuthValue::parse_header(header_value.as_deref()) {
        Ok(_) => next.run(req).await,
        Err(AuthParseError::Missing) if allow_static && fallback.is_some() => {
            warn!("falling back to static credentials (--allow-static-auth)");
            next.run(req).await
        }
        Err(AuthParseError::Missing) => {
            unauthorized("no Authorization header and no static fallback")
        }
        Err(e) => unauthorized(&format!("{}", e)),
    }
}

fn unauthorized(detail: &str) -> Response {
    error!("auth rejected: {detail}");
    Response::builder()
        .status(StatusCode::UNAUTHORIZED)
        .header(header::WWW_AUTHENTICATE, "Bearer realm=\"superposition-mcp\"")
        .body(Body::from(detail.to_string()))
        .unwrap()
}
