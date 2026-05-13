use superposition_mcp_server::{auth, build, config};

mod transport_http;

use clap::Parser;
use smithy_mcp_runtime::Router;
use tracing_subscriber::EnvFilter;

use crate::auth::{AuthValue, SUPERPOSITION_AUTH};
use crate::config::{Config, Mode};

#[derive(Parser, Debug)]
#[command(name = "superposition-mcp", about = "MCP server for the Superposition API")]
struct Cli {
    /// Bind address for HTTP+SSE transport. If unset, stdio is used.
    #[arg(long, value_name = "ADDR")]
    http: Option<String>,

    /// In HTTP mode, fall back to env-var credentials when no Authorization header is present.
    #[arg(long, requires = "http")]
    allow_static_auth: bool,
}

fn init_logging() {
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"));
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_writer(std::io::stderr) // STDOUT is the JSON-RPC channel on stdio
        .init();
}

async fn stdio_serve(cfg: Config, router: Router) -> anyhow::Result<()> {
    let auth_value: AuthValue = cfg
        .creds
        .clone()
        .expect("config::load enforces creds presence for stdio mode")
        .into();

    SUPERPOSITION_AUTH
        .scope(auth_value, async {
            smithy_mcp_runtime::serve_stdio(router)
                .await
                .map_err(|e| anyhow::anyhow!("stdio transport error: {e}"))
        })
        .await
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    init_logging();
    let cli = Cli::parse();

    let mode = match (&cli.http, cli.allow_static_auth) {
        (None, _) => Mode::Stdio,
        (Some(_), true) => Mode::HttpWithStaticFallback,
        (Some(_), false) => Mode::HttpPassthrough,
    };

    let cfg = config::load(mode, &config::ProcessEnv)?;
    let client = build::build_client(&cfg, mode);
    let router = build::build_router(client, cfg.defaults.clone());

    match (mode, cli.http.as_deref()) {
        (Mode::Stdio, _) => stdio_serve(cfg, router).await,
        (Mode::HttpPassthrough, Some(addr)) | (Mode::HttpWithStaticFallback, Some(addr)) => {
            let socket_addr: std::net::SocketAddr = addr
                .parse()
                .map_err(|e| anyhow::anyhow!("invalid --http address {}: {}", addr, e))?;
            let static_fallback: Option<crate::auth::AuthValue> =
                cfg.creds.clone().map(Into::into);
            transport_http::serve(socket_addr, mode, static_fallback, router).await
        }
        _ => unreachable!(),
    }
}
