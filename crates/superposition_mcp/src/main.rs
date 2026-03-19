use rmcp::ServiceExt;
use superposition_mcp::{McpServerConfig, SuperpositionMcpServer};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    let config = McpServerConfig::from_env()?;
    tracing::info!(
        endpoint = %config.endpoint_url,
        workspace = %config.workspace_id,
        org = %config.org_id,
        "Starting Superposition MCP server"
    );

    let server = SuperpositionMcpServer::new(config);
    let service = server
        .serve(rmcp::transport::io::stdio())
        .await
        .map_err(|e| anyhow::anyhow!("Failed to start MCP server: {e}"))?;

    service.waiting().await?;
    Ok(())
}
