use std::env;

/// Configuration for the Superposition MCP server.
#[derive(Clone, Debug)]
pub struct McpServerConfig {
    /// Base URL of the Superposition API (e.g., "http://localhost:8080")
    pub endpoint_url: String,
    /// Default workspace ID injected into all SDK calls
    pub workspace_id: String,
    /// Default organisation ID injected into all SDK calls
    pub org_id: String,
    /// Optional bearer token for authentication
    pub auth_token: Option<String>,
}

impl McpServerConfig {
    /// Reads configuration from environment variables.
    ///
    /// Required:
    /// - `SUPERPOSITION_URL` — Base URL of the Superposition API
    /// - `SUPERPOSITION_WORKSPACE` — Default workspace ID
    /// - `SUPERPOSITION_ORG_ID` — Default organisation ID
    ///
    /// Optional:
    /// - `SUPERPOSITION_AUTH_TOKEN` — Bearer token for authentication
    pub fn from_env() -> anyhow::Result<Self> {
        Ok(Self {
            endpoint_url: env::var("SUPERPOSITION_URL")
                .map_err(|_| anyhow::anyhow!("SUPERPOSITION_URL env var is required"))?,
            workspace_id: env::var("SUPERPOSITION_WORKSPACE").map_err(|_| {
                anyhow::anyhow!("SUPERPOSITION_WORKSPACE env var is required")
            })?,
            org_id: env::var("SUPERPOSITION_ORG_ID").map_err(|_| {
                anyhow::anyhow!("SUPERPOSITION_ORG_ID env var is required")
            })?,
            auth_token: env::var("SUPERPOSITION_AUTH_TOKEN").ok(),
        })
    }

    /// Build a `superposition_sdk::Client` from this configuration.
    pub fn build_sdk_client(&self) -> superposition_sdk::Client {
        let mut config_builder = superposition_sdk::Config::builder()
            .endpoint_url(&self.endpoint_url)
            .behavior_version(superposition_sdk::config::BehaviorVersion::latest());

        if let Some(ref token) = self.auth_token {
            use aws_smithy_runtime_api::client::identity::http::Token;
            config_builder = config_builder.bearer_token(Token::new(token, None));
        }

        let config = config_builder.build();
        superposition_sdk::Client::from_conf(config)
    }
}
