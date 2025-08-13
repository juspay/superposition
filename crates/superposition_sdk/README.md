# Superposition SDK

Superposition SDK is a rust client for the Superposition platform, designed to facilitate the integration of Superposition's capabilities into rust applications. Read the complete documentation at [Superposition SDK Documentation](https://juspay.io/superposition/docs) or [docs.rs](https://docs.rs/superposition_sdk/latest/superposition_sdk/)

## Installation

Add the following to your `Cargo.toml`:

```toml
[dependencies]
superposition_sdk = "<version>"
```

## Initialization

```rust
use anyhow::Result; // anyhow is optional and used to simplify this example
use superposition_sdk::{Client, Config};

/// Create a Superposition SDK client with the given URL
pub fn create_client(url: String, token: String) -> Result<Client> {
    let config = Config::builder()
        .endpoint_url(url)
        .bearer_token(token.into())
        .behavior_version_latest()
        .build();

    let client = Client::from_conf(config);
    Ok(client)
}
```

## Usage

The SDK provides commands for every API call that Superposition supports. Below is an example of how to use the SDK to list default configs.

```rust
use anyhow::Result; // anyhow is optional and used to simplify this example
use superposition_sdk::types::DefaultConfigFull;

/// Fetch all default configs using the Superposition SDK
pub async fn fetch_default_configs(
    client: &superposition_sdk::Client,
    workspace_id: &str,
    org_id: &str,
) -> Result<Vec<DefaultConfigFull>> {
    let response = client
        .list_default_configs()
        .workspace_id(workspace_id)
        .org_id(org_id)
        .set_count(Some(1000))
        .send()
        .await
        .map_err(|e| anyhow::anyhow!("Failed to fetch default configs: {}", e))?;

    let configs: Vec<DefaultConfigFull> = Vec::from(response.data());
    // do stuff with the configs
    Ok(configs)
}
```
