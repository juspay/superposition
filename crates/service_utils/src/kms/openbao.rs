//! OpenBao (or HashiCorp Vault) KV v2 backend: fetches a stored plaintext
//! secret by location (`mount:path[:key]`), rather than decrypting a
//! ciphertext like the AWS/GCP backends.

use std::collections::HashMap;

use async_trait::async_trait;
use vaultrs::{
    client::{VaultClient, VaultClientSettingsBuilder},
    kv2,
};

use crate::{helpers::get_from_env_unsafe, kms::KmsClient};

pub struct Client {
    inner: VaultClient,
}

pub async fn new_client() -> Client {
    let address: String = get_from_env_unsafe("OPENBAO_ADDR")
        .unwrap_or_else(|_| panic!("OPENBAO_ADDR not present in env"));
    let token: String = get_from_env_unsafe("OPENBAO_TOKEN")
        .unwrap_or_else(|_| panic!("OPENBAO_TOKEN not present in env"));

    let settings = VaultClientSettingsBuilder::default()
        .address(address)
        .token(token)
        .build()
        .unwrap_or_else(|e| panic!("Failed to build OpenBao client settings: {e}"));

    let inner = VaultClient::new(settings)
        .unwrap_or_else(|e| panic!("Failed to create OpenBao client: {e}"));

    Client { inner }
}

async fn fetch_helper(client: &Client, key: &str, location: String) -> String {
    let mut parts = location.split(':');
    let mount = parts.next().unwrap_or_default();
    let path = parts.next().unwrap_or_else(|| {
        panic!("Invalid OpenBao location for {key}: '{location}', expected 'mount:path[:key]'")
    });
    let field = parts.next().unwrap_or("value");

    let mut secret: HashMap<String, String> = kv2::read(&client.inner, mount, path)
        .await
        .unwrap_or_else(|e| panic!("Failed to fetch {key} from OpenBao: {e}"));

    secret.remove(field).unwrap_or_else(|| {
        panic!("OpenBao secret at '{location}' has no field '{field}'")
    })
}

#[async_trait]
impl KmsClient for Client {
    async fn decrypt(&self, key: &str) -> String {
        let location: String = get_from_env_unsafe(key)
            .unwrap_or_else(|_| panic!("{key} not present in env"));
        fetch_helper(self, key, location).await
    }

    async fn decrypt_opt(&self, key: &str) -> Option<String> {
        let location: String = get_from_env_unsafe(key).ok()?;
        Some(fetch_helper(self, key, location).await)
    }
}
