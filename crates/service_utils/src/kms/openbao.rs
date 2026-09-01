//! OpenBao (or HashiCorp Vault) KV v2 backend: fetches a stored plaintext
//! secret by location (`mount:path[:key]`), rather than decrypting a
//! ciphertext like the AWS/GCP backends.

use std::{collections::HashMap, sync::Arc};

use vaultrs::{
    client::{VaultClient, VaultClientSettingsBuilder},
    kv2,
};

use crate::helpers::get_from_env_unsafe;

#[derive(Clone)]
pub struct Client {
    inner: Arc<VaultClient>,
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

    Client {
        inner: Arc::new(inner),
    }
}

async fn fetch_helper(client: Client, key: &str, location: String) -> String {
    let mut parts = location.split(':');
    let mount = parts.next().unwrap_or_default();
    let path = parts.next().unwrap_or_else(|| {
        panic!("Invalid OpenBao location for {key}: '{location}', expected 'mount:path[:key]'")
    });
    let field = parts.next().unwrap_or("value");

    let mut secret: HashMap<String, String> = kv2::read(&*client.inner, mount, path)
        .await
        .unwrap_or_else(|e| panic!("Failed to fetch {key} from OpenBao: {e}"));

    secret.remove(field).unwrap_or_else(|| {
        panic!("OpenBao secret at '{location}' has no field '{field}'")
    })
}

pub async fn decrypt(client: Client, key: &str) -> String {
    let location: String =
        get_from_env_unsafe(key).unwrap_or_else(|_| panic!("{key} not present in env"));
    fetch_helper(client, key, location).await
}

pub async fn decrypt_opt(client: Client, key: &str) -> Option<String> {
    let location: String = get_from_env_unsafe(key).ok()?;
    Some(fetch_helper(client, key, location).await)
}
