//! Fetches a plaintext secret from an OpenBao (or HashiCorp Vault, which
//! implements the same API) server's KV v2 engine. Unlike the AWS/GCP KMS
//! backends this does not decrypt a ciphertext: the "secret env var" holds an
//! OpenBao location string (`mount:path[:key]`, `key` defaults to `value`)
//! and OpenBao returns the plaintext directly, matching how Hyperswitch's
//! `hashicorp_vault` backend works.

use std::{collections::HashMap, sync::Arc};

use vaultrs::{
    client::{VaultClient, VaultClientSettingsBuilder},
    kv2,
};

use crate::helpers::get_from_env_unsafe;

#[derive(Debug, PartialEq)]
struct OpenBaoLocation {
    mount: String,
    path: String,
    key: String,
}

#[derive(Debug)]
enum OpenBaoError {
    IncompleteLocation(String),
}

impl std::fmt::Display for OpenBaoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpenBaoError::IncompleteLocation(location) => {
                write!(
                    f,
                    "incomplete OpenBao location '{location}', expected 'mount:path[:key]'"
                )
            }
        }
    }
}

fn parse_location(location: &str) -> Result<OpenBaoLocation, OpenBaoError> {
    let mut parts = location.split(':');
    let mount = parts
        .next()
        .ok_or_else(|| OpenBaoError::IncompleteLocation(location.to_string()))?;
    let path = parts
        .next()
        .ok_or_else(|| OpenBaoError::IncompleteLocation(location.to_string()))?;
    let key = parts.next().unwrap_or("value");
    Ok(OpenBaoLocation {
        mount: mount.to_string(),
        path: path.to_string(),
        key: key.to_string(),
    })
}

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
    let loc = parse_location(&location)
        .unwrap_or_else(|e| panic!("Failed to resolve OpenBao location for {key}: {e}"));

    let mut secret: HashMap<String, String> =
        kv2::read(&*client.inner, &loc.mount, &loc.path)
            .await
            .unwrap_or_else(|e| panic!("Failed to fetch {key} from OpenBao: {e}"));

    secret.remove(&loc.key).unwrap_or_else(|| {
        panic!("OpenBao secret at '{location}' has no field '{}'", loc.key)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_mount_path_and_key() {
        let loc = parse_location("secret:superposition:master_key").unwrap();
        assert_eq!(loc.mount, "secret");
        assert_eq!(loc.path, "superposition");
        assert_eq!(loc.key, "master_key");
    }

    #[test]
    fn defaults_key_to_value_when_omitted() {
        let loc = parse_location("secret:superposition").unwrap();
        assert_eq!(loc.mount, "secret");
        assert_eq!(loc.path, "superposition");
        assert_eq!(loc.key, "value");
    }

    #[test]
    fn rejects_a_location_missing_a_path() {
        assert!(matches!(
            parse_location("secret"),
            Err(OpenBaoError::IncompleteLocation(_))
        ));
    }

    #[test]
    fn rejects_an_empty_location() {
        assert!(matches!(
            parse_location(""),
            Err(OpenBaoError::IncompleteLocation(_))
        ));
    }
}
