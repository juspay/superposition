//! Decrypts a ciphertext via Google Cloud KMS, using the official
//! `google-cloud-kms` client (gRPC, Application Default Credentials).

use async_trait::async_trait;
use base64::{Engine, engine::general_purpose};
use google_cloud_kms::{
    client::{Client as GcpKmsClient, ClientConfig},
    grpc::kms::v1::DecryptRequest,
};

use crate::{helpers::get_from_env_unsafe, kms::SecretProvider};

pub struct Client {
    inner: GcpKmsClient,
    key_name: String,
}

pub async fn new_client() -> Client {
    let key_name: String = get_from_env_unsafe("GCP_KMS_KEY_NAME")
        .unwrap_or_else(|_| panic!("GCP_KMS_KEY_NAME not present in env"));

    let config = ClientConfig::default().with_auth().await.unwrap_or_else(|e| {
        panic!("Failed to initialize GCP credentials (Application Default Credentials): {e:?}")
    });

    let inner = GcpKmsClient::new(config)
        .await
        .unwrap_or_else(|e| panic!("Failed to create GCP KMS client: {e:?}"));

    Client { inner, key_name }
}

async fn decrypt_helper(client: &Client, key: &str, ciphertext_b64: String) -> String {
    let ciphertext = general_purpose::STANDARD
        .decode(ciphertext_b64)
        .unwrap_or_else(|e| {
            panic!("Input string for {key} does not contain valid base64 characters: {e}")
        });

    let request = DecryptRequest {
        name: client.key_name.clone(),
        ciphertext,
        additional_authenticated_data: Vec::new(),
        ciphertext_crc32c: None,
        additional_authenticated_data_crc32c: None,
    };

    let response = client
        .inner
        .decrypt(request, None)
        .await
        .unwrap_or_else(|e| panic!("Failed to decrypt {key} via GCP KMS: {e:?}"));

    String::from_utf8(response.plaintext)
        .unwrap_or_else(|e| panic!("Could not convert decrypted {key} to UTF-8: {e}"))
}

#[async_trait]
impl SecretProvider for Client {
    async fn get_secret(&self, key: &str) -> String {
        let ciphertext_b64: String = get_from_env_unsafe(key)
            .unwrap_or_else(|_| panic!("{key} not present in env"));
        decrypt_helper(self, key, ciphertext_b64).await
    }

    async fn get_secret_opt(&self, key: &str) -> Option<String> {
        let ciphertext_b64: String = get_from_env_unsafe(key).ok()?;
        Some(decrypt_helper(self, key, ciphertext_b64).await)
    }
}
