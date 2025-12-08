use aes_gcm::{
    aead::{Aead, KeyInit, OsRng},
    Aes256Gcm, Nonce,
};
use base64::{engine::general_purpose, Engine};
use rand::RngCore;

use crate::service::types::AppEnv;

const NONCE_SIZE: usize = 12;

#[derive(Debug)]
pub enum EncryptionError {
    EncryptionFailed(String),
    DecryptionFailed(String),
    InvalidKey(String),
    InvalidCiphertext(String),
}

impl std::fmt::Display for EncryptionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EncryptionError::EncryptionFailed(msg) => {
                write!(f, "Encryption failed: {}", msg)
            }
            EncryptionError::DecryptionFailed(msg) => {
                write!(f, "Decryption failed: {}", msg)
            }
            EncryptionError::InvalidKey(msg) => write!(f, "Invalid key: {}", msg),
            EncryptionError::InvalidCiphertext(msg) => {
                write!(f, "Invalid ciphertext: {}", msg)
            }
        }
    }
}

impl std::error::Error for EncryptionError {}

pub fn generate_encryption_key() -> String {
    let mut key_bytes = [0u8; 32];
    OsRng.fill_bytes(&mut key_bytes);
    general_purpose::STANDARD.encode(key_bytes)
}

pub fn encrypt_secret(plaintext: &str, key: &str) -> Result<String, EncryptionError> {
    let key_bytes = general_purpose::STANDARD.decode(key).map_err(|e| {
        EncryptionError::InvalidKey(format!("Failed to decode key: {}", e))
    })?;

    if key_bytes.len() != 32 {
        return Err(EncryptionError::InvalidKey(format!(
            "Key {} must be 32 bytes, got {}",
            key,
            key_bytes.len()
        )));
    }

    let cipher = Aes256Gcm::new_from_slice(&key_bytes).map_err(|e| {
        EncryptionError::InvalidKey(format!("Failed to create cipher: {}", e))
    })?;

    let mut nonce_bytes = [0u8; NONCE_SIZE];
    OsRng.fill_bytes(&mut nonce_bytes);
    let nonce = Nonce::from_slice(&nonce_bytes);

    let ciphertext = cipher.encrypt(nonce, plaintext.as_bytes()).map_err(|e| {
        EncryptionError::EncryptionFailed(format!("AES-GCM encryption failed: {}", e))
    })?;

    let mut result = nonce_bytes.to_vec();
    result.extend_from_slice(&ciphertext);

    Ok(general_purpose::STANDARD.encode(result))
}

pub fn decrypt_secret(ciphertext: &str, key: &str) -> Result<String, EncryptionError> {
    let key_bytes = general_purpose::STANDARD.decode(key).map_err(|e| {
        EncryptionError::InvalidKey(format!("Failed to decode key: {}", e))
    })?;

    if key_bytes.len() != 32 {
        return Err(EncryptionError::InvalidKey(format!(
            "Key must be 32 bytes, got {}",
            key_bytes.len()
        )));
    }

    let cipher = Aes256Gcm::new_from_slice(&key_bytes).map_err(|e| {
        EncryptionError::InvalidKey(format!("Failed to create cipher: {}", e))
    })?;

    let encrypted_data = general_purpose::STANDARD.decode(ciphertext).map_err(|e| {
        EncryptionError::InvalidCiphertext(format!("Failed to decode ciphertext: {}", e))
    })?;

    if encrypted_data.len() < NONCE_SIZE {
        return Err(EncryptionError::InvalidCiphertext(format!(
            "Ciphertext too short, expected at least {} bytes",
            NONCE_SIZE
        )));
    }

    let (nonce_bytes, ciphertext_bytes) = encrypted_data.split_at(NONCE_SIZE);
    let nonce = Nonce::from_slice(nonce_bytes);

    let plaintext_bytes = cipher.decrypt(nonce, ciphertext_bytes).map_err(|e| {
        EncryptionError::DecryptionFailed(format!("AES-GCM decryption failed: {}", e))
    })?;

    String::from_utf8(plaintext_bytes).map_err(|e| {
        EncryptionError::DecryptionFailed(format!("Invalid UTF-8 in plaintext: {}", e))
    })
}

pub fn decrypt_with_fallback(
    ciphertext: &str,
    current_key: &str,
    previous_key: Option<&str>,
) -> Result<String, EncryptionError> {
    match decrypt_secret(ciphertext, current_key) {
        Ok(plaintext) => Ok(plaintext),
        Err(e) => {
            if let Some(prev_key) = previous_key {
                log::info!("Current key failed, trying previous key for decryption");
                decrypt_secret(ciphertext, prev_key).map_err(|_| {
                    EncryptionError::DecryptionFailed(
                        "Failed to decrypt with both current and previous keys"
                            .to_string(),
                    )
                })
            } else {
                Err(e)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_key() {
        let key = generate_encryption_key();
        let decoded = general_purpose::STANDARD.decode(&key).unwrap();
        assert_eq!(decoded.len(), 32);
    }

    #[test]
    fn test_encrypt_decrypt() {
        let key = generate_encryption_key();
        let plaintext = "my secret value";

        let encrypted = encrypt_secret(plaintext, &key).unwrap();
        let decrypted = decrypt_secret(&encrypted, &key).unwrap();

        assert_eq!(plaintext, decrypted);
    }

    #[test]
    fn test_decrypt_with_wrong_key_fails() {
        let key1 = generate_encryption_key();
        let key2 = generate_encryption_key();
        let plaintext = "my secret";

        let encrypted = encrypt_secret(plaintext, &key1).unwrap();
        let result = decrypt_secret(&encrypted, &key2);

        assert!(result.is_err());
    }

    #[test]
    fn test_decrypt_with_fallback() {
        let old_key = generate_encryption_key();
        let new_key = generate_encryption_key();
        let plaintext = "my secret";

        let encrypted = encrypt_secret(plaintext, &old_key).unwrap();

        let decrypted =
            decrypt_with_fallback(&encrypted, &new_key, Some(&old_key)).unwrap();
        assert_eq!(plaintext, decrypted);
    }
}

pub async fn encrypt_workspace_key(
    workspace_key: &str,
    master_key: &str,
) -> Result<String, EncryptionError> {
    encrypt_secret(workspace_key, master_key)
}

pub async fn decrypt_workspace_key(
    encrypted_workspace_key: &str,
    master_key: &str,
) -> Result<String, EncryptionError> {
    decrypt_secret(encrypted_workspace_key, master_key)
}

pub async fn get_master_encryption_key(
    kms_client: &Option<aws_sdk_kms::Client>,
    app_env: &AppEnv,
) -> Result<String, EncryptionError> {
    match app_env {
        AppEnv::DEV | AppEnv::TEST => {
            use crate::helpers::get_from_env_or_default;
            let key = get_from_env_or_default(
                "MASTER_ENCRYPTION_KEY",
                generate_encryption_key(),
            );
            Ok(key)
        }
        _ => {
            let kms_client = kms_client.as_ref().ok_or_else(|| {
                EncryptionError::EncryptionFailed(
                    "KMS client not available".to_string(),
                )
            })?;
            let decrypted_key = crate::aws::kms::decrypt(
                kms_client.clone(),
                "MASTER_ENCRYPTION_KEY",
            )
            .await;
            Ok(decrypted_key)
        }
    }
}
