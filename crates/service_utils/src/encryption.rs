use aes_gcm::{
    Aes256Gcm, Nonce,
    aead::{Aead, KeyInit, OsRng},
};
use base64::{Engine, engine::general_purpose};
use rand::RngCore;
use secrecy::{ExposeSecret, SecretString};

use crate::service::types::AppEnv;

const NONCE_SIZE: usize = 12;

#[derive(Debug)]
pub enum EncryptionError {
    EncryptionFailed(String),
    DecryptionFailed(String),
    InvalidKey(String),
    InvalidCiphertext(String),
    MasterKeyNotConfigured,
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
            EncryptionError::MasterKeyNotConfigured => {
                write!(
                    f,
                    "Master encryption key not configured. Generate it via UI and configure it in your deployment"
                )
            }
        }
    }
}

impl std::error::Error for EncryptionError {}

impl From<EncryptionError> for superposition_types::result::AppError {
    fn from(err: EncryptionError) -> Self {
        superposition_types::result::AppError::BadArgument(err.to_string())
    }
}

/// Returns a reference to the master key if available, or an error if not.
/// Use this in functions that require the master key.
pub fn require_master_key(
    master_key: &Option<SecretString>,
) -> Result<&SecretString, EncryptionError> {
    master_key.as_ref().ok_or(EncryptionError::MasterKeyNotConfigured)
}

pub fn generate_encryption_key() -> String {
    let mut key_bytes = [0u8; 32];
    OsRng.fill_bytes(&mut key_bytes);
    general_purpose::STANDARD.encode(key_bytes)
}

pub fn encrypt_secret(
    plaintext: &str,
    key: &secrecy::SecretString,
) -> Result<String, EncryptionError> {
    let key_bytes = general_purpose::STANDARD
        .decode(key.expose_secret())
        .map_err(|e| {
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

pub fn decrypt_secret(
    ciphertext: &str,
    key: &secrecy::SecretString,
) -> Result<SecretString, EncryptionError> {
    let key_bytes = general_purpose::STANDARD
        .decode(key.expose_secret())
        .map_err(|e| {
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

    let Some((nonce_bytes, ciphertext_bytes)) =
        encrypted_data.split_at_checked(NONCE_SIZE)
    else {
        return Err(EncryptionError::InvalidCiphertext(format!(
            "Ciphertext too short, expected at least {} bytes",
            NONCE_SIZE
        )));
    };
    let nonce = Nonce::from_slice(nonce_bytes);

    let plaintext_bytes = cipher.decrypt(nonce, ciphertext_bytes).map_err(|e| {
        EncryptionError::DecryptionFailed(format!("AES-GCM decryption failed: {}", e))
    })?;

    String::from_utf8(plaintext_bytes)
        .map(SecretString::from)
        .map_err(|e| {
            EncryptionError::DecryptionFailed(format!(
                "Invalid UTF-8 in plaintext: {}",
                e
            ))
        })
}

pub fn decrypt_with_fallback(
    ciphertext: &str,
    current_key: &SecretString,
    previous_key: Option<&SecretString>,
) -> Result<SecretString, EncryptionError> {
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

pub fn encrypt_workspace_key(
    workspace_key: &str,
    master_key: &secrecy::SecretString,
) -> Result<String, EncryptionError> {
    encrypt_secret(workspace_key, master_key)
}

pub fn decrypt_workspace_key(
    encrypted_workspace_key: &str,
    master_key: &secrecy::SecretString,
) -> Result<secrecy::SecretString, EncryptionError> {
    decrypt_secret(encrypted_workspace_key, master_key)
}

pub async fn get_master_encryption_keys(
    kms_client: &Option<aws_sdk_kms::Client>,
    app_env: &AppEnv,
) -> Result<(Option<SecretString>, Option<SecretString>), EncryptionError> {
    match app_env {
        AppEnv::DEV | AppEnv::TEST => {
            let env_key = std::env::var("MASTER_ENCRYPTION_KEY").ok();
            let master_key = env_key.map(SecretString::from);
            if master_key.is_none() {
                log::info!(
                    "MASTER_ENCRYPTION_KEY not set - secrets functionality will be disabled."
                );
            }
            let previous_master_key = std::env::var("PREVIOUS_MASTER_ENCRYPTION_KEY")
                .ok()
                .filter(|s| !s.is_empty())
                .map(SecretString::from);

            Ok((master_key, previous_master_key))
        }
        _ => {
            let kms_client = kms_client.as_ref().ok_or_else(|| {
                EncryptionError::EncryptionFailed("KMS client not available".to_string())
            })?;
            let decrypted_master_key =
                crate::aws::kms::decrypt(kms_client.clone(), "MASTER_ENCRYPTION_KEY")
                    .await;
            let decrypted_previous_master_key = crate::aws::kms::decrypt(
                kms_client.clone(),
                "PREVIOUS_MASTER_ENCRYPTION_KEY",
            )
            .await;

            // Only include master key if it's actually set (non-empty)
            let master_key = if decrypted_master_key.is_empty() {
                None
            } else {
                Some(SecretString::from(decrypted_master_key))
            };

            // Only include previous key if it's actually set (non-empty)
            let previous_key = if decrypted_previous_master_key.is_empty() {
                None
            } else {
                Some(SecretString::from(decrypted_previous_master_key))
            };

            Ok((master_key, previous_key))
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
        let key = secrecy::SecretString::from(generate_encryption_key());
        let plaintext = "my secret value";

        let encrypted = encrypt_secret(plaintext, &key).unwrap();
        let decrypted = decrypt_secret(&encrypted, &key).unwrap();

        assert_eq!(plaintext, decrypted.expose_secret());
    }

    #[test]
    fn test_decrypt_with_wrong_key_fails() {
        let key1 = secrecy::SecretString::from(generate_encryption_key());
        let key2 = secrecy::SecretString::from(generate_encryption_key());
        let plaintext = "my secret";

        let encrypted = encrypt_secret(plaintext, &key1).unwrap();
        let result = decrypt_secret(&encrypted, &key2);

        assert!(result.is_err());
    }

    #[test]
    fn test_decrypt_with_fallback() {
        let old_key = secrecy::SecretString::from(generate_encryption_key());
        let new_key = secrecy::SecretString::from(generate_encryption_key());
        let plaintext = "my secret";

        let encrypted = encrypt_secret(plaintext, &old_key).unwrap();

        let decrypted =
            decrypt_with_fallback(&encrypted, &new_key, Some(&old_key)).unwrap();
        assert_eq!(plaintext, decrypted.expose_secret());
    }
}
