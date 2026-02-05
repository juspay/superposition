use aes_gcm::{
    Aes256Gcm, Nonce,
    aead::{Aead, AeadCore, KeyInit, OsRng},
};
use base64::{Engine, engine::general_purpose};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use rand::RngCore;
use secrecy::{ExposeSecret, SecretString};
use superposition_macros::bad_argument;
use superposition_types::{
    DBConnection,
    database::{schema::secrets, superposition_schema::superposition::workspaces},
    result,
};

use crate::service::types::{AppEnv, EncryptionKey, SchemaName, WorkspaceContext};

const NONCE_SIZE: usize = 12;
const ENCRYPTION_KEY_BYTE_LENGTH: usize = 32;

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

pub fn generate_encryption_key() -> SecretString {
    let mut key_bytes = [0u8; ENCRYPTION_KEY_BYTE_LENGTH];
    OsRng.fill_bytes(&mut key_bytes);
    SecretString::from(general_purpose::STANDARD.encode(key_bytes))
}

pub fn encrypt_secret(
    plaintext: &str,
    key: &SecretString,
) -> Result<String, EncryptionError> {
    let key_bytes = general_purpose::STANDARD
        .decode(key.expose_secret())
        .map_err(|e| {
            EncryptionError::InvalidKey(format!("Failed to decode key: {}", e))
        })?;

    if key_bytes.len() != ENCRYPTION_KEY_BYTE_LENGTH {
        return Err(EncryptionError::InvalidKey(format!(
            "Key must be {ENCRYPTION_KEY_BYTE_LENGTH} bytes, got {}",
            key_bytes.len()
        )));
    }

    let cipher = Aes256Gcm::new_from_slice(&key_bytes).map_err(|e| {
        EncryptionError::InvalidKey(format!("Failed to create cipher: {}", e))
    })?;

    let nonce = Aes256Gcm::generate_nonce(OsRng);

    let ciphertext = cipher.encrypt(&nonce, plaintext.as_bytes()).map_err(|e| {
        EncryptionError::EncryptionFailed(format!("AES-GCM encryption failed: {}", e))
    })?;

    let mut result = nonce.to_vec();
    result.extend_from_slice(&ciphertext);

    Ok(general_purpose::STANDARD.encode(result))
}

pub fn decrypt_secret(
    ciphertext: &str,
    key: &SecretString,
) -> Result<SecretString, EncryptionError> {
    let key_bytes = general_purpose::STANDARD
        .decode(key.expose_secret())
        .map_err(|e| {
            EncryptionError::InvalidKey(format!("Failed to decode key: {}", e))
        })?;

    if key_bytes.len() != ENCRYPTION_KEY_BYTE_LENGTH {
        return Err(EncryptionError::InvalidKey(format!(
            "Key must be {ENCRYPTION_KEY_BYTE_LENGTH} bytes, got {}",
            key_bytes.len()
        )));
    }

    let cipher = Aes256Gcm::new_from_slice(&key_bytes).map_err(|e| {
        EncryptionError::InvalidKey(format!("Failed to create cipher: {}", e))
    })?;

    let decoded_data = general_purpose::STANDARD.decode(ciphertext).map_err(|e| {
        EncryptionError::InvalidCiphertext(format!("Failed to decode ciphertext: {}", e))
    })?;

    let Some((nonce_bytes, ciphertext_bytes)) = decoded_data.split_at_checked(NONCE_SIZE)
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
    encryption_key: &EncryptionKey,
) -> Result<SecretString, EncryptionError> {
    match decrypt_secret(ciphertext, &encryption_key.current_key) {
        Ok(plaintext) => Ok(plaintext),
        Err(e) => {
            if let Some(ref prev_key) = encryption_key.previous_key {
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
    workspace_key: &SecretString,
    current_key: &SecretString,
) -> Result<String, EncryptionError> {
    encrypt_secret(workspace_key.expose_secret(), current_key)
}

pub fn decrypt_workspace_key(
    encrypted_workspace_key: &str,
    master_encryption_key: &EncryptionKey,
) -> Result<SecretString, EncryptionError> {
    decrypt_with_fallback(encrypted_workspace_key, master_encryption_key)
}

pub async fn get_master_encryption_keys(
    kms_client: &Option<aws_sdk_kms::Client>,
    app_env: &AppEnv,
) -> Result<Option<EncryptionKey>, EncryptionError> {
    match app_env {
        AppEnv::DEV | AppEnv::TEST => {
            let env_key = std::env::var("MASTER_ENCRYPTION_KEY").ok();
            let Some(current_key) = env_key.map(SecretString::from) else {
                log::info!(
                    "MASTER_ENCRYPTION_KEY not set - secrets functionality will be disabled."
                );
                return Ok(None);
            };

            let previous_key = std::env::var("PREVIOUS_MASTER_ENCRYPTION_KEY")
                .ok()
                .map(SecretString::from);

            Ok(Some(EncryptionKey {
                current_key,
                previous_key,
            }))
        }
        _ => {
            let kms_client = kms_client.clone().unwrap();
            let decrypted_master_key =
                crate::aws::kms::decrypt_opt(kms_client.clone(), "MASTER_ENCRYPTION_KEY")
                    .await
                    .map(SecretString::from);
            let Some(current_key) = decrypted_master_key else {
                log::info!(
                    "MASTER_ENCRYPTION_KEY not set - secrets functionality will be disabled."
                );
                return Ok(None);
            };

            let previous_key = crate::aws::kms::decrypt_opt(
                kms_client,
                "PREVIOUS_MASTER_ENCRYPTION_KEY",
            )
            .await
            .map(SecretString::from);

            Ok(Some(EncryptionKey {
                current_key,
                previous_key,
            }))
        }
    }
}

fn re_encrypt_secrets(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    current_key: &SecretString,
    new_key: &SecretString,
    user_email: &str,
) -> result::Result<i64> {
    let all_secrets: Vec<(String, String)> = secrets::table
        .select((secrets::name, secrets::encrypted_value))
        .schema_name(schema_name)
        .load(conn)?;

    if all_secrets.is_empty() {
        return Ok(0);
    }

    let now = chrono::Utc::now();

    for (name, encrypted_value) in &all_secrets {
        let decrypted_value = decrypt_secret(encrypted_value, current_key)
            .map_err(|e| bad_argument!("Failed to decrypt secret '{}': {}", name, e))?;

        let new_encrypted_value =
            encrypt_secret(decrypted_value.expose_secret(), new_key).map_err(|e| {
                bad_argument!("Failed to encrypt secret '{}' with new key: {}", name, e)
            })?;

        diesel::update(secrets::table.find(&name))
            .set((
                secrets::encrypted_value.eq(&new_encrypted_value),
                secrets::last_modified_by.eq(user_email),
                secrets::last_modified_at.eq(now),
                secrets::change_reason.eq("Key rotation"),
            ))
            .schema_name(schema_name)
            .execute(conn)?;
    }

    Ok(all_secrets.len() as i64)
}

pub fn rotate_workspace_encryption_key_helper(
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
    master_encryption_key: &EncryptionKey,
    user_email: &str,
) -> result::Result<i64> {
    let current_key = decrypt_workspace_key(
        &workspace_context.settings.encryption_key,
        master_encryption_key,
    )
    .map_err(|e| {
        log::error!("Failed to decrypt current workspace key for  {}", e);
        bad_argument!("Failed to decrypt workspace encryption key")
    })?;

    let new_key = generate_encryption_key();
    let encrypted_new_key =
        encrypt_workspace_key(&new_key, &master_encryption_key.current_key).map_err(
            |e| {
                log::error!("Failed to encrypt new workspace key: {}", e);
                bad_argument!("Failed to encrypt new workspace key: {}", e)
            },
        )?;

    // Re-encrypt secrets if we have an existing key, otherwise this is initialization
    let total_secrets_re_encrypted = re_encrypt_secrets(
        conn,
        &workspace_context.schema_name,
        &current_key,
        &new_key,
        user_email,
    )
    .unwrap_or_else(|_| {
        log::info!(
            "Initializing encryption key for workspace {}",
            workspace_context.settings.workspace_name
        );
        0 // Just return i64, not Result<i64>
    });

    let rotation_time = chrono::Utc::now();

    // Update workspace with new encrypted key
    diesel::update(workspaces::dsl::workspaces.find((
        &workspace_context.organisation_id.0,
        &workspace_context.settings.workspace_name,
    )))
    .set((
        workspaces::dsl::encryption_key.eq(&encrypted_new_key),
        workspaces::dsl::key_rotated_at.eq(Some(rotation_time)),
        workspaces::dsl::last_modified_at.eq(rotation_time),
        workspaces::dsl::last_modified_by.eq(user_email),
        workspaces::dsl::change_reason.eq("Encryption key rotation"),
    ))
    .execute(conn)?;

    log::info!(
        "Rotated encryption key for workspace {}. total number of re-encrypted secrets {}",
        workspace_context.settings.workspace_name,
        total_secrets_re_encrypted
    );

    Ok(total_secrets_re_encrypted)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_key() {
        let key = generate_encryption_key();
        let decoded = general_purpose::STANDARD
            .decode(key.expose_secret())
            .unwrap();
        assert_eq!(decoded.len(), ENCRYPTION_KEY_BYTE_LENGTH);
    }

    #[test]
    fn test_encrypt_decrypt() {
        let key = generate_encryption_key();
        let plaintext = "my secret value";

        let encrypted = encrypt_secret(plaintext, &key).unwrap();
        let decrypted = decrypt_secret(&encrypted, &key).unwrap();

        assert_eq!(plaintext, decrypted.expose_secret());
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

        let encryption_key = EncryptionKey {
            current_key: new_key,
            previous_key: Some(old_key),
        };

        let decrypted = decrypt_with_fallback(&encrypted, &encryption_key).unwrap();
        assert_eq!(plaintext, decrypted.expose_secret());
    }
}
