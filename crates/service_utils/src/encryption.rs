use aes_gcm::{
    Aes256Gcm, Nonce,
    aead::{Aead, KeyInit, OsRng},
};
use base64::{Engine, engine::general_purpose};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use rand::RngCore;
use secrecy::{ExposeSecret, SecretString};
use superposition_macros::bad_argument;
use superposition_types::{
    DBConnection,
    database::{
        models::others::Secret, schema::secrets::dsl::*,
        superposition_schema::superposition::workspaces,
    },
    result,
};

use crate::service::types::{AppEnv, SchemaName, WorkspaceContext};

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
    master_key
        .as_ref()
        .ok_or(EncryptionError::MasterKeyNotConfigured)
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
                .map(SecretString::from);

            Ok((master_key, previous_master_key))
        }
        _ => {
            let kms_client = kms_client.as_ref().ok_or_else(|| {
                EncryptionError::EncryptionFailed("KMS client not available".to_string())
            })?;
            let decrypted_master_key =
                crate::aws::kms::decrypt_opt(kms_client.clone(), "MASTER_ENCRYPTION_KEY")
                    .await
                    .map(SecretString::from);
            let decrypted_previous_master_key = crate::aws::kms::decrypt_opt(
                kms_client.clone(),
                "PREVIOUS_MASTER_ENCRYPTION_KEY",
            )
            .await
            .map(SecretString::from);

            Ok((decrypted_master_key, decrypted_previous_master_key))
        }
    }
}

pub fn re_encrypt_secrets(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    current_key: &secrecy::SecretBox<str>,
    new_key: &secrecy::SecretString,
    user_email: &str,
) -> result::Result<i64> {
    let all_secrets: Vec<Secret> = secrets.schema_name(schema_name).load(conn)?;

    if all_secrets.is_empty() {
        return Ok(0);
    }

    let now = chrono::Utc::now();
    let current_key_str =
        secrecy::SecretString::from(current_key.expose_secret().to_string());

    for secret in &all_secrets {
        let decrypted_value =
            decrypt_with_fallback(&secret.encrypted_value, &current_key_str, None)
                .map_err(|e| {
                    bad_argument!("Failed to decrypt secret '{}': {}", secret.name.0, e)
                })?;

        let new_encrypted_value =
            encrypt_secret(decrypted_value.expose_secret(), new_key).map_err(|e| {
                bad_argument!(
                    "Failed to encrypt secret '{}' with new key: {}",
                    secret.name.0,
                    e
                )
            })?;

        diesel::update(secrets.find(&secret.name))
            .set((
                encrypted_value.eq(&new_encrypted_value),
                last_modified_by.eq(user_email),
                last_modified_at.eq(now),
                change_reason.eq("Key rotation"),
            ))
            .schema_name(schema_name)
            .execute(conn)?;
    }

    Ok(all_secrets.len() as i64)
}

pub fn rotate_workspace_encryption_key_helper(
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
    new_master_key: &SecretString,
    old_master_key: &SecretString,
    user_email: &str,
) -> result::Result<(i64, chrono::DateTime<chrono::Utc>)> {
    let existing_key = workspace_context.settings.encryption_key.clone();

    let current_key =
        decrypt_workspace_key(&existing_key, old_master_key).map_err(|e| {
            log::error!("Failed to decrypt current workspace key for  {}", e);
            bad_argument!("Failed to decrypt workspace encryption key")
        })?;

    let new_key = SecretString::from(generate_encryption_key());
    let encrypted_new_key =
        encrypt_workspace_key(new_key.expose_secret(), new_master_key).map_err(|e| {
            log::error!("Failed to encrypt new workspace key: {}", e);
            bad_argument!("Failed to encrypt new workspace key: {}", e)
        })?;

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

    // Encrypt previous key with new master key (if it exists)
    let encrypted_previous_key: Option<String> = Some(
        encrypt_workspace_key(current_key.expose_secret(), new_master_key).map_err(
            |e| {
                log::error!("Failed to encrypt previous workspace key: {}", e);
                bad_argument!("Failed to encrypt previous workspace key: {}", e)
            },
        )?,
    );

    let rotation_time = chrono::Utc::now();

    // Update workspace with new encrypted keys
    diesel::update(workspaces::dsl::workspaces.find((
        &workspace_context.organisation_id.0,
        &workspace_context.settings.workspace_name,
    )))
    .set((
        workspaces::dsl::encryption_key.eq(&encrypted_new_key),
        workspaces::dsl::previous_encryption_key.eq(encrypted_previous_key),
        workspaces::dsl::key_rotated_at.eq(Some(rotation_time)),
    ))
    .execute(conn)?;

    log::info!(
        "Rotated encryption key for workspace {}. total number of re-encrypted secrets {}",
        workspace_context.settings.workspace_name,
        total_secrets_re_encrypted
    );

    Ok((total_secrets_re_encrypted, rotation_time))
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
