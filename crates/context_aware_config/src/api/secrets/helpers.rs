use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use secrecy::ExposeSecret;
use service_utils::{
    encryption::{decrypt_with_fallback, encrypt_secret},
    service::types::SchemaName,
};
use superposition_macros::bad_argument;
use superposition_types::{
    DBConnection,
    database::{models::others::Secret, schema::secrets::dsl::*},
    result as superposition,
};

pub fn re_encrypt_secrets(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    current_key: &secrecy::SecretBox<str>,
    new_key: &secrecy::SecretString,
    user_email: &str,
) -> superposition::Result<i64> {
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
