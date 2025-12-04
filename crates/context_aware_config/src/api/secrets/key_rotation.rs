use actix_web::{post, web::Json};
use diesel::{Connection, ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::{
    encryption::{decrypt_with_fallback, encrypt_secret, generate_encryption_key},
    service::types::DbConnection,
};
use superposition_types::{
    api::secrets::{KeyRotationStatus, RotateKeyRequest},
    database::{
        models::{others::Secret, Workspace},
        schema::secrets::dsl::*,
    },
    result as superposition, User,
};

#[post("/rotate-key")]
pub async fn rotate_encryption_key(
    _req: Json<RotateKeyRequest>,
    user: User,
    db_conn: DbConnection,
    workspace_id: service_utils::service::types::WorkspaceId,
    org_id: service_utils::service::types::OrganisationId,
    schema_name: service_utils::service::types::SchemaName,
) -> superposition::Result<Json<KeyRotationStatus>> {
    let DbConnection(mut conn) = db_conn;

    let result = conn.transaction::<KeyRotationStatus, superposition::AppError, _>(|conn| {
        use superposition_types::database::superposition_schema::superposition::workspaces::dsl as ws;

        let workspace: Workspace = ws::workspaces
            .filter(ws::workspace_name.eq(&workspace_id.0))
            .filter(ws::organisation_id.eq(&org_id.0))
            .first(conn)?;

        let current_key = workspace
            .encryption_key
            .ok_or_else(|| superposition::AppError::BadArgument(
                "Workspace has no encryption key configured".to_string()
            ))?;

        let all_secrets: Vec<Secret> = secrets
            .schema_name(&schema_name)
            .load(conn)?;

        let new_key = generate_encryption_key();

        let mut secrets_re_encrypted = 0i64;

        for secret in all_secrets {
            let decrypted_value = decrypt_with_fallback(
                &secret.encrypted_value,
                &current_key,
                workspace.previous_encryption_key.as_deref(),
            )
            .map_err(|e| {
                superposition::AppError::BadArgument(format!(
                    "Failed to decrypt secret '{}': {}",
                    secret.name.0, e
                ))
            })?;

            let new_encrypted_value = encrypt_secret(&decrypted_value, &new_key)
                .map_err(|e| {
                    superposition::AppError::BadArgument(format!(
                        "Failed to encrypt secret '{}' with new key: {}",
                        secret.name.0, e
                    ))
                })?;

            diesel::update(secrets.find(&secret.name))
                .set((
                    encrypted_value.eq(&new_encrypted_value),
                    key_version.eq(secret.key_version + 1),
                    last_modified_by.eq(user.get_email()),
                    last_modified_at.eq(chrono::Utc::now()),
                ))
                .schema_name(&schema_name)
                .execute(conn)?;

            secrets_re_encrypted += 1;
        }

        let rotation_time = chrono::Utc::now();

        diesel::update(ws::workspaces.find((&workspace.organisation_id, &workspace.workspace_name)))
            .set((
                ws::encryption_key.eq(&new_key),
                ws::previous_encryption_key.eq(Some(&current_key)),
                ws::key_rotation_at.eq(Some(rotation_time)),
            ))
            .execute(conn)?;

        log::info!(
            "Successfully rotated encryption key for workspace {}/{}. Re-encrypted {} secrets.",
            org_id.0,
            workspace_id.0,
            secrets_re_encrypted
        );

        Ok(KeyRotationStatus {
            success: true,
            secrets_re_encrypted,
            rotation_timestamp: rotation_time,
            message: format!(
                "Successfully rotated encryption key and re-encrypted {} secret(s)",
                secrets_re_encrypted
            ),
        })
    })?;

    Ok(Json(result))
}
