use actix_web::{post, web::{Data, Json}};
use diesel::{Connection, ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::{
    encryption::{decrypt_with_fallback, decrypt_workspace_key, encrypt_secret, encrypt_workspace_key, generate_encryption_key, get_master_encryption_key},
    service::types::{AppState, DbConnection},
};
use superposition_macros::unexpected_error;
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
    app_state: Data<AppState>,
) -> superposition::Result<Json<KeyRotationStatus>> {
    use superposition_types::database::superposition_schema::superposition::workspaces::dsl as ws;
    
    let DbConnection(mut conn) = db_conn;

    let master_key = get_master_encryption_key(
        &app_state.kms_client,
        &app_state.app_env,
    )
    .await
    .map_err(|e| {
        log::error!("Failed to get master encryption key: {}", e);
        unexpected_error!("Failed to get master encryption key")
    })?;

    let workspace: Workspace = ws::workspaces
        .filter(ws::workspace_name.eq(&workspace_id.0))
        .filter(ws::organisation_id.eq(&org_id.0))
        .first(&mut conn)?;

    let is_initial_setup = workspace.encryption_key.is_none();
    
    let (current_key, previous_key) = if let Some(ref encrypted_current_key) = workspace.encryption_key {
        let current_key = decrypt_workspace_key(
            encrypted_current_key,
            &master_key,
        )
        .await
        .map_err(|e| {
            log::error!("Failed to decrypt current workspace key: {}", e);
            superposition::AppError::BadArgument(
                "Failed to decrypt workspace encryption key".to_string()
            )
        })?;
        
        let previous_key = if let Some(ref encrypted_prev) = workspace.previous_encryption_key {
            Some(decrypt_workspace_key(
                encrypted_prev,
                &master_key,
            )
            .await
            .map_err(|e| {
                log::error!("Failed to decrypt previous workspace key: {}", e);
                superposition::AppError::BadArgument(
                    "Failed to decrypt previous workspace encryption key".to_string()
                )
            })?)
        } else {
            None
        };
        
        (current_key, previous_key)
    } else {
        log::info!(
            "Workspace {}/{} has no encryption key configured. Setting up initial encryption key.",
            org_id.0,
            workspace_id.0
        );
        (generate_encryption_key(), None)
    };

    let all_secrets: Vec<Secret> = secrets
        .schema_name(&schema_name)
        .load(&mut conn)?;

    let new_key = generate_encryption_key();
    
    let encrypted_new_key = encrypt_workspace_key(
        &new_key,
        &master_key,
    )
    .await
    .map_err(|e| {
        log::error!("Failed to encrypt new workspace key: {}", e);
        superposition::AppError::BadArgument(
            "Failed to encrypt new workspace key".to_string()
        )
    })?;
    
    let encrypted_previous_key = encrypt_workspace_key(
        &current_key,
        &master_key,
    )
    .await
    .map_err(|e| {
        log::error!("Failed to encrypt previous workspace key: {}", e);
        superposition::AppError::BadArgument(
            "Failed to encrypt previous workspace key".to_string()
        )
    })?;

    let result = conn.transaction::<KeyRotationStatus, superposition::AppError, _>(|conn| {
        use superposition_types::database::superposition_schema::superposition::workspaces::dsl as ws;
        
        let mut secrets_re_encrypted = 0i64;

        for secret in &all_secrets {
            let decrypted_value = decrypt_with_fallback(
                &secret.encrypted_value,
                &current_key,
                previous_key.as_deref(),
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
                ws::encryption_key.eq(&encrypted_new_key),
                ws::previous_encryption_key.eq(Some(&encrypted_previous_key)),
                ws::key_rotation_at.eq(Some(rotation_time)),
            ))
            .execute(conn)?;

        let action = if is_initial_setup { "set up initial" } else { "rotated" };
        
        log::info!(
            "Successfully {} encryption key for workspace {}/{}. Re-encrypted {} secrets.",
            action,
            org_id.0,
            workspace_id.0,
            secrets_re_encrypted
        );

        Ok(KeyRotationStatus {
            success: true,
            secrets_re_encrypted,
            rotation_timestamp: rotation_time,
            message: if is_initial_setup {
                format!(
                    "Successfully set up initial encryption key and encrypted {} secret(s)",
                    secrets_re_encrypted
                )
            } else {
                format!(
                    "Successfully rotated encryption key and re-encrypted {} secret(s)",
                    secrets_re_encrypted
                )
            },
        })
    })?;

    Ok(Json(result))
}
