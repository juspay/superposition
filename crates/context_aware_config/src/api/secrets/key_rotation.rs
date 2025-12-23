use actix_web::{
    post,
    web::{Data, Json},
};
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    Connection, ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use service_utils::{
    encryption::{
        decrypt_with_fallback, decrypt_workspace_key, encrypt_secret,
        encrypt_workspace_key, generate_encryption_key,
    },
    service::types::{AppState, DbConnection},
};
use superposition_types::{
    api::secrets::{KeyRotationStatus, MasterKeyRotationStatus},
    database::{
        models::{others::Secret, Workspace},
        schema::secrets::dsl::*,
    },
    result as superposition, User,
};

/// Synchronous internal function for rotating workspace encryption key
/// This is called within a diesel transaction, so it cannot be async
fn rotate_workspace_encryption_key_internal(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    workspace: &Workspace,
    schema_name: &str,
    new_master_key: &str,
    old_master_key: &str,
    user_email: &str,
) -> superposition::Result<(i64, chrono::DateTime<chrono::Utc>)> {
    use superposition_types::database::superposition_schema::superposition::workspaces::dsl as ws;

    let is_initial_setup = workspace.encryption_key.is_none();

    let (current_key, previous_key) = if let Some(ref encrypted_current_key) =
        workspace.encryption_key
    {
        let current_key = decrypt_workspace_key(encrypted_current_key, old_master_key)
            .map_err(|e| {
                log::error!(
                    "Failed to decrypt current workspace key for {}/{}: {}",
                    workspace.organisation_id,
                    workspace.workspace_name,
                    e
                );
                superposition::AppError::BadArgument(
                    "Failed to decrypt workspace encryption key".to_string(),
                )
            })?;

        let previous_key = if let Some(ref encrypted_prev) =
            workspace.previous_encryption_key
        {
            Some(
                decrypt_workspace_key(encrypted_prev, old_master_key).map_err(|e| {
                    log::error!(
                        "Failed to decrypt previous workspace key for {}/{}: {}",
                        workspace.organisation_id,
                        workspace.workspace_name,
                        e
                    );
                    superposition::AppError::BadArgument(
                        "Failed to decrypt previous workspace encryption key".to_string(),
                    )
                })?,
            )
        } else {
            None
        };

        (current_key, previous_key)
    } else {
        log::info!(
            "Workspace {}/{} has no encryption key configured. Setting up initial encryption key.",
            workspace.organisation_id,
            workspace.workspace_name
        );
        (generate_encryption_key(), None)
    };

    // Get all secrets for this workspace
    let all_secrets: Vec<Secret> =
        secrets.schema_name(&schema_name.to_string()).load(conn)?;

    // Generate new workspace key
    let new_key = generate_encryption_key();

    // Encrypt new workspace key with new master key (block_on for async call in sync context)
    let encrypted_new_key =
        encrypt_workspace_key(&new_key, new_master_key).map_err(|e| {
            log::error!("Failed to encrypt new workspace key: {}", e);
            superposition::AppError::BadArgument(
                "Failed to encrypt new workspace key".to_string(),
            )
        })?;

    // Encrypt current key as previous key with new master key
    let encrypted_previous_key = encrypt_workspace_key(&current_key, new_master_key)
        .map_err(|e| {
            log::error!("Failed to encrypt previous workspace key: {}", e);
            superposition::AppError::BadArgument(
                "Failed to encrypt previous workspace key".to_string(),
            )
        })?;

    let mut secrets_re_encrypted = 0i64;

    // Re-encrypt all secrets with new workspace key
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

        let new_encrypted_value =
            encrypt_secret(&decrypted_value, &new_key).map_err(|e| {
                superposition::AppError::BadArgument(format!(
                    "Failed to encrypt secret '{}' with new key: {}",
                    secret.name.0, e
                ))
            })?;

        diesel::update(secrets.find(&secret.name))
            .set((
                encrypted_value.eq(&new_encrypted_value),
                key_version.eq(secret.key_version + 1),
                last_modified_by.eq(user_email),
                last_modified_at.eq(chrono::Utc::now()),
            ))
            .schema_name(&schema_name.to_string())
            .execute(conn)?;

        secrets_re_encrypted += 1;
    }

    let rotation_time = chrono::Utc::now();

    // Update workspace with new encrypted keys
    diesel::update(
        ws::workspaces.find((&workspace.organisation_id, &workspace.workspace_name)),
    )
    .set((
        ws::encryption_key.eq(&encrypted_new_key),
        ws::previous_encryption_key.eq(Some(&encrypted_previous_key)),
        ws::key_rotation_at.eq(Some(rotation_time)),
    ))
    .execute(conn)?;

    let action = if is_initial_setup {
        "set up initial"
    } else {
        "rotated"
    };
    log::info!(
        "Successfully {} encryption key for workspace {}/{}. Re-encrypted {} secrets.",
        action,
        workspace.organisation_id,
        workspace.workspace_name,
        secrets_re_encrypted
    );

    Ok((secrets_re_encrypted, rotation_time))
}

#[post("/rotate-workspace-key")]
pub async fn rotate_encryption_key(
    user: User,
    db_conn: DbConnection,
    workspace_id: service_utils::service::types::WorkspaceId,
    org_id: service_utils::service::types::OrganisationId,
    schema_name: service_utils::service::types::SchemaName,
    app_state: Data<AppState>,
) -> superposition::Result<Json<KeyRotationStatus>> {
    use superposition_types::database::superposition_schema::superposition::workspaces::dsl as ws;

    let DbConnection(mut conn) = db_conn;

    let master_key = app_state.master_key.clone();

    let workspace: Workspace = ws::workspaces
        .filter(ws::workspace_name.eq(&workspace_id.0))
        .filter(ws::organisation_id.eq(&org_id.0))
        .first(&mut conn)?;

    let is_initial_setup = workspace.encryption_key.is_none();
    let user_email = user.get_email();
    let schema_name_str = schema_name.0.clone();

    let result = conn.transaction::<KeyRotationStatus, superposition::AppError, _>(|conn| {
        let (secrets_re_encrypted, rotation_timestamp) = rotate_workspace_encryption_key_internal(
            conn,
            &workspace,
            &schema_name_str,
            &master_key,
            &master_key, // Same master key for both encrypt/decrypt in workspace rotation
            &user_email,
        )?;

        Ok(KeyRotationStatus {
            success: true,
            secrets_re_encrypted,
            rotation_timestamp,
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

#[post("/rotate-master-key")]
pub async fn rotate_master_key(
    user: User,
    db_conn: DbConnection,
    app_state: Data<AppState>,
) -> superposition::Result<Json<MasterKeyRotationStatus>> {
    use superposition_types::database::superposition_schema::superposition::workspaces::dsl as ws;

    let DbConnection(mut conn) = db_conn;

    let new_master_key = app_state.master_key.clone();
    let previous_master_key = app_state.previous_master_key.clone();

    let all_workspaces: Vec<Workspace> = ws::workspaces.load(&mut conn)?;

    let total_workspaces = all_workspaces.len() as i64;
    let user_email = user.get_email();

    let result = conn.transaction::<MasterKeyRotationStatus, superposition::AppError, _>(|conn| {
        let mut workspaces_rotated = 0i64;
        let mut total_secrets_re_encrypted = 0i64;

        for workspace in &all_workspaces {
            // Skip workspaces without encryption keys configured
            if workspace.encryption_key.is_none() {
                log::info!(
                    "Skipping workspace {}/{} - no encryption key configured",
                    workspace.organisation_id,
                    workspace.workspace_name
                );
                continue;
            }

            // Derive schema name from workspace (adjust based on your schema naming convention)
            let schema_name = format!("{}_{}", workspace.organisation_id, workspace.workspace_name);

            match rotate_workspace_encryption_key_internal(
                conn,
                workspace,
                &schema_name,
                &new_master_key,
                &previous_master_key,
                &user_email,
            ) {
                Ok((secrets_count, _)) => {
                    workspaces_rotated += 1;
                    total_secrets_re_encrypted += secrets_count;
                }
                Err(e) => {
                    let workspace_identifier = format!(
                        "{}/{}",
                        workspace.organisation_id, workspace.workspace_name
                    );
                    log::error!(
                        "Failed to rotate keys for workspace {}: {}",
                        workspace_identifier,
                        e
                    );
                    // Return error to rollback transaction on any failure
                    return Err(e);
                }
            }
        }

        let rotation_time = chrono::Utc::now();

        log::info!(
            "Successfully rotated master key. Rotated {} workspaces, re-encrypted {} secrets.",
            workspaces_rotated,
            total_secrets_re_encrypted
        );

        Ok(MasterKeyRotationStatus {
            success: true,
            workspaces_rotated,
            total_workspaces,
            total_secrets_re_encrypted,
            rotation_timestamp: rotation_time,
            message: format!(
                "Successfully rotated master key for {} workspace(s) and re-encrypted {} secret(s)",
                workspaces_rotated, total_secrets_re_encrypted
            ),
        })
    })?;

    Ok(Json(result))
}
