use actix_web::{
    Scope, delete, get, patch, post,
    web::{self, Data, Json, Query},
};
use diesel::{Connection, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use secrecy::SecretString;
use service_utils::{
    encryption::{
        decrypt_workspace_key, encrypt_secret, rotate_workspace_encryption_key_helper,
    },
    service::types::{
        AppState, DbConnection, EncryptionKey, OrganisationId, SchemaName,
        WorkspaceContext, WorkspaceId,
    },
};
use superposition_derives::authorized;
use superposition_macros::{bad_argument, unexpected_error};
use superposition_types::{
    PaginatedResponse, SortBy, User,
    api::secrets::{
        CreateSecretRequest, MasterEncryptionKeyRotationResponse, SecretFilters,
        SecretResponse, SortOn, UpdateSecretRequest,
    },
    custom_query::PaginationParams,
    database::{
        models::{Workspace, others::Secret},
        schema::secrets,
        superposition_schema::superposition::workspaces,
    },
    result::{self as superposition},
};

use super::types::UpdateSecretChangeset;

pub fn endpoints() -> Scope {
    web::scope("")
        .service(list_handler)
        .service(create_handler)
        .service(get_handler)
        .service(update_handler)
        .service(delete_handler)
}

pub fn master_key_endpoints() -> Scope {
    web::scope("").service(rotate_master_key_handler)
}

fn get_workspace_encryption_key(
    workspace_context: &WorkspaceContext,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<SecretString> {
    let workspace: &Workspace = &workspace_context.settings;

    let Some(master_encryption_key) = master_encryption_key else {
        log::error!("Master encryption key not configured");
        return Err(bad_argument!(
            "Master encryption key not configured. Configure master encryption key to use secrets"
        ));
    };

    let decrypted_key =
        decrypt_workspace_key(&workspace.encryption_key, master_encryption_key).map_err(
            |e| {
                log::error!("Failed to decrypt workspace key: {}", e);
                unexpected_error!("Failed to decrypt workspace encryption key")
            },
        )?;

    Ok(decrypted_key)
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    pagination: Query<PaginationParams>,
    filters: Query<SecretFilters>,
) -> superposition::Result<Json<PaginatedResponse<SecretResponse>>> {
    let DbConnection(mut conn) = db_conn;

    let filters_inner = filters.into_inner();

    let query_builder = |filters: &SecretFilters| {
        let mut builder = secrets::table
            .schema_name(&workspace_context.schema_name)
            .into_boxed();

        if let Some(ref secret_names) = filters.name {
            builder = builder.filter(secrets::name.eq_any(secret_names.0.clone()));
        }

        if let Some(ref creators) = filters.created_by {
            builder = builder.filter(secrets::created_by.eq_any(creators.0.clone()));
        }

        if let Some(ref last_modifiers) = filters.last_modified_by {
            builder = builder
                .filter(secrets::last_modified_by.eq_any(last_modifiers.0.clone()));
        }

        builder
    };

    if let Some(true) = pagination.all {
        let result = query_builder(&filters_inner).get_results::<Secret>(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(
            result.into_iter().map(SecretResponse::from).collect(),
        )));
    }

    let base_query = query_builder(&filters_inner);
    let count_query = query_builder(&filters_inner);

    let n_secrets: i64 = count_query.count().get_result(&mut conn)?;
    let limit = pagination.count.unwrap_or(10);

    let sort_on = filters_inner.sort_on.unwrap_or_default();
    let sort_by_order = filters_inner.sort_by.unwrap_or_default();

    #[rustfmt::skip]
    let base_query = match (sort_on, sort_by_order) {
        (SortOn::Name,           SortBy::Asc)  => base_query.order(secrets::name.asc()),
        (SortOn::Name,           SortBy::Desc) => base_query.order(secrets::name.desc()),
        (SortOn::CreatedAt,      SortBy::Asc)  => base_query.order(secrets::created_at.asc()),
        (SortOn::CreatedAt,      SortBy::Desc) => base_query.order(secrets::created_at.desc()),
        (SortOn::LastModifiedAt, SortBy::Asc)  => base_query.order(secrets::last_modified_at.asc()),
        (SortOn::LastModifiedAt, SortBy::Desc) => base_query.order(secrets::last_modified_at.desc()),
    };

    let mut builder = base_query.limit(limit);
    if let Some(page) = pagination.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let result = builder.load::<Secret>(&mut conn)?;

    let total_pages = (n_secrets as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_secrets,
        data: result.into_iter().map(SecretResponse::from).collect(),
    }))
}

#[authorized]
#[post("")]
async fn create_handler(
    req: web::Json<CreateSecretRequest>,
    user: User,
    db_conn: DbConnection,
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
) -> superposition::Result<Json<SecretResponse>> {
    let req = req.into_inner();

    let DbConnection(mut conn) = db_conn;

    let encryption_key =
        get_workspace_encryption_key(&workspace_context, &state.master_encryption_key)?;

    let encrypted_secret_value = encrypt_secret(&req.value, &encryption_key)
        .map_err(|e| bad_argument!("Encryption failed: {}", e))?;

    let now = chrono::Utc::now();

    let new_secret = Secret {
        name: req.name,
        encrypted_value: encrypted_secret_value,
        description: req.description,
        change_reason: req.change_reason.clone(),
        created_at: now,
        last_modified_at: now,
        created_by: user.get_email(),
        last_modified_by: user.get_email(),
    };

    let created_secret = diesel::insert_into(secrets::table)
        .values(&new_secret)
        .returning(Secret::as_returning())
        .schema_name(&workspace_context.schema_name)
        .get_result(&mut conn)?;

    Ok(Json(SecretResponse::from(created_secret)))
}

#[authorized]
#[get("/{secret_name}")]
async fn get_handler(
    path: web::Path<String>,
    db_conn: DbConnection,
    workspace_context: WorkspaceContext,
) -> superposition::Result<Json<SecretResponse>> {
    let DbConnection(mut conn) = db_conn;

    let secret_name = path.into_inner();

    let secret = secrets::table
        .filter(secrets::name.eq(secret_name))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Secret>(&mut conn)?;

    Ok(Json(SecretResponse::from(secret)))
}

#[authorized]
#[patch("/{secret_name}")]
async fn update_handler(
    path: web::Path<String>,
    req: web::Json<UpdateSecretRequest>,
    user: User,
    db_conn: DbConnection,
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
) -> superposition::Result<Json<SecretResponse>> {
    let DbConnection(mut conn) = db_conn;
    let secret_name = path.into_inner();
    let req_inner = req.into_inner();

    let encrypted_value_opt = if let Some(ref plaintext_value) = req_inner.value {
        let encryption_key = get_workspace_encryption_key(
            &workspace_context,
            &state.master_encryption_key,
        )?;
        Some(
            encrypt_secret(plaintext_value, &encryption_key)
                .map_err(|e| bad_argument!("Encryption failed: {}", e))?,
        )
    } else {
        None
    };

    let changeset = UpdateSecretChangeset {
        encrypted_value: encrypted_value_opt,
        description: req_inner.description,
        change_reason: req_inner.change_reason,
    };

    let updated_secret = diesel::update(secrets::table)
        .filter(secrets::name.eq(secret_name))
        .set((
            changeset,
            secrets::last_modified_at.eq(chrono::Utc::now()),
            secrets::last_modified_by.eq(user.get_email()),
        ))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Secret>(&mut conn)?;

    Ok(Json(SecretResponse::from(updated_secret)))
}

#[authorized]
#[delete("/{secret_name}")]
async fn delete_handler(
    path: web::Path<String>,
    user: User,
    db_conn: DbConnection,
    workspace_context: WorkspaceContext,
) -> superposition::Result<Json<SecretResponse>> {
    let DbConnection(mut conn) = db_conn;
    let secret_name = path.into_inner();

    diesel::update(secrets::table)
        .filter(secrets::name.eq(&secret_name))
        .set((
            secrets::last_modified_at.eq(chrono::Utc::now()),
            secrets::last_modified_by.eq(user.get_email()),
        ))
        .schema_name(&workspace_context.schema_name)
        .execute(&mut conn)?;

    let deleted_secret = diesel::delete(secrets::table)
        .filter(secrets::name.eq(&secret_name))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Secret>(&mut conn)?;

    Ok(Json(SecretResponse::from(deleted_secret)))
}

// Note: Not to be used during mid migration - to avoid old replicas from failing
#[authorized]
#[post("/rotate")]
pub async fn rotate_master_key_handler(
    user: User,
    db_conn: DbConnection,
    state: Data<AppState>,
) -> superposition::Result<Json<MasterEncryptionKeyRotationResponse>> {
    let DbConnection(mut conn) = db_conn;

    let Some(ref master_encryption_key) = state.master_encryption_key else {
        log::error!("Master encryption key not configured");
        return Err(bad_argument!(
            "Master encryption key not configured. Configure master encryption key to rotate keys"
        ));
    };

    master_encryption_key.previous_key.as_ref().ok_or_else(|| {
        bad_argument!(
            "PREVIOUS_MASTER_ENCRYPTION_KEY must be set to rotate master encryption key"
        )
    })?;

    let all_workspaces: Vec<Workspace> = workspaces::table.load(&mut conn)?;

    let user_email = user.get_email();

    let (workspaces_rotated, total_secrets_re_encrypted) = conn
        .transaction::<(i64, i64), superposition::AppError, _>(|conn| {
            let mut workspaces_rotated = 0i64;
            let mut total_secrets_re_encrypted = 0i64;

            for workspace in all_workspaces {
                let workspace_context = WorkspaceContext {
                    workspace_id: WorkspaceId(workspace.workspace_name.clone()),
                    organisation_id: OrganisationId(workspace.organisation_id.clone()),
                    schema_name: SchemaName(workspace.workspace_schema_name.clone()),
                    settings: workspace,
                };
                match rotate_workspace_encryption_key_helper(
                    &workspace_context,
                    conn,
                    master_encryption_key,
                    &user_email,
                ) {
                    Ok(secrets_count) => {
                        workspaces_rotated += 1;
                        total_secrets_re_encrypted += secrets_count;
                    }
                    Err(e) => {
                        log::error!(
                            "Failed to rotate keys for workspace {}: {}",
                            workspace_context.schema_name.0,
                            e
                        );
                        return Err(e);
                    }
                }
            }

            Ok((workspaces_rotated, total_secrets_re_encrypted))
        })?;

    log::info!(
        "Successfully rotated master encryption key. Rotated {} workspaces, re-encrypted {} secrets.",
        workspaces_rotated,
        total_secrets_re_encrypted
    );

    let result = MasterEncryptionKeyRotationResponse {
        workspaces_rotated,
        total_secrets_re_encrypted,
    };

    Ok(Json(result))
}
