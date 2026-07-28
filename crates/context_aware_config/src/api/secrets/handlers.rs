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
    redis::put_workspace_in_redis,
    service::types::{
        AppState, DbConnection, EncryptionKey, SchemaName, WorkspaceContext,
    },
};
use superposition_derives::{authorized, declare_resource};
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

declare_resource!(Secret);

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
    workspace: &Workspace,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<SecretString> {
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
    let user_email = user.get_email();

    let created_secret =
        conn.transaction::<Secret, superposition::AppError, _>(|conn| {
            // Share the workspace-row lock with key rotation and reload the key from the
            // database. Holding this lock through the insert prevents a secret from being
            // written with an old cached key while a rotation is in flight.
            let workspace = workspaces::table
                .find((
                    &workspace_context.organisation_id.0,
                    &workspace_context.workspace_id.0,
                ))
                .for_share()
                .get_result::<Workspace>(conn)?;
            let encryption_key =
                get_workspace_encryption_key(&workspace, &state.master_encryption_key)?;
            let encrypted_secret_value = encrypt_secret(&req.value, &encryption_key)
                .map_err(|e| bad_argument!("Encryption failed: {}", e))?;
            let schema_name = SchemaName(workspace.workspace_schema_name);
            let now = chrono::Utc::now();

            let new_secret = Secret {
                name: req.name,
                encrypted_value: encrypted_secret_value,
                description: req.description,
                change_reason: req.change_reason.clone(),
                created_at: now,
                last_modified_at: now,
                created_by: user_email.clone(),
                last_modified_by: user_email,
            };

            diesel::insert_into(secrets::table)
                .values(&new_secret)
                .returning(Secret::as_returning())
                .schema_name(&schema_name)
                .get_result(conn)
                .map_err(Into::into)
        })?;

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
    let user_email = user.get_email();

    let updated_secret =
        conn.transaction::<Secret, superposition::AppError, _>(|conn| {
            let workspace = workspaces::table
                .find((
                    &workspace_context.organisation_id.0,
                    &workspace_context.workspace_id.0,
                ))
                .for_share()
                .get_result::<Workspace>(conn)?;
            let encrypted_value = if let Some(ref plaintext_value) = req_inner.value {
                let encryption_key = get_workspace_encryption_key(
                    &workspace,
                    &state.master_encryption_key,
                )?;
                Some(
                    encrypt_secret(plaintext_value, &encryption_key)
                        .map_err(|e| bad_argument!("Encryption failed: {}", e))?,
                )
            } else {
                None
            };
            let schema_name = SchemaName(workspace.workspace_schema_name);
            let changeset = UpdateSecretChangeset {
                encrypted_value,
                description: req_inner.description,
                change_reason: req_inner.change_reason,
            };

            diesel::update(secrets::table)
                .filter(secrets::name.eq(secret_name))
                .set((
                    changeset,
                    secrets::last_modified_at.eq(chrono::Utc::now()),
                    secrets::last_modified_by.eq(user_email),
                ))
                .schema_name(&schema_name)
                .get_result::<Secret>(conn)
                .map_err(Into::into)
        })?;

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
#[authorized(resource = MasterEncryptionKey)]
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

    let user_email = user.get_email();

    let (rotated_workspaces, total_secrets_re_encrypted) = conn
        .transaction::<(Vec<Workspace>, i64), superposition::AppError, _>(|conn| {
            let workspace_targets: Vec<(String, String)> = workspaces::table
                .select((workspaces::organisation_id, workspaces::workspace_name))
                .order((
                    workspaces::organisation_id.asc(),
                    workspaces::workspace_name.asc(),
                ))
                .load(conn)?;

            let mut rotated_workspaces = Vec::with_capacity(workspace_targets.len());
            let mut total_secrets_re_encrypted = 0i64;

            for (organisation_id, workspace_name) in workspace_targets {
                let (secrets_count, workspace) = rotate_workspace_encryption_key_helper(
                    &organisation_id,
                    &workspace_name,
                    conn,
                    master_encryption_key,
                    &user_email,
                )?;
                total_secrets_re_encrypted += secrets_count;
                rotated_workspaces.push(workspace);
            }

            Ok((rotated_workspaces, total_secrets_re_encrypted))
        })?;

    for workspace in &rotated_workspaces {
        put_workspace_in_redis(workspace, &state.redis).await;
    }

    let workspaces_rotated = rotated_workspaces.len() as i64;

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
