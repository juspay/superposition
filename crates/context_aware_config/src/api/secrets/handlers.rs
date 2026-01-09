use actix_web::{
    delete, get, patch, post,
    web::{self, Data, Json, Query},
    Scope,
};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};

use service_utils::{
    encryption::{decrypt_workspace_key, encrypt_secret},
    service::types::{AppState, DbConnection, SchemaName},
};

use superposition_derives::authorized;
use superposition_macros::unexpected_error;
use superposition_types::{
    api::secrets::{MASKED_VALUE, *},
    custom_query::PaginationParams,
    database::{
        models::{others::Secret, Workspace},
        schema::secrets::dsl::*,
    },
    result as superposition, PaginatedResponse, SortBy, User,
};

pub fn endpoints() -> Scope {
    web::scope("")
        .service(list_handler)
        .service(create_handler)
        .service(get_handler)
        .service(update_handler)
        .service(delete_handler)
        .service(super::key_rotation::rotate_encryption_key_handler)
        .service(super::key_rotation::rotate_master_key_handler)
}

fn get_workspace_keys(
    conn: &mut diesel::PgConnection,
    workspace_id: &str,
    org_id: &str,
    app_state: &AppState,
) -> superposition::Result<(String, Option<String>)> {
    use superposition_types::database::superposition_schema::superposition::workspaces::dsl as ws;

    let workspace: Workspace = ws::workspaces
        .filter(ws::workspace_name.eq(workspace_id))
        .filter(ws::organisation_id.eq(org_id))
        .first(conn)?;

    let encrypted_key = workspace.encryption_key.ok_or_else(|| {
        superposition::AppError::BadArgument(
            "Workspace encryption key not found. Please contact administrator."
                .to_string(),
        )
    })?;

    let master_key = &app_state.master_key;

    let decrypted_key =
        decrypt_workspace_key(&encrypted_key, master_key).map_err(|e| {
            log::error!("Failed to decrypt workspace key: {}", e);
            unexpected_error!("Failed to decrypt workspace encryption key")
        })?;

    let decrypted_previous_key = if let Some(ref encrypted_prev) =
        workspace.previous_encryption_key
    {
        Some(
            decrypt_workspace_key(encrypted_prev, master_key).map_err(|e| {
                log::error!("Failed to decrypt previous workspace key: {}", e);
                unexpected_error!("Failed to decrypt previous workspace encryption key")
            })?,
        )
    } else {
        None
    };

    Ok((decrypted_key, decrypted_previous_key))
}

#[authorized]
#[get("")]
async fn list_handler(
    db_conn: DbConnection,
    pagination: Query<PaginationParams>,
    filters: Query<SecretFilters>,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<SecretResponse>>> {
    let DbConnection(mut conn) = db_conn;

    let filters_inner = filters.into_inner();

    let query_builder = |filters: &SecretFilters| {
        let mut builder = secrets.schema_name(&schema_name).into_boxed();

        if let Some(ref secret_names) = filters.name {
            builder = builder.filter(name.eq_any(secret_names.0.clone()));
        }

        if let Some(ref creators) = filters.created_by {
            builder = builder.filter(created_by.eq_any(creators.0.clone()));
        }

        if let Some(ref last_modifiers) = filters.last_modified_by {
            builder = builder.filter(last_modified_by.eq_any(last_modifiers.0.clone()));
        }

        builder
    };

    if let Some(true) = pagination.all {
        let result: Vec<Secret> = query_builder(&filters_inner).get_results(&mut conn)?;
        let masked_results: Vec<SecretResponse> =
            result.into_iter().map(SecretResponse::from).collect();
        return Ok(Json(PaginatedResponse::all(masked_results)));
    }

    let base_query = query_builder(&filters_inner);
    let count_query = query_builder(&filters_inner);

    let n_secrets: i64 = count_query.count().get_result(&mut conn)?;
    let limit = pagination.count.unwrap_or(10);

    let sort_on = filters_inner.sort_on.unwrap_or_default();
    let sort_by_order = filters_inner.sort_by.unwrap_or_default();

    #[rustfmt::skip]
    let base_query = match (sort_on, sort_by_order) {
        (SortOn::Name,           SortBy::Asc)  => base_query.order(name.asc()),
        (SortOn::Name,           SortBy::Desc) => base_query.order(name.desc()),
        (SortOn::CreatedAt,      SortBy::Asc)  => base_query.order(created_at.asc()),
        (SortOn::CreatedAt,      SortBy::Desc) => base_query.order(created_at.desc()),
        (SortOn::LastModifiedAt, SortBy::Asc)  => base_query.order(last_modified_at.asc()),
        (SortOn::LastModifiedAt, SortBy::Desc) => base_query.order(last_modified_at.desc()),
    };

    let mut builder = base_query.limit(limit);
    if let Some(page) = pagination.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let result: Vec<Secret> = builder.load(&mut conn)?;

    let masked_results: Vec<SecretResponse> =
        result.into_iter().map(SecretResponse::from).collect();

    let total_pages = (n_secrets as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_secrets,
        data: masked_results,
    }))
}

#[authorized]
#[post("")]
async fn create_handler(
    req: web::Json<CreateSecretRequest>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
    workspace_id: service_utils::service::types::WorkspaceId,
    org_id: service_utils::service::types::OrganisationId,
    app_state: Data<AppState>,
) -> superposition::Result<Json<SecretResponse>> {
    let DbConnection(mut conn) = db_conn;

    let req = req.into_inner();

    let (encryption_key, _) =
        get_workspace_keys(&mut conn, &workspace_id.0, &org_id.0, &app_state)?;

    let encrypted_secret_value =
        encrypt_secret(&req.value, &encryption_key).map_err(|e| {
            superposition::AppError::BadArgument(format!("Encryption failed: {}", e))
        })?;

    let now = chrono::Utc::now();

    let new_secret = Secret {
        name: req.name,
        encrypted_value: encrypted_secret_value.clone(),
        key_version: 1,
        description: req.description,
        change_reason: req.change_reason.clone(),
        created_at: now,
        last_modified_at: now,
        created_by: user.get_email(),
        last_modified_by: user.get_email(),
    };

    let created_secret = diesel::insert_into(secrets)
        .values(&new_secret)
        .returning(Secret::as_returning())
        .schema_name(&schema_name)
        .get_result(&mut conn)?;

    Ok(Json(SecretResponse {
        name: created_secret.name.0,
        value: MASKED_VALUE.to_string(),
        key_version: created_secret.key_version,
        description: created_secret.description,
        change_reason: created_secret.change_reason,
        created_at: created_secret.created_at,
        last_modified_at: created_secret.last_modified_at,
        created_by: created_secret.created_by,
        last_modified_by: created_secret.last_modified_by,
    }))
}

#[authorized]
#[get("/{secret_name}")]
async fn get_handler(
    path: web::Path<String>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<SecretResponse>> {
    let DbConnection(mut conn) = db_conn;

    let secret_name = path.into_inner();

    let secret = secrets
        .filter(name.eq(secret_name))
        .schema_name(&schema_name)
        .get_result::<Secret>(&mut conn)?;

    Ok(Json(SecretResponse::from(secret)))
}

#[authorized]
#[allow(clippy::too_many_arguments)]
#[patch("/{secret_name}")]
async fn update_handler(
    path: web::Path<String>,
    req: web::Json<UpdateSecretRequest>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
    workspace_id: service_utils::service::types::WorkspaceId,
    org_id: service_utils::service::types::OrganisationId,
    app_state: Data<AppState>,
) -> superposition::Result<Json<SecretResponse>> {
    let DbConnection(mut conn) = db_conn;
    let secret_name = path.into_inner();
    let req_inner = req.into_inner();

    let (encryption_key, _) =
        get_workspace_keys(&mut conn, &workspace_id.0, &org_id.0, &app_state)?;

    let encrypted_value_opt = if let Some(ref plaintext_value) = req_inner.value {
        Some(
            encrypt_secret(plaintext_value, &encryption_key).map_err(|e| {
                superposition::AppError::BadArgument(format!("Encryption failed: {}", e))
            })?,
        )
    } else {
        None
    };

    let changeset = superposition_types::api::secrets::UpdateSecretChangeset {
        encrypted_value: encrypted_value_opt,
        description: req_inner.description,
        change_reason: req_inner.change_reason,
    };

    let updated_secret = diesel::update(secrets)
        .filter(name.eq(secret_name))
        .set((
            changeset,
            last_modified_at.eq(chrono::Utc::now()),
            last_modified_by.eq(user.get_email()),
        ))
        .schema_name(&schema_name)
        .get_result::<Secret>(&mut conn)?;

    Ok(Json(SecretResponse::from(updated_secret)))
}

#[authorized]
#[delete("/{secret_name}")]
async fn delete_handler(
    path: web::Path<String>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<SecretResponse>> {
    let DbConnection(mut conn) = db_conn;
    let secret_name = path.into_inner();

    diesel::update(secrets)
        .filter(name.eq(&secret_name))
        .set((
            last_modified_at.eq(chrono::Utc::now()),
            last_modified_by.eq(user.get_email()),
        ))
        .schema_name(&schema_name)
        .execute(&mut conn)?;

    let deleted_secret = diesel::delete(secrets)
        .filter(name.eq(&secret_name))
        .schema_name(&schema_name)
        .get_result::<Secret>(&mut conn)?;

    Ok(Json(SecretResponse::from(deleted_secret)))
}
