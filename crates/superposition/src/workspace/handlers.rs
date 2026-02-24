use std::fs;

use actix_web::{
    Scope, get, post, routes,
    web::{self, Data, Json, Path, Query},
};
use chrono::Utc;
use diesel::{
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl, TextExpressionMethods,
    connection::SimpleConnection,
    r2d2::{ConnectionManager, PooledConnection},
};
use regex::Regex;
use service_utils::{
    db::run_transaction,
    encryption::{
        encrypt_workspace_key, generate_encryption_key,
        rotate_workspace_encryption_key_helper,
    },
    helpers::get_workspace,
    run_query,
    service::types::{
        AppState, OrganisationId, SchemaName, WorkspaceContext, WorkspaceId,
    },
};
use superposition_derives::authorized;
use superposition_macros::{bad_argument, db_error, unexpected_error, validation_error};
use superposition_types::{
    DBConnection, PaginatedResponse, User,
    api::{
        I64Update,
        workspace::{
            CreateWorkspaceRequest, KeyRotationResponse, UpdateWorkspaceRequest,
            WorkspaceListFilters, WorkspaceResponse,
        },
    },
    custom_query::PaginationParams,
    database::{
        models::{Organisation, Workspace, WorkspaceStatus},
        schema::config_versions::dsl as config_versions,
        superposition_schema::superposition::{organisations, workspaces},
    },
    result as superposition,
};

const WORKSPACE_TEMPLATE_PATH: &str = "workspace_template.sql";

fn setup_workspace_schema(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    workspace_schema_name: &str,
) -> superposition::Result<()> {
    let workspace_template =
        fs::read_to_string(WORKSPACE_TEMPLATE_PATH).map_err(|err| {
            log::error!("Could not load the workspace template due to {}", err);
            unexpected_error!(
                "Could not load the workspace template, please contact an admin"
            )
        })?;
    let workspace_template =
        workspace_template.replace("{replaceme}", workspace_schema_name);
    conn.batch_execute(&workspace_template).map_err(|err| {
        log::error!(
            "Could not create workspace {} due to {}",
            workspace_schema_name,
            err
        );
        db_error!(err)
    })?;
    Ok(())
}

pub fn endpoints(scope: Scope) -> Scope {
    scope
        .service(create_handler)
        .service(update_handler)
        .service(list_handler)
        .service(get_handler)
        .service(migrate_schema_handler)
        .service(rotate_encryption_key_handler)
}

#[authorized]
#[get("/{workspace_name}")]
async fn get_handler(
    workspace_name: Path<String>,
    state: Data<AppState>,
    org_id: OrganisationId,
) -> superposition::Result<Json<WorkspaceResponse>> {
    let workspace_name = workspace_name.into_inner();
    let workspace: Workspace = run_query!(
        state.db_pool,
        conn,
        workspaces::dsl::workspaces
            .filter(workspaces::organisation_id.eq(&org_id.0))
            .filter(workspaces::workspace_name.eq(workspace_name))
            .get_result(&mut conn)
    )?;
    let response = WorkspaceResponse::from(workspace);
    Ok(Json(response))
}

#[authorized]
#[post("")]
async fn create_handler(
    request: Json<CreateWorkspaceRequest>,
    state: Data<AppState>,
    org_id: OrganisationId,
    user: User,
) -> superposition::Result<Json<WorkspaceResponse>> {
    let org_info = run_query!(
        state.db_pool,
        conn,
        organisations::dsl::organisations
            .filter(organisations::id.eq(&org_id.0))
            .get_result::<Organisation>(&mut conn)
    )?;
    let timestamp = Utc::now();
    let request = request.into_inner();
    let email = user.get_email();
    validate_workspace_name(&request.workspace_name)?;
    let workspace_schema_name = format!("{}_{}", &org_info.id, &request.workspace_name);

    let encryption_key = match state.master_encryption_key {
        Some(ref master_encryption_key) => {
            let encryption_key = generate_encryption_key();
            encrypt_workspace_key(&encryption_key, &master_encryption_key.current_key)
                .map_err(|e| {
                    log::error!("Failed to encrypt workspace key: {}", e);
                    unexpected_error!("Failed to encrypt workspace key")
                })?
        }
        None => {
            log::warn!(
                "Master encryption key not configured, workspace will be created without encryption"
            );
            String::new()
        }
    };

    let workspace = Workspace {
        organisation_id: org_info.id,
        organisation_name: org_info.name,
        workspace_name: request.workspace_name,
        workspace_schema_name: workspace_schema_name.clone(),
        workspace_status: WorkspaceStatus::ENABLED,
        workspace_admin_email: request.workspace_admin_email,
        config_version: None,
        created_by: email.clone(),
        last_modified_by: email,
        last_modified_at: timestamp,
        created_at: timestamp,
        mandatory_dimensions: None,
        metrics: request.metrics.unwrap_or_default(),
        allow_experiment_self_approval: request.allow_experiment_self_approval,
        auto_populate_control: request.auto_populate_control,
        enable_context_validation: request.enable_context_validation,
        enable_change_reason_validation: request.enable_change_reason_validation,
        encryption_key,
        key_rotated_at: None,
    };

    let created_workspace =
        run_transaction(&state.db_pool, |conn: &mut DBConnection| {
            let inserted_workspace = diesel::insert_into(workspaces::table)
                .values(workspace)
                .get_result(conn)?;

            setup_workspace_schema(conn, &workspace_schema_name)?;
            Ok::<Workspace, superposition::AppError>(inserted_workspace)
        })?;
    let response = WorkspaceResponse::from(created_workspace);
    Ok(Json(response))
}

#[authorized]
#[routes]
#[put("/{workspace_name}")]
#[patch("/{workspace_name}")]
async fn update_handler(
    workspace_name: web::Path<String>,
    request: Json<UpdateWorkspaceRequest>,
    state: Data<AppState>,
    org_id: OrganisationId,
    user: User,
) -> superposition::Result<Json<WorkspaceResponse>> {
    let request = request.into_inner();
    let workspace_name = workspace_name.into_inner();
    let timestamp = Utc::now();
    let schema_name = SchemaName(format!("{}_{}", *org_id, workspace_name));
    // TODO: mandatory dimensions updation needs to be validated
    // for the existance of the dimensions in the workspace

    let updated_workspace =
        run_transaction(&state.db_pool, |conn: &mut DBConnection| {
            if let Some(I64Update::Add(version)) = request.config_version {
                config_versions::config_versions
                    .select(config_versions::id)
                    .filter(config_versions::id.eq(version))
                    .schema_name(&schema_name)
                    .first::<i64>(conn)?;
            }

            let updated_workspace = diesel::update(workspaces::table)
                .filter(workspaces::organisation_id.eq(&org_id.0))
                .filter(workspaces::workspace_name.eq(workspace_name))
                .set((
                    request,
                    workspaces::last_modified_by.eq(user.email),
                    workspaces::last_modified_at.eq(timestamp),
                ))
                .get_result::<Workspace>(conn)
                .map_err(|err| {
                    log::error!("failed to update workspace with error: {}", err);
                    err
                })?;

            Ok::<Workspace, superposition::AppError>(updated_workspace)
        })?;
    let response = WorkspaceResponse::from(updated_workspace);
    Ok(Json(response))
}

#[authorized]
#[get("")]
async fn list_handler(
    state: Data<AppState>,
    pagination_filters: Query<PaginationParams>,
    filters: Query<WorkspaceListFilters>,
    org_id: OrganisationId,
) -> superposition::Result<Json<PaginatedResponse<WorkspaceResponse>>> {
    if let Some(true) = pagination_filters.all {
        let result = run_query!(
            state.db_pool,
            conn,
            workspaces::dsl::workspaces
                .filter(workspaces::organisation_id.eq(&org_id.0))
                .get_results::<Workspace>(&mut conn)
        )?
        .into_iter()
        .map(WorkspaceResponse::from)
        .collect::<Vec<_>>();
        return Ok(Json(PaginatedResponse::all(result)));
    };

    let filters = filters.into_inner();
    let query_builder = |filters: &WorkspaceListFilters| {
        let mut builder = workspaces::dsl::workspaces
            .filter(workspaces::organisation_id.eq(&org_id.0))
            .into_boxed();
        if let Some(ref workspace_name) = filters.workspace_name {
            builder = builder.filter(
                workspaces::dsl::workspace_name.like(format!("%{}%", workspace_name)),
            );
        }
        builder
    };

    let count_query = query_builder(&filters);
    let base_query = query_builder(&filters);

    let n_types = run_query!(
        state.db_pool,
        conn,
        count_query.count().get_result(&mut conn)
    )?;
    let limit = pagination_filters.count.unwrap_or(10);
    let mut builder = base_query
        .order(workspaces::dsl::created_at.desc())
        .limit(limit);
    if let Some(page) = pagination_filters.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let workspaces =
        run_query!(state.db_pool, conn, builder.load::<Workspace>(&mut conn))?
            .into_iter()
            .map(WorkspaceResponse::from)
            .collect::<Vec<_>>();
    let total_pages = (n_types as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_types,
        data: workspaces,
    }))
}

const WORKSPACE_NAME_REGEX: &str = "^[a-zA-Z0-9]+$";

fn validate_workspace_name(workspace_name: &String) -> superposition::Result<()> {
    let regex = Regex::new(WORKSPACE_NAME_REGEX).map_err(|err| {
        log::error!("Could not process the regex for validating workspace names {err}");
        unexpected_error!("Could not process the regex for validating workspace names")
    })?;
    match workspace_name {
        w_name if w_name.len() > 25 => {
            log::error!(
                "the workspace name {} was larger than 25 bytes/characters, the actual length was {}",
                w_name,
                w_name.len()
            );
            Err(validation_error!(
                "the workspace name cannot be larger than 25 characters"
            ))
        }
        w_name if !regex.is_match(w_name) => {
            log::error!(
                "the workspace name {} did not match the regex {}",
                w_name,
                WORKSPACE_NAME_REGEX
            );
            Err(validation_error!(
                "the workspace name can only contain letters and numbers"
            ))
        }
        _ => Ok(()),
    }
}

#[authorized]
#[post("/{workspace_name}/db/migrate")]
async fn migrate_schema_handler(
    workspace_name: Path<String>,
    org_id: OrganisationId,
    state: Data<AppState>,
    user: User,
) -> superposition::Result<Json<WorkspaceResponse>> {
    let workspace_name = workspace_name.into_inner();
    let schema_name = SchemaName(format!("{}_{}", *org_id, &workspace_name));
    let workspace = get_workspace(&schema_name, &state.db_pool)?;

    run_transaction(&state.db_pool, |conn: &mut DBConnection| {
        setup_workspace_schema(conn, &workspace.workspace_schema_name)?;
        if workspace.encryption_key.is_empty() {
            match state.master_encryption_key {
                Some(ref master_encryption_key) => {
                    let new_key = generate_encryption_key();
                    let encrypted_key = encrypt_workspace_key(
                        &new_key,
                        &master_encryption_key.current_key,
                    )
                    .map_err(|e| {
                        log::error!("Failed to encrypt workspace key: {}", e);
                        unexpected_error!("Failed to encrypt workspace key")
                    })?;

                    diesel::update(workspaces::table)
                        .filter(workspaces::organisation_id.eq(&org_id.0))
                        .filter(workspaces::workspace_name.eq(&workspace_name))
                        .set((
                            workspaces::encryption_key.eq(encrypted_key),
                            workspaces::last_modified_by.eq(user.get_username()),
                            workspaces::last_modified_at.eq(Utc::now()),
                        ))
                        .execute(conn)?;
                }
                None => {
                    log::warn!(
                        "Master encryption key not configured, skipping encryption setup for workspace '{}'. \
                        Secrets will not be available for this workspace.",
                        workspace_name
                    );
                }
            }
        }
        Ok::<(), superposition::AppError>(())
    })?;

    let response = WorkspaceResponse::from(workspace);
    Ok(Json(response))
}

#[authorized]
#[post("/{workspace_name}/rotate-encryption-key")]
pub async fn rotate_encryption_key_handler(
    workspace_name: Path<String>,
    user: User,
    org_id: OrganisationId,
    state: Data<AppState>,
) -> superposition::Result<Json<KeyRotationResponse>> {
    let Some(ref master_encryption_key) = state.master_encryption_key else {
        log::error!("Master encryption key not configured");
        return Err(bad_argument!(
            "Master encryption key not configured. Configure master encryption key to rotate keys"
        ));
    };

    let schema_name = SchemaName(format!("{}_{}", *org_id, workspace_name.into_inner()));
    let workspace = get_workspace(&schema_name, &state.db_pool)?;
    let workspace_context = WorkspaceContext {
        schema_name,
        organisation_id: org_id,
        workspace_id: WorkspaceId(workspace.workspace_name.clone()),
        settings: workspace,
    };

    let total_secrets_re_encrypted =
        run_transaction(&state.db_pool, |conn: &mut DBConnection| {
            rotate_workspace_encryption_key_helper(
                &workspace_context,
                conn,
                master_encryption_key,
                &user.get_username(),
            )
        })?;

    Ok(Json(KeyRotationResponse {
        total_secrets_re_encrypted,
    }))
}
