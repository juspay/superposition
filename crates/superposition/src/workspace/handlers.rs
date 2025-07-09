use std::fs;

use actix_web::{
    get, post, put,
    web::{self, Json, Path, Query},
    Scope,
};
use chrono::Utc;
use diesel::{
    connection::SimpleConnection,
    r2d2::{ConnectionManager, PooledConnection},
    Connection, ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
    TextExpressionMethods,
};
use regex::Regex;
use service_utils::service::types::{DbConnection, OrganisationId, SchemaName};
use superposition_macros::{db_error, unexpected_error, validation_error};
use superposition_types::{
    api::{
        workspace::{
            CreateWorkspaceRequest, UpdateWorkspaceRequest, WorkspaceListFilters,
            WorkspaceResponse,
        },
        I64Update,
    },
    custom_query::PaginationParams,
    database::{
        models::{Organisation, Workspace, WorkspaceStatus},
        schema::config_versions::dsl as config_versions,
        superposition_schema::superposition::{organisations, workspaces},
    },
    result as superposition, PaginatedResponse, User,
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
        .service(create_workspace)
        .service(update_workspace)
        .service(list_workspaces)
        .service(get_workspace)
        .service(migrate_workspace_schema)
}

#[get("/{workspace_name}")]
async fn get_workspace(
    workspace_name: Path<String>,
    db_conn: DbConnection,
    org_id: OrganisationId,
) -> superposition::Result<Json<WorkspaceResponse>> {
    let DbConnection(mut conn) = db_conn;
    let workspace_name = workspace_name.into_inner();
    let workspace: Workspace = workspaces::dsl::workspaces
        .filter(workspaces::organisation_id.eq(&org_id.0))
        .filter(workspaces::workspace_name.eq(workspace_name))
        .get_result(&mut conn)?;
    let response = WorkspaceResponse::from(workspace);
    Ok(Json(response))
}

#[post("")]
async fn create_workspace(
    request: Json<CreateWorkspaceRequest>,
    db_conn: DbConnection,
    org_id: OrganisationId,
    user: User,
) -> superposition::Result<Json<WorkspaceResponse>> {
    let DbConnection(mut conn) = db_conn;
    let org_info: Organisation = organisations::dsl::organisations
        .filter(organisations::id.eq(&org_id.0))
        .get_result::<Organisation>(&mut conn)?;
    let timestamp = Utc::now();
    let request = request.into_inner();
    let email = user.get_email();
    validate_workspace_name(&request.workspace_name)?;
    let workspace_schema_name = format!("{}_{}", &org_info.id, &request.workspace_name);
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
        strict_mode: request.strict_mode,
        metrics: request.metrics.unwrap_or_default(),
        allow_experiment_self_approval: request.allow_experiment_self_approval,
        auto_populate_control: request.auto_populate_control,
    };

    let created_workspace =
        conn.transaction::<Workspace, superposition::AppError, _>(|transaction_conn| {
            let mut inserted_workspace: Vec<Workspace> =
                diesel::insert_into(workspaces::dsl::workspaces)
                    .values(workspace)
                    .get_results(transaction_conn)?;

            setup_workspace_schema(transaction_conn, &workspace_schema_name)?;
            Ok(inserted_workspace.remove(0))
        })?;
    let response = WorkspaceResponse::from(created_workspace);
    Ok(Json(response))
}

#[put("/{workspace_name}")]
async fn update_workspace(
    workspace_name: web::Path<String>,
    request: Json<UpdateWorkspaceRequest>,
    db_conn: DbConnection,
    org_id: OrganisationId,
    user: User,
) -> superposition::Result<Json<WorkspaceResponse>> {
    let request = request.into_inner();
    let workspace_name = workspace_name.into_inner();
    let timestamp = Utc::now();
    let schema_name = SchemaName(org_id.clone().0 + "_" + &workspace_name);
    // TODO: mandatory dimensions updation needs to be validated
    // for the existance of the dimensions in the workspace
    let DbConnection(mut conn) = db_conn;
    if let Some(I64Update::Add(version)) = request.config_version {
        let _ = config_versions::config_versions
            .select(config_versions::id)
            .filter(config_versions::id.eq(version))
            .schema_name(&schema_name)
            .first::<i64>(&mut conn)?;
    }

    let updated_workspace =
        conn.transaction::<Workspace, superposition::AppError, _>(|transaction_conn| {
            let updated_workspace = diesel::update(workspaces::table)
                .filter(workspaces::organisation_id.eq(&org_id.0))
                .filter(workspaces::workspace_name.eq(workspace_name))
                .set((
                    request,
                    workspaces::last_modified_by.eq(user.email),
                    workspaces::last_modified_at.eq(timestamp),
                ))
                .get_result::<Workspace>(transaction_conn)
                .map_err(|err| {
                    log::error!("failed to update workspace with error: {}", err);
                    err
                })?;

            Ok(updated_workspace)
        })?;
    let response = WorkspaceResponse::from(updated_workspace);
    Ok(Json(response))
}
#[get("")]
async fn list_workspaces(
    db_conn: DbConnection,
    pagination_filters: Query<PaginationParams>,
    filters: Query<WorkspaceListFilters>,
    org_id: OrganisationId,
) -> superposition::Result<Json<PaginatedResponse<WorkspaceResponse>>> {
    let DbConnection(mut conn) = db_conn;
    if let Some(true) = pagination_filters.all {
        let result: Vec<WorkspaceResponse> = workspaces::dsl::workspaces
            .filter(workspaces::organisation_id.eq(&org_id.0))
            .get_results::<Workspace>(&mut conn)?
            .into_iter()
            .map(WorkspaceResponse::from)
            .collect();
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

    let n_types: i64 = count_query.count().get_result(&mut conn)?;
    let limit = pagination_filters.count.unwrap_or(10);
    let mut builder = base_query
        .order(workspaces::dsl::created_at.desc())
        .limit(limit);
    if let Some(page) = pagination_filters.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let workspaces: Vec<WorkspaceResponse> = builder
        .load::<Workspace>(&mut conn)?
        .into_iter()
        .map(WorkspaceResponse::from)
        .collect();
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
            log::error!("the workspace name {} was larger than 25 bytes/characters, the actual length was {}", w_name, w_name.len());
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

#[post("/{workspace_name}/db/migrate")]
async fn migrate_workspace_schema(
    workspace_name: Path<String>,
    db_conn: DbConnection,
    org_id: OrganisationId,
) -> superposition::Result<Json<WorkspaceResponse>> {
    let workspace_name = workspace_name.into_inner();
    let DbConnection(mut conn) = db_conn;

    let workspace: Workspace = workspaces::dsl::workspaces
        .filter(workspaces::organisation_id.eq(&org_id.0))
        .filter(workspaces::workspace_name.eq(workspace_name))
        .get_result(&mut conn)?;

    setup_workspace_schema(&mut conn, &workspace.workspace_schema_name)?;

    let response = WorkspaceResponse::from(workspace);
    Ok(Json(response))
}
