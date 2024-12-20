use std::fs;

use actix_web::{
    get, post, put,
    web::{self, Json, Query},
    Scope,
};
use chrono::Utc;
use diesel::{
    connection::SimpleConnection, Connection, ExpressionMethods, QueryDsl, RunQueryDsl,
    TextExpressionMethods,
};
use regex::Regex;
use service_utils::service::types::DbConnection;
use superposition_macros::{db_error, unexpected_error, validation_error};
use superposition_types::{
    custom_query::PaginationParams,
    database::{
        models::{Workspace, WorkspaceStatus},
        superposition_schema::superposition::workspaces,
    },
    result as superposition, PaginatedResponse, User,
};

use crate::workspace::types::{
    CreateWorkspaceRequest, UpdateWorkspaceRequest, WorkspaceListFilters,
};

pub fn endpoints(scope: Scope) -> Scope {
    scope
        .service(create_workspace)
        .service(update_workspace)
        .service(list_workspaces)
}

#[post("")]
async fn create_workspace(
    request: web::Json<CreateWorkspaceRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Workspace>> {
    let DbConnection(mut conn) = db_conn;
    let timestamp = Utc::now().naive_utc();
    let request = request.into_inner();
    let email = user.get_email();
    let organization_id = String::from("org");
    validate_workspace_name(&request.workspace_name)?;
    let workspace_schema_name =
        format!("{}_{}", organization_id, &request.workspace_name);
    let workspace = Workspace {
        organization_id: organization_id,
        organization_name: String::from("org"),
        workspace_name: request.workspace_name,
        workspace_schema_name: workspace_schema_name.clone(),
        workspace_status: WorkspaceStatus::ENABLED,
        workspace_admin_email: request.workspace_admin_email,
        created_by: email.clone(),
        last_modified_by: email,
        last_modified_at: timestamp.clone(),
        created_at: timestamp,
        mandatory_dimensions: None,
    };

    let created_workspace =
        conn.transaction::<Workspace, superposition::AppError, _>(|transaction_conn| {
            let mut inserted_workspace: Vec<Workspace> =
                diesel::insert_into(workspaces::dsl::workspaces)
                    .values(workspace)
                    .get_results(transaction_conn)?;

            let workspace_template = fs::read_to_string("workspace_template.sql")
                .map_err(|err| {
                    log::error!("Could not load the workspace template due to {}", err);
                    unexpected_error!(
                        "Could not load the workspace template, please contact an admin"
                    )
                })?;
            let workspace_schema_str = workspace_schema_name.as_str();
            let workspace_template =
                workspace_template.replace("{replaceme}", workspace_schema_str);
            transaction_conn
                .batch_execute(&workspace_template)
                .map_err(|err| {
                    log::error!(
                        "Could not create workspace {} due to {}",
                        workspace_schema_str,
                        err
                    );
                    db_error!(err)
                })?;
            Ok(inserted_workspace.remove(0))
        })?;
    Ok(Json(created_workspace))
}

#[put("/{workspace_name}")]
async fn update_workspace(
    workspace_name: web::Path<String>,
    request: web::Json<UpdateWorkspaceRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Workspace>> {
    let request = request.into_inner();
    let workspace_name = workspace_name.into_inner();
    let timestamp = Utc::now().naive_utc();
    let DbConnection(mut conn) = db_conn;
    let mut updated_workspace = diesel::update(workspaces::table)
        .filter(workspaces::workspace_name.eq(workspace_name.clone()))
        .set((
            workspaces::workspace_admin_email.eq(request.workspace_admin_email),
            workspaces::mandatory_dimensions.eq(request.mandatory_dimensions),
            workspaces::last_modified_by.eq(user.email),
            workspaces::last_modified_at.eq(timestamp),
        ))
        .get_result::<Workspace>(&mut conn)
        .map_err(|err| {
            log::error!("failed to update workspace with error: {}", err);
            db_error!(err)
        })?;
    if let Some(workspace_status) = request.workspace_status {
        updated_workspace = diesel::update(workspaces::table)
            .filter(workspaces::workspace_name.eq(workspace_name))
            .set((
                workspaces::workspace_status.eq(workspace_status),
                workspaces::last_modified_at.eq(Utc::now().naive_utc()),
            ))
            .get_result::<Workspace>(&mut conn)
            .map_err(|err| {
                log::error!("failed to update workspace with error: {}", err);
                db_error!(err)
            })?;
    }
    Ok(Json(updated_workspace))
}

#[get("")]
async fn list_workspaces(
    db_conn: DbConnection,
    pagination_filters: Query<PaginationParams>,
    filters: Query<WorkspaceListFilters>,
) -> superposition::Result<Json<PaginatedResponse<Workspace>>> {
    // TODO: filter by org ID
    let organization_id = String::from("org");
    let DbConnection(mut conn) = db_conn;
    if let Some(true) = pagination_filters.all {
        let result: Vec<Workspace> = workspaces::dsl::workspaces
            .filter(workspaces::organization_id.eq(&organization_id))
            .get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse {
            total_pages: 1,
            total_items: result.len() as i64,
            data: result,
        }));
    };

    let filters = filters.into_inner();
    let query_builder = |filters: &WorkspaceListFilters| {
        let mut builder = workspaces::dsl::workspaces
            .filter(workspaces::organization_id.eq(&organization_id))
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
    let workspaces: Vec<Workspace> = builder.load(&mut conn)?;
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
