use actix_web::web::{Json, Path, Query};
use actix_web::{delete, get, post, routes, Scope};
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use handlebars::Handlebars;
use mime::Mime;
use serde_json::Value;
use service_utils::service::types::{DbConnection, WorkspaceContext};
use superposition_derives::authorized;
use superposition_macros::bad_argument;
use superposition_types::{
    api::response_templates::{
        ResponseTemplateCreateRequest, ResponseTemplateName,
        ResponseTemplateUpdateRequest,
    },
    custom_query::PaginationParams,
    database::{
        models::cac::ResponseTemplate,
        schema::response_templates::{self, dsl},
    },
    result as superposition, PaginatedResponse, User,
};

use crate::{api::context::hash, helpers::validate_change_reason};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(get_handler)
        .service(list_handler)
        .service(create_handler)
        .service(update_handler)
        .service(delete_handler)
}

#[authorized]
#[post("")]
async fn create_handler(
    workspace_context: WorkspaceContext,
    request: Json<ResponseTemplateCreateRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<ResponseTemplate>> {
    let DbConnection(mut conn) = db_conn;

    let request = request.into_inner();

    request.content_type.parse::<Mime>().map_err(|e| {
        log::error!(
            "Invalid content_type: {}, error: {}",
            request.content_type,
            e
        );
        bad_argument!("Invalid content_type: {}", e)
    })?;

    let mut handlebars = Handlebars::new();
    handlebars
        .register_template_string("template", &request.template)
        .map_err(|e| {
            log::error!(
                "Invalid template syntax: {}, error: {}",
                request.template,
                e
            );
            bad_argument!("Invalid template syntax: {}", e)
        })?;

    validate_change_reason(&workspace_context, &request.change_reason, &mut conn)?;
    let context_id = hash(&Value::Object((*request.context).clone()));
    let now = Utc::now();
    let response_template = ResponseTemplate {
        name: (*request.name).clone(),
        description: request.description,
        change_reason: request.change_reason,
        context_id,
        context: request.context,
        content_type: request.content_type,
        template: request.template,
        created_at: now,
        last_modified_at: now,
        created_by: user.email.clone(),
        last_modified_by: user.email,
    };

    let response_template = diesel::insert_into(response_templates::table)
        .values(&response_template)
        .returning(ResponseTemplate::as_returning())
        .schema_name(&workspace_context.schema_name)
        .get_result::<ResponseTemplate>(&mut conn)?;
    Ok(Json(response_template))
}

#[authorized]
#[get("/{name}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    name: Path<ResponseTemplateName>,
    db_conn: DbConnection,
) -> superposition::Result<Json<ResponseTemplate>> {
    let DbConnection(mut conn) = db_conn;
    let name: String = name.into_inner().into();
    let response_template = response_templates::table
        .filter(response_templates::name.eq(name))
        .schema_name(&workspace_context.schema_name)
        .first::<ResponseTemplate>(&mut conn)?;

    Ok(Json(response_template))
}

#[authorized]
#[routes]
#[put("/{name}")]
#[patch("/{name}")]
async fn update_handler(
    workspace_context: WorkspaceContext,
    request: Json<ResponseTemplateUpdateRequest>,
    path: Path<ResponseTemplateName>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<ResponseTemplate>> {
    let DbConnection(mut conn) = db_conn;
    let request = request.into_inner();

    if let Some(ref content_type) = request.content_type {
        content_type.parse::<Mime>().map_err(|e| {
            log::error!("Invalid content_type: {}, error: {}", content_type, e);
            bad_argument!("Invalid content_type: {}", e)
        })?;
    }

    if let Some(ref template) = request.template {
        let handlebars = Handlebars::new();
        handlebars
            .render_template(template, &serde_json::json!({}))
            .map_err(|e| {
                log::error!("Invalid template syntax: {}, error: {}", template, e);
                bad_argument!("Invalid template syntax: {}", e)
            })?;
    }

    validate_change_reason(&workspace_context, &request.change_reason, &mut conn)?;

    let name: String = path.into_inner().into();

    let timestamp = Utc::now();
    let updated_template = diesel::update(response_templates::table)
        .filter(response_templates::name.eq(name))
        .set((
            request,
            response_templates::last_modified_at.eq(timestamp),
            response_templates::last_modified_by.eq(user.email),
        ))
        .returning(ResponseTemplate::as_returning())
        .schema_name(&workspace_context.schema_name)
        .get_result::<ResponseTemplate>(&mut conn)?;
    Ok(Json(updated_template))
}

#[authorized]
#[delete("/{name}")]
async fn delete_handler(
    workspace_context: WorkspaceContext,
    path: Path<ResponseTemplateName>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<ResponseTemplate>> {
    let DbConnection(mut conn) = db_conn;
    let name: String = path.into_inner().into();
    diesel::update(dsl::response_templates)
        .filter(dsl::name.eq(name.clone()))
        .set((
            dsl::last_modified_at.eq(Utc::now()),
            dsl::last_modified_by.eq(user.email.clone()),
        ))
        .schema_name(&workspace_context.schema_name)
        .execute(&mut conn)?;
    let deleted_template =
        diesel::delete(dsl::response_templates.filter(dsl::name.eq(name)))
            .schema_name(&workspace_context.schema_name)
            .get_result::<ResponseTemplate>(&mut conn)?;
    Ok(Json(deleted_template))
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<ResponseTemplate>>> {
    let DbConnection(mut conn) = db_conn;

    if let Some(true) = filters.all {
        let result: Vec<ResponseTemplate> = response_templates::dsl::response_templates
            .schema_name(&workspace_context.schema_name)
            .get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(result)));
    };

    let n_templates: i64 = response_templates::dsl::response_templates
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result(&mut conn)?;
    let limit = filters.count.unwrap_or(10);
    let mut builder = response_templates::dsl::response_templates
        .schema_name(&workspace_context.schema_name)
        .order(response_templates::dsl::created_at.desc())
        .limit(limit)
        .into_boxed();
    if let Some(page) = filters.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let templates: Vec<ResponseTemplate> = builder.load(&mut conn)?;
    let total_pages = (n_templates as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_templates,
        data: templates,
    }))
}
