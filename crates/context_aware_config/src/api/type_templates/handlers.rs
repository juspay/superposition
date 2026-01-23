use actix_web::web::{Json, Path, Query};
use actix_web::{delete, get, post, routes, Scope};
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use jsonschema::JSONSchema;
use serde_json::Value;
use service_utils::service::types::{DbConnection, WorkspaceContext};
use superposition_derives::authorized;
use superposition_macros::bad_argument;
use superposition_types::{
    api::type_templates::{
        TypeTemplateCreateRequest, TypeTemplateName, TypeTemplateUpdateRequest,
    },
    custom_query::PaginationParams,
    database::{
        models::cac::TypeTemplate,
        schema::type_templates::{self, dsl},
    },
    result as superposition, PaginatedResponse, User,
};

use crate::helpers::validate_change_reason;

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
    request: Json<TypeTemplateCreateRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<TypeTemplate>> {
    let DbConnection(mut conn) = db_conn;
    JSONSchema::compile(&Value::from(&request.type_schema)).map_err(|err| {
        log::error!(
            "Invalid jsonschema sent in the request, schema: {:?} error: {}",
            request.type_schema,
            err
        );
        bad_argument!(
            "Invalid jsonschema sent in the request, validation error is: {}",
            err.to_string()
        )
    })?;

    validate_change_reason(&workspace_context, &request.change_reason, &mut conn)?;

    let now = Utc::now();
    let type_template = TypeTemplate {
        type_schema: request.type_schema.clone(),
        type_name: request.type_name.clone().into(),
        created_at: now,
        created_by: user.email.clone(),
        last_modified_at: now,
        last_modified_by: user.email.clone(),
        description: request.description.clone(),
        change_reason: request.change_reason.clone(),
    };

    let type_template = diesel::insert_into(type_templates::table)
        .values(&type_template)
        .returning(TypeTemplate::as_returning())
        .schema_name(&workspace_context.schema_name)
        .get_result::<TypeTemplate>(&mut conn)?;
    Ok(Json(type_template))
}

#[authorized]
#[get("/{type_name}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    type_name: Path<TypeTemplateName>,
    db_conn: DbConnection,
) -> superposition::Result<Json<TypeTemplate>> {
    let DbConnection(mut conn) = db_conn;
    let type_name: String = type_name.into_inner().into();
    let type_template = type_templates::table
        .filter(type_templates::type_name.eq(type_name))
        .schema_name(&workspace_context.schema_name)
        .first::<TypeTemplate>(&mut conn)?;

    Ok(Json(type_template))
}

#[authorized]
#[routes]
#[put("/{type_name}")]
#[patch("/{type_name}")]
async fn update_handler(
    workspace_context: WorkspaceContext,
    request: Json<TypeTemplateUpdateRequest>,
    path: Path<TypeTemplateName>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<TypeTemplate>> {
    let DbConnection(mut conn) = db_conn;
    let request = request.into_inner();
    JSONSchema::compile(&Value::from(&request.type_schema)).map_err(|err| {
        log::error!(
            "Invalid jsonschema sent in the request, schema: {:?} error: {}",
            request,
            err
        );
        bad_argument!(
            "Invalid jsonschema sent in the request, validation error is: {}",
            err.to_string()
        )
    })?;

    validate_change_reason(&workspace_context, &request.change_reason, &mut conn)?;

    let type_name: String = path.into_inner().into();

    let timestamp = Utc::now();
    let updated_type = diesel::update(type_templates::table)
        .filter(type_templates::type_name.eq(type_name))
        .set((
            request,
            type_templates::last_modified_at.eq(timestamp),
            type_templates::last_modified_by.eq(user.email.clone()),
        ))
        .returning(TypeTemplate::as_returning())
        .schema_name(&workspace_context.schema_name)
        .get_result::<TypeTemplate>(&mut conn)?;
    Ok(Json(updated_type))
}

#[authorized]
#[delete("/{type_name}")]
async fn delete_handler(
    workspace_context: WorkspaceContext,
    path: Path<TypeTemplateName>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<TypeTemplate>> {
    let DbConnection(mut conn) = db_conn;
    let type_name: String = path.into_inner().into();
    diesel::update(dsl::type_templates)
        .filter(dsl::type_name.eq(type_name.clone()))
        .set((
            dsl::last_modified_at.eq(Utc::now()),
            dsl::last_modified_by.eq(user.email.clone()),
        ))
        .returning(TypeTemplate::as_returning())
        .schema_name(&workspace_context.schema_name)
        .execute(&mut conn)?;
    let deleted_type =
        diesel::delete(dsl::type_templates.filter(dsl::type_name.eq(type_name)))
            .schema_name(&workspace_context.schema_name)
            .get_result::<TypeTemplate>(&mut conn)?;
    Ok(Json(deleted_type))
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<TypeTemplate>>> {
    let DbConnection(mut conn) = db_conn;

    if let Some(true) = filters.all {
        let result: Vec<TypeTemplate> = type_templates::dsl::type_templates
            .schema_name(&workspace_context.schema_name)
            .get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(result)));
    };

    let n_types: i64 = type_templates::dsl::type_templates
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result(&mut conn)?;
    let limit = filters.count.unwrap_or(10);
    let mut builder = type_templates::dsl::type_templates
        .schema_name(&workspace_context.schema_name)
        .order(type_templates::dsl::created_at.desc())
        .limit(limit)
        .into_boxed();
    if let Some(page) = filters.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let custom_types: Vec<TypeTemplate> = builder.load(&mut conn)?;
    let total_pages = (n_types as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_types,
        data: custom_types,
    }))
}
