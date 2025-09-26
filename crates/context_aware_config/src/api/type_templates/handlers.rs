use std::ops::Deref;

use actix_web::web::{Json, Path, Query};
use actix_web::{delete, get, post, put, Scope};
use chrono::Utc;
use diesel::{
    ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl, SelectableHelper,
};
use jsonschema::JSONSchema;
use serde_json::Value;
use service_utils::service::types::{DbConnection, SchemaName};
use superposition_macros::{bad_argument, db_error};
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

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(get_type)
        .service(list_types)
        .service(create_type)
        .service(update_type)
        .service(delete_type)
}

#[post("")]
async fn create_type(
    request: Json<TypeTemplateCreateRequest>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<Json<TypeTemplate>> {
    let DbConnection(mut conn) = db_conn;
    JSONSchema::compile(&Value::Object(request.type_schema.deref().clone())).map_err(
        |err| {
            log::error!(
                "Invalid jsonschema sent in the request, schema: {:?} error: {}",
                request.type_schema,
                err
            );
            bad_argument!(
                "Invalid jsonschema sent in the request, validation error is: {}",
                err.to_string()
            )
        },
    )?;
    let type_name: String = request.type_name.clone().into();
    let type_template = diesel::insert_into(type_templates::table)
        .values((
            type_templates::type_schema.eq(request.type_schema.clone()),
            type_templates::type_name.eq(type_name),
            type_templates::created_by.eq(user.email.clone()),
            type_templates::last_modified_by.eq(user.email.clone()),
            type_templates::description.eq(request.description.clone()),
            type_templates::change_reason.eq(request.change_reason.clone()),
        ))
        .returning(TypeTemplate::as_returning())
        .schema_name(&schema_name)
        .get_result::<TypeTemplate>(&mut conn)
        .map_err(|err| {
            log::error!("failed to insert custom type with error: {}", err);
            db_error!(err)
        })?;
    Ok(Json(type_template))
}

#[get("/{type_name}")]
async fn get_type(
    type_name: Path<TypeTemplateName>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<TypeTemplate>> {
    let DbConnection(mut conn) = db_conn;
    let type_name: String = type_name.into_inner().into();
    let type_template = type_templates::table
        .filter(type_templates::type_name.eq(type_name))
        .schema_name(&schema_name)
        .first::<TypeTemplate>(&mut conn)?;

    Ok(Json(type_template))
}

#[put("/{type_name}")]
async fn update_type(
    request: Json<TypeTemplateUpdateRequest>,
    path: Path<TypeTemplateName>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<Json<TypeTemplate>> {
    let DbConnection(mut conn) = db_conn;
    let request = request.into_inner();
    JSONSchema::compile(&Value::Object(request.type_schema.deref().clone())).map_err(
        |err| {
            log::error!(
                "Invalid jsonschema sent in the request, schema: {:?} error: {}",
                request,
                err
            );
            bad_argument!(
                "Invalid jsonschema sent in the request, validation error is: {}",
                err.to_string()
            )
        },
    )?;

    let description = request.description;
    let type_name: String = path.into_inner().into();
    let final_description = if let Some(description) = description {
        description
    } else {
        let existing_template = type_templates::table
            .filter(type_templates::type_name.eq(&type_name))
            .schema_name(&schema_name)
            .first::<TypeTemplate>(&mut conn)
            .optional()
            .map_err(|err| {
                log::error!("Failed to fetch existing type template: {}", err);
                db_error!(err)
            })?;

        match existing_template {
            Some(template) => template.description.clone(), // Use existing description
            None => {
                return Err(bad_argument!(
                    "Description is required as the type template does not exist."
                ));
            }
        }
    };
    let change_reason = request.change_reason;
    let timestamp = Utc::now();
    let updated_type = diesel::update(type_templates::table)
        .filter(type_templates::type_name.eq(type_name))
        .set((
            type_templates::type_schema.eq(request.type_schema),
            type_templates::last_modified_at.eq(timestamp),
            type_templates::last_modified_by.eq(user.email.clone()),
            type_templates::description.eq(final_description),
            type_templates::change_reason.eq(change_reason),
        ))
        .returning(TypeTemplate::as_returning())
        .schema_name(&schema_name)
        .get_result::<TypeTemplate>(&mut conn)
        .map_err(|err| {
            log::error!("failed to insert custom type with error: {}", err);
            db_error!(err)
        })?;
    Ok(Json(updated_type))
}

#[delete("/{type_name}")]
async fn delete_type(
    path: Path<TypeTemplateName>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
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
        .schema_name(&schema_name)
        .execute(&mut conn)?;
    let deleted_type =
        diesel::delete(dsl::type_templates.filter(dsl::type_name.eq(type_name)))
            .schema_name(&schema_name)
            .get_result::<TypeTemplate>(&mut conn)?;
    Ok(Json(deleted_type))
}

#[get("")]
async fn list_types(
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<TypeTemplate>>> {
    let DbConnection(mut conn) = db_conn;

    if let Some(true) = filters.all {
        let result: Vec<TypeTemplate> = type_templates::dsl::type_templates
            .schema_name(&schema_name)
            .get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(result)));
    };

    let n_types: i64 = type_templates::dsl::type_templates
        .count()
        .schema_name(&schema_name)
        .get_result(&mut conn)?;
    let limit = filters.count.unwrap_or(10);
    let mut builder = type_templates::dsl::type_templates
        .schema_name(&schema_name)
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
