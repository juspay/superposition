use actix_web::web::{Json, Path, Query};
use actix_web::{delete, get, post, put, HttpResponse, Scope};
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use jsonschema::JSONSchema;
use serde_json::Value;
use service_utils::service::types::DbConnection;
use superposition_macros::{bad_argument, db_error};
use superposition_types::cac::models::TypeTemplates;
use superposition_types::{
    cac::schema::type_templates::{self, dsl},
    custom_query::PaginationParams,
    result as superposition, PaginatedResponse, User,
};

use crate::api::type_templates::types::{TypeTemplateName, TypeTemplateRequest};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(list_types)
        .service(create_type)
        .service(update_type)
        .service(delete_type)
}

#[post("")]
async fn create_type(
    request: Json<TypeTemplateRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let _ = JSONSchema::compile(&request.type_schema).map_err(|err| {
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
    let type_name: String = request.type_name.clone().into();
    let type_template = diesel::insert_into(type_templates::table)
        .values((
            type_templates::type_schema.eq(request.type_schema.clone()),
            type_templates::type_name.eq(type_name),
            type_templates::created_by.eq(user.email.clone()),
            type_templates::last_modified_by.eq(user.email.clone()),
        ))
        .get_result::<TypeTemplates>(&mut conn)
        .map_err(|err| {
            log::error!("failed to insert custom type with error: {}", err);
            db_error!(err)
        })?;
    Ok(HttpResponse::Ok().json(type_template))
}

#[put("/{type_name}")]
async fn update_type(
    request: Json<Value>,
    path: Path<TypeTemplateName>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let _ = JSONSchema::compile(&request).map_err(|err| {
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
    let type_name: String = path.into_inner().into();

    let timestamp = Utc::now().naive_utc();
    let updated_type = diesel::update(type_templates::table)
        .filter(type_templates::type_name.eq(type_name))
        .set((
            type_templates::type_schema.eq(request.clone()),
            type_templates::last_modified_at.eq(timestamp),
            type_templates::last_modified_by.eq(user.email),
        ))
        .get_result::<TypeTemplates>(&mut conn)
        .map_err(|err| {
            log::error!("failed to insert custom type with error: {}", err);
            db_error!(err)
        })?;
    Ok(HttpResponse::Ok().json(updated_type))
}

#[delete("/{type_name}")]
async fn delete_type(
    path: Path<TypeTemplateName>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let type_name: String = path.into_inner().into();
    diesel::update(dsl::type_templates)
        .filter(dsl::type_name.eq(type_name.clone()))
        .set((
            dsl::last_modified_at.eq(Utc::now().naive_utc()),
            dsl::last_modified_by.eq(user.email),
        ))
        .execute(&mut conn)?;
    let deleted_type =
        diesel::delete(dsl::type_templates.filter(dsl::type_name.eq(type_name)))
            .get_result::<TypeTemplates>(&mut conn)?;
    Ok(HttpResponse::Ok().json(deleted_type))
}

#[get("")]
async fn list_types(
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<TypeTemplates>>> {
    let DbConnection(mut conn) = db_conn;

    if let Some(true) = filters.all {
        let result: Vec<TypeTemplates> =
            type_templates::dsl::type_templates.get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse {
            total_pages: 1,
            total_items: result.len() as i64,
            data: result,
        }));
    };

    let n_types: i64 = type_templates::dsl::type_templates
        .count()
        .get_result(&mut conn)?;
    let limit = filters.count.unwrap_or(10);
    let mut builder = type_templates::dsl::type_templates
        .into_boxed()
        .order(type_templates::dsl::created_at.desc())
        .limit(limit);
    if let Some(page) = filters.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let custom_types: Vec<TypeTemplates> = builder.load(&mut conn)?;
    let total_pages = (n_types as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_types,
        data: custom_types,
    }))
}
