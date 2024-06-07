use crate::db::models::TypeTemplates;
use crate::db::schema::type_templates::{self, dsl};
use actix_web::web::{Json, Path, Query};
use actix_web::{delete, get, post, put, HttpResponse, Scope};
use chrono::Utc;

use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};

use jsonschema::JSONSchema;
use regex::Regex;
use serde_json::{json, Value};
use service_utils::{
    bad_argument, db_error, result as superposition, service::types::DbConnection,
    unexpected_error,
};
use superposition_types::User;

use crate::api::type_templates::types::{QueryFilters, TypeTemplateRequest};

const TYPE_NAME_REGEX: &str = "^[a-zA-Z0-9-_]{1,64}$";

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
    let type_name = request.type_name.clone();
    let regex = Regex::new(TYPE_NAME_REGEX).map_err(|err| {
        unexpected_error!("could not parse regex due to: {}", err.to_string())
    })?;
    if !regex.is_match(type_name.as_str()) {
        return Err(bad_argument!(
            "The type name {} is invalid, it should obey the regex {}",
            type_name,
            TYPE_NAME_REGEX
        ));
    }
    let type_template = diesel::insert_into(type_templates::table)
        .values((
            type_templates::type_schema.eq(request.type_schema.clone()),
            type_templates::type_name.eq(type_name),
            type_templates::created_by.eq(user.email.clone()),
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
    path: Path<String>,
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
    let type_name = path.into_inner();
    let regex = Regex::new(TYPE_NAME_REGEX).map_err(|err| {
        unexpected_error!("could not parse regex due to: {}", err.to_string())
    })?;
    if !regex.is_match(type_name.as_str()) {
        return Err(bad_argument!(
            "The type name {} is invalid, it should obey the regex {}",
            type_name,
            TYPE_NAME_REGEX
        ));
    }
    let timestamp = Utc::now().naive_utc();
    let updated_type = diesel::update(type_templates::table)
        .filter(type_templates::type_name.eq(type_name))
        .set((
            type_templates::type_schema.eq(request.clone()),
            type_templates::created_by.eq(user.email),
            type_templates::last_modified.eq(timestamp),
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
    path: Path<String>,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let type_name = path.into_inner();
    let deleted_type =
        diesel::delete(dsl::type_templates.filter(dsl::type_name.eq(type_name)))
            .get_result::<TypeTemplates>(&mut conn)?;
    Ok(HttpResponse::Ok().json(deleted_type))
}

#[get("")]
async fn list_types(
    db_conn: DbConnection,
    filters: Query<QueryFilters>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let n_types: i64 = type_templates::dsl::type_templates
        .count()
        .get_result(&mut conn)?;
    let mut builder = type_templates::dsl::type_templates
        .into_boxed()
        .order(type_templates::dsl::created_at.desc());
    if let Some(limit) = filters.count {
        builder = builder.limit(limit);
    }
    if let Some(page) = filters.page {
        let offset = (page - 1) * filters.count.unwrap_or(10);
        builder = builder.offset(offset);
    }
    let limit = filters.count.unwrap_or(10);
    let custom_types: Vec<TypeTemplates> = builder.load(&mut conn)?;
    let total_pages = (n_types as f64 / limit as f64).ceil() as u64;
    Ok(HttpResponse::Ok().json(json!({
    "total_pages": total_pages,
    "total_items": n_types,
    "data": custom_types
    })))
}
