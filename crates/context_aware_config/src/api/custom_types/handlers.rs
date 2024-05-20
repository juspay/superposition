use crate::db::models::JsonSchemaTypes;
use crate::db::schema::jsonschema_types::{self, dsl};
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

use crate::api::custom_types::types::{QueryFilters, TypeTemplateRequest};

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
    let type_template = diesel::insert_into(jsonschema_types::table)
        .values((
            jsonschema_types::type_schema.eq(request.type_schema.clone()),
            jsonschema_types::type_name.eq(request.type_name.clone()),
            jsonschema_types::created_by.eq(user.email.clone()),
        ))
        .get_result::<JsonSchemaTypes>(&mut conn)
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
    let updated_type = diesel::update(jsonschema_types::table)
        .filter(jsonschema_types::type_name.eq(type_name.clone()))
        .set((
            jsonschema_types::type_schema.eq(request.clone()),
            jsonschema_types::created_by.eq(user.email),
            jsonschema_types::last_modified.eq(timestamp),
        ))
        .get_result::<JsonSchemaTypes>(&mut conn)
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
        diesel::delete(dsl::jsonschema_types.filter(dsl::type_name.eq(type_name)))
            .get_result::<JsonSchemaTypes>(&mut conn)?;
    Ok(HttpResponse::Ok().json(deleted_type))
}

#[get("")]
async fn list_types(
    db_conn: DbConnection,
    filters: Query<QueryFilters>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let n_types: i64 = jsonschema_types::dsl::jsonschema_types
        .count()
        .get_result(&mut conn)?;
    let mut builder = jsonschema_types::dsl::jsonschema_types
        .into_boxed()
        .order(jsonschema_types::dsl::created_at.desc());
    if let Some(limit) = filters.count {
        builder = builder.limit(limit);
    }
    if let Some(page) = filters.page {
        let offset = (page - 1) * filters.count.unwrap_or(10);
        builder = builder.offset(offset);
    }
    let limit = filters.count.unwrap_or(10);
    let custom_types: Vec<JsonSchemaTypes> = builder.load(&mut conn)?;
    let total_pages = (n_types as f64 / limit as f64).ceil() as u64;
    Ok(HttpResponse::Ok().json(json!({
    "total_pages": total_pages,
    "total_items": n_types,
    "data": custom_types
    })))
}
