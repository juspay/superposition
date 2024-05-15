mod types;

use crate::db::models::JsonSchemaTypes;
use crate::db::schema::jsonschema_types::{self, dsl};
use actix_web::web::{Json, Path};
use actix_web::{delete, get, put, HttpResponse, Scope};
use chrono::Utc;

use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use jsonschema::JSONSchema;
use serde_json::json;
use service_utils::service::types::DbConnection;
use service_utils::{bad_argument, db_error, result as superposition};
use superposition_types::User;

use crate::api::custom_types::types::CreateTypeRequest;

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(list_types)
        .service(create_or_update_type)
        .service(delete_type)
}

#[put("")]
async fn create_or_update_type(
    request: Json<CreateTypeRequest>,
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
    let timestamp = Utc::now().naive_utc();
    diesel::insert_into(jsonschema_types::table)
        .values((
            jsonschema_types::type_schema.eq(request.type_schema.clone()),
            jsonschema_types::type_name.eq(request.type_name.clone()),
            jsonschema_types::created_by.eq(user.email.clone()),
            jsonschema_types::created_at.eq(timestamp),
        ))
        .on_conflict(jsonschema_types::type_name)
        .do_update()
        .set((
            jsonschema_types::type_schema.eq(request.type_schema.clone()),
            jsonschema_types::created_by.eq(user.email),
        ))
        .execute(&mut conn)
        .map_err(|err| {
            log::error!("failed to insert custom type with error: {}", err);
            db_error!(err)
        })?;
    Ok(HttpResponse::Ok().json(json!({"message": "custom type created successfully"})))
}

#[delete("/{type_name}")]
async fn delete_type(
    path: Path<String>,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let type_name = path.into_inner();
    diesel::delete(dsl::jsonschema_types.filter(dsl::type_name.eq(type_name)))
        .execute(&mut conn)?;
    Ok(HttpResponse::Ok().json(json!({"message": "deleted custom type"})))
}

#[get("/{page}/{count}")]
async fn list_types(
    db_conn: DbConnection,
    filters: Path<(i64, i64)>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let (page, limit) = filters.into_inner();
    if page < 1 {
        return Err(bad_argument!("Param 'page' has to be at least 1"));
    } else if limit < 1 {
        return Err(bad_argument!("Param 'count' has to be at least 1"));
    }
    let n_types: i64 = jsonschema_types::dsl::jsonschema_types
        .count()
        .get_result(&mut conn)?;
    let offset = (page - 1) * limit;
    let custom_types: Vec<JsonSchemaTypes> = jsonschema_types::dsl::jsonschema_types
        .order(jsonschema_types::dsl::created_at.desc())
        .limit(limit)
        .offset(offset)
        .load(&mut conn)?;
    let total_pages = (n_types as f64 / limit as f64).ceil() as u64;
    Ok(HttpResponse::Ok().json(json!({
        "total_pages": total_pages,
        "total_items": n_types,
        "data": custom_types
    })))
}
