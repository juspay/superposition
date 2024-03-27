extern crate base64;
use super::types::CreateReq;
use base64::prelude::*;
use std::str;

use crate::validation_functions::execute_fn;
use crate::{
    api::functions::helpers::get_published_function_code,
    db::{self, models::DefaultConfig, schema::default_configs::dsl::default_configs},
    helpers::validate_jsonschema,
};
use actix_web::{
    error::{ErrorBadRequest, ErrorInternalServerError},
    get, put,
    web::{self, Data, Json},
    HttpResponse, Scope,
};
use chrono::Utc;
use dashboard_auth::types::User;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{json, Value};
use service_utils::{
    service::types::{AppState, DbConnection},
    types as app,
};

pub fn endpoints() -> Scope {
    Scope::new("").service(create).service(get)
}

#[put("/{key}")]
async fn create(
    state: Data<AppState>,
    key: web::Path<String>,
    request: web::Json<CreateReq>,
    user: User,
    db_conn: DbConnection,
) -> actix_web::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let key = key.into_inner();

    if req.value.is_none() && req.schema.is_none() && req.function_name.is_none() {
        log::error!("No data provided in the request body for {key}");
        return Err(ErrorBadRequest(json!({
            "message": "Please provide data in the request body."
        })));
    }

    let func_name = match &req.function_name {
        Some(Value::String(s)) => Some(s.clone()),
        Some(Value::Null) | None => None,
        Some(_) => {
            return Err(ErrorBadRequest(json!({
                "message": "Expected a string or null as the function name."
            })))
        }
    };

    let result = fetch_default_key(&key, &mut conn);

    let (value, schema, function_name) = match result {
        Ok((val, schema, f_name)) => {
            let val = req.value.unwrap_or_else(|| val);
            let schema = req.schema.map_or_else(|| schema, Value::Object);
            let f_name = if req.function_name == Some(Value::Null) {
                None
            } else {
                func_name.or(f_name)
            };
            (val, schema, f_name)
        }
        Err(diesel::NotFound) => match (req.value, req.schema) {
            (Some(val), Some(schema)) => (val, Value::Object(schema), func_name),
            _ => {
                log::error!("No record found for {key}.");
                return Err(ErrorBadRequest(json!({
                    "message": format!( "No record found for {key}." )
                })));
            }
        },
        Err(e) => {
            log::error!("Failed to fetch default_config {key} with error: {e}.");
            return Err(ErrorBadRequest(json!({
                "message": format!( "Something went wrong." )
            })));
        }
    };

    let default_config = DefaultConfig {
        key: key.to_owned(),
        value,
        schema,
        function_name,
        created_by: user.email,
        created_at: Utc::now(),
    };

    if let Err(e) = validate_jsonschema(
        &state.default_config_validation_schema,
        &default_config.schema,
    ) {
        return Err(ErrorBadRequest(json!({ "message": e })));
    };
    let schema_compile_result = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&default_config.schema);
    let jschema = match schema_compile_result {
        Ok(jschema) => jschema,
        Err(e) => {
            log::info!("Failed to compile as a Draft-7 JSON schema: {e}");
            return Err(ErrorBadRequest(json!({"message": "Bad json schema."})));
        }
    };

    if let Err(e) = jschema.validate(&default_config.value) {
        let verrors = e.collect::<Vec<ValidationError>>();
        log::info!(
            "Validation for value with given JSON schema failed: {:?}",
            verrors
        );
        return Err(ErrorBadRequest(
            json!({"message": "Validation with given schema failed."}),
        ));
    }

    if let Some(f_name) = &default_config.function_name {
        let function_code = get_published_function_code(&mut conn, f_name.to_string())
            .map_err(|e| {
                log::info!("Function not found with error : {e}");
                ErrorBadRequest(json!({"message" : "Function not found."}))
            })?;
        if let Some(f_code) = function_code {
            let base64_decoded = BASE64_STANDARD.decode(f_code.clone()).map_err(|e| {
                ErrorInternalServerError(json!({"message": e.to_string()}))
            })?;
            let utf8_decoded = str::from_utf8(&base64_decoded).map_err(|e| {
                ErrorInternalServerError(json!({"message": e.to_string()}))
            })?;

            if let Err((e, stdout)) = execute_fn(
                &utf8_decoded,
                &f_name,
                &key,
                default_config.value.to_owned(),
            ) {
                log::info!("function validation failed for {key} with error: {e}");
                return Err(ErrorBadRequest(json!({
                    "message": format!( "function validation failed with error: {}", e), "stdout": stdout
                })));
            }
        }
    }

    let upsert = diesel::insert_into(default_configs)
        .values(&default_config)
        .on_conflict(db::schema::default_configs::key)
        .do_update()
        .set(&default_config)
        .execute(&mut conn);

    match upsert {
        Ok(_) => Ok(HttpResponse::Ok().json(json!({
            "message": "DefaultConfig created/updated successfully."
        }))),
        Err(e) => {
            log::info!("DefaultConfig creation failed with error: {e}");
            Err(ErrorInternalServerError(
                json!({"message": "Failed to create DefaultConfig"}),
            ))
        }
    }
}

fn fetch_default_key(
    key: &String,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> Result<(Value, Value, Option<String>), diesel::result::Error> {
    let res: (Value, Value, Option<String>) = default_configs
        .filter(db::schema::default_configs::key.eq(key))
        .select((
            db::schema::default_configs::value,
            db::schema::default_configs::schema,
            db::schema::default_configs::function_name,
        ))
        .get_result::<(Value, Value, Option<String>)>(conn)?;
    Ok(res)
}

#[get("")]
async fn get(db_conn: DbConnection) -> app::Result<Json<Vec<DefaultConfig>>> {
    let DbConnection(mut conn) = db_conn;

    let result: Vec<DefaultConfig> = default_configs.get_results(&mut conn)?;
    Ok(Json(result))
}
