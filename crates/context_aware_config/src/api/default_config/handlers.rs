extern crate base64;
use super::types::CreateReq;
use service_utils::{bad_argument, unexpected_error, validation_error};

use superposition_types::{SuperpositionUser, User};

use crate::api::context::helpers::validate_value_with_function;
use crate::{
    api::functions::helpers::get_published_function_code,
    db::{self, models::DefaultConfig, schema::default_configs::dsl::default_configs},
    helpers::validate_jsonschema,
};
use actix_web::{
    get, put,
    web::{self, Data, Json},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{json, Value};
use service_utils::{
    result as superposition,
    service::types::{AppState, DbConnection},
};

pub fn endpoints() -> Scope {
    Scope::new("").service(create).service(get)
}

#[put("/{key}")]
async fn create(
    state: Data<AppState>,
    key: web::Path<String>,
    request: web::Json<CreateReq>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let key = key.into_inner();

    if req.value.is_none() && req.schema.is_none() && req.function_name.is_none() {
        log::error!("No data provided in the request body for {key}");
        return Err(bad_argument!("Please provide data in the request body."));
    }

    let func_name = match &req.function_name {
        Some(Value::String(s)) => Some(s.clone()),
        Some(Value::Null) | None => None,
        Some(_) => {
            return Err(bad_argument!(
                "Expected a string or null as the function name.",
            ))
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
        Err(superposition::AppError::DbError(diesel::NotFound)) => {
            match (req.value, req.schema) {
                (Some(val), Some(schema)) => (val, Value::Object(schema), func_name),
                _ => {
                    log::error!("No record found for {key}.");
                    return Err(bad_argument!("No record found for {}", key));
                }
            }
        }
        Err(e) => {
            log::error!("Failed to fetch default_config {key} with error: {e}.");
            return Err(unexpected_error!("Something went wrong."));
        }
    };

    let default_config = DefaultConfig {
        key: key.to_owned(),
        value,
        schema,
        function_name,
        created_by: user.get_email(),
        created_at: Utc::now(),
    };

    validate_jsonschema(
        &state.default_config_validation_schema,
        &default_config.schema,
    )?;

    let schema_compile_result = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&default_config.schema);
    let jschema = match schema_compile_result {
        Ok(jschema) => jschema,
        Err(e) => {
            log::info!("Failed to compile as a Draft-7 JSON schema: {e}");
            return Err(bad_argument!("Invalid JSON schema (failed to compile)"));
        }
    };

    if let Err(e) = jschema.validate(&default_config.value) {
        let verrors = e.collect::<Vec<ValidationError>>();
        log::info!(
            "Validation for value with given JSON schema failed: {:?}",
            verrors
        );
        return Err(validation_error!(
            "Schema validation failed for key {} with error {:?}",
            default_config.key,
            verrors
        ));
    }

    if let Some(f_name) = &default_config.function_name {
        let function_code = get_published_function_code(&mut conn, f_name.to_string())
            .map_err(|e| {
                log::info!("Function not found with error : {e}");
                bad_argument!("Function {} doesn't exists.", f_name)
            })?;
        if let Some(f_code) = function_code {
            validate_value_with_function(
                f_name,
                &f_code,
                &default_config.key,
                &default_config.value,
            )?;
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
            Err(unexpected_error!(
                "Something went wrong, failed to create DefaultConfig"
            ))
        }
    }
}

fn fetch_default_key(
    key: &String,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<(Value, Value, Option<String>)> {
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
async fn get(db_conn: DbConnection) -> superposition::Result<Json<Vec<DefaultConfig>>> {
    let DbConnection(mut conn) = db_conn;

    let result: Vec<DefaultConfig> = default_configs.get_results(&mut conn)?;
    Ok(Json(result))
}
