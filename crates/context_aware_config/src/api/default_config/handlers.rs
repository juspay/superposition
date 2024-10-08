extern crate base64;
use super::types::CreateReq;
use service_utils::{
    helpers::{parse_config_tags, validation_err_to_str},
    service::types::{AppHeader, AppState, CustomHeaders, DbConnection},
};

use superposition_macros::{
    bad_argument, db_error, not_found, unexpected_error, validation_error,
};
use superposition_types::{result as superposition, QueryFilters, User};

use crate::{
    api::{
        context::helpers::validate_value_with_function,
        default_config::types::DefaultConfigKey,
        functions::helpers::get_published_function_code,
    },
    db::{
        self,
        models::{self, Context, DefaultConfig},
        schema::{contexts::dsl::contexts, default_configs::dsl},
    },
    helpers::add_config_version,
};
use actix_web::{
    delete, get, put,
    web::{self, Data, Path, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use diesel::{Connection, SelectableHelper};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{from_value, json, Map, Value};

pub fn endpoints() -> Scope {
    Scope::new("").service(create).service(get).service(delete)
}

#[put("/{key}")]
async fn create(
    state: Data<AppState>,
    key: web::Path<DefaultConfigKey>,
    custom_headers: CustomHeaders,
    request: web::Json<CreateReq>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let key = key.into_inner().into();
    let tags = parse_config_tags(custom_headers.config_tags)?;

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

    let (value, schema, function_name, created_at_val, created_by_val) = match result {
        Ok(default_config_row) => {
            let val = req.value.unwrap_or(default_config_row.value);
            let schema = req
                .schema
                .map_or_else(|| default_config_row.schema, Value::Object);
            let f_name = if req.function_name == Some(Value::Null) {
                None
            } else {
                func_name.or(default_config_row.function_name)
            };
            (
                val,
                schema,
                f_name,
                default_config_row.created_at,
                default_config_row.created_by,
            )
        }
        Err(superposition::AppError::DbError(diesel::NotFound)) => {
            match (req.value, req.schema) {
                (Some(val), Some(schema)) => (
                    val,
                    Value::Object(schema),
                    func_name,
                    Utc::now(),
                    user.get_email(),
                ),
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
        created_by: created_by_val,
        created_at: created_at_val,
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
    };

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
            "Schema validation failed: {}",
            &validation_err_to_str(verrors)
                .first()
                .unwrap_or(&String::new())
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
    let version_id =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let upsert = diesel::insert_into(dsl::default_configs)
                .values(&default_config)
                .on_conflict(db::schema::default_configs::key)
                .do_update()
                .set(&default_config)
                .execute(transaction_conn);
            let version_id = add_config_version(&state, tags, transaction_conn)?;
            match upsert {
                Ok(_) => Ok(version_id),
                Err(e) => {
                    log::info!("DefaultConfig creation failed with error: {e}");
                    Err(unexpected_error!(
                        "Something went wrong, failed to create DefaultConfig"
                    ))
                }
            }
        })?;

    let mut http_resp = HttpResponse::Ok();

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));
    Ok(http_resp.json(default_config))
}

fn fetch_default_key(
    key: &String,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<models::DefaultConfig> {
    let res = dsl::default_configs
        .filter(db::schema::default_configs::key.eq(key))
        .select(models::DefaultConfig::as_select())
        .get_result(conn)?;
    Ok(res)
}

#[get("")]
async fn get(
    db_conn: DbConnection,
    filters: Query<QueryFilters>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let n_default_configs: i64 = dsl::default_configs.count().get_result(&mut conn)?;
    let mut builder = dsl::default_configs
        .into_boxed()
        .order(dsl::created_at.desc());
    if let Some(limit) = filters.count {
        builder = builder.limit(limit);
    }
    if let Some(page) = filters.page {
        let offset = (page - 1) * filters.count.unwrap_or(10);
        builder = builder.offset(offset);
    }
    let limit = filters.count.unwrap_or(10);
    let result: Vec<DefaultConfig> = builder.load(&mut conn)?;
    let total_pages = (n_default_configs as f64 / limit as f64).ceil() as u64;
    Ok(HttpResponse::Ok().json(json!({
    "total_pages": total_pages,
    "total_items": n_default_configs,
    "data": result
    })))
}

pub fn get_key_usage_context_ids(
    key: &str,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Vec<String>> {
    let result: Vec<Context> = contexts.load(conn).map_err(|err| {
        log::error!("failed to fetch contexts with error: {}", err);
        db_error!(err)
    })?;

    let mut context_ids = vec![];
    for context in result.iter() {
        from_value::<Map<String, Value>>(context.override_.to_owned())
            .map_err(|err| {
                log::error!("failed decode override into object: {}", err);
                unexpected_error!("failed to decode override")
            })?
            .get(key)
            .map_or((), |_| context_ids.push(context.id.to_owned()))
    }
    Ok(context_ids)
}

#[delete("/{key}")]
async fn delete(
    state: Data<AppState>,
    path: Path<DefaultConfigKey>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let key: String = path.into_inner().into();
    fetch_default_key(&key, &mut conn)?;
    let context_ids = get_key_usage_context_ids(&key, &mut conn)
        .map_err(|_| unexpected_error!("Something went wrong"))?;
    if context_ids.is_empty() {
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            diesel::update(dsl::default_configs)
                .filter(dsl::key.eq(&key))
                .set((
                    dsl::last_modified_at.eq(Utc::now().naive_utc()),
                    dsl::last_modified_by.eq(user.get_email()),
                ))
                .execute(transaction_conn)?;

            let deleted_row =
                diesel::delete(dsl::default_configs.filter(dsl::key.eq(&key)))
                    .execute(transaction_conn);
            match deleted_row {
                Ok(0) => Err(not_found!("default config key `{}` doesn't exists", key)),
                Ok(_) => {
                    let version_id = add_config_version(&state, tags, transaction_conn)?;
                    log::info!(
                        "default config key: {key} deleted by {}",
                        user.get_email()
                    );
                    Ok(HttpResponse::NoContent()
                        .insert_header((
                            AppHeader::XConfigVersion.to_string(),
                            version_id.to_string(),
                        ))
                        .finish())
                }
                Err(e) => {
                    log::error!("default config delete query failed with error: {e}");
                    Err(unexpected_error!("Something went wrong."))
                }
            }
        })
    } else {
        Err(bad_argument!(
            "Given key already in use in contexts: {}",
            context_ids.join(",")
        ))
    }
}
