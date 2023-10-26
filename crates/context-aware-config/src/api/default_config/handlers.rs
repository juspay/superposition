use super::types::CreateReq;
use crate::{
    db::{self, models::DefaultConfig, schema::default_configs::dsl::default_configs},
    helpers::validate_jsonschema,
};
use actix_web::{
    error::{ErrorBadRequest, ErrorInternalServerError},
    put,
    web::{self, Data},
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
use service_utils::service::types::{AppState, DbConnection};

pub fn endpoints() -> Scope {
    Scope::new("").service(create)
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

    let (value, schema) = match (req.value, req.schema) {
        (Some(val), Some(schema)) => (val, Value::Object(schema)),
        (Some(val), None) => {
            let (_, schema) = fetch_default_key(&key, &mut conn)
                .map_err(|e| ErrorBadRequest(json!({"message" : e.to_string()})))?;
            (val, schema)
        }
        (None, Some(schema)) => {
            let (value, _) = fetch_default_key(&key, &mut conn)
                .map_err(|e| ErrorBadRequest(json!({"message" : e.to_string()})))?;
            (value, Value::Object(schema))
        }
        (None, None) => {
            log::info!("value/schema not provided.");
            return Err(ErrorBadRequest(
                json!({"message": "Either value/schema required."}),
            ));
        }
    };

    if let Err(e) = validate_jsonschema(&state.default_config_validation_schema, &schema)
    {
        return Err(ErrorBadRequest(json!({ "message": e })));
    };
    let schema_compile_result = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&schema);
    let jschema = match schema_compile_result {
        Ok(jschema) => jschema,
        Err(e) => {
            log::info!("Failed to compile as a Draft-7 JSON schema: {e}");
            return Err(ErrorBadRequest(json!({"message": "Bad json schema."})));
        }
    };

    if let Err(e) = jschema.validate(&value) {
        let verrors = e.collect::<Vec<ValidationError>>();
        log::info!(
            "Validation for value with given JSON schema failed: {:?}",
            verrors
        );
        return Err(ErrorBadRequest(
            json!({"message": "Validation with given schema failed."}),
        ));
    }

    let new_default_config = DefaultConfig {
        key,
        value,
        schema,
        created_by: user.email,
        created_at: Utc::now(),
    };

    let upsert = diesel::insert_into(default_configs)
        .values(&new_default_config)
        .on_conflict(db::schema::default_configs::key)
        .do_update()
        .set(&new_default_config)
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
) -> anyhow::Result<(Value, Value)> {
    let res: (Value, Value) = default_configs
        .filter(db::schema::default_configs::key.eq(key))
        .select((
            db::schema::default_configs::value,
            db::schema::default_configs::schema,
        ))
        .get_result::<(Value, Value)>(conn)?;
    Ok(res)
}
