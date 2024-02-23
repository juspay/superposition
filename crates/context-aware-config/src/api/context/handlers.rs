use crate::helpers::{json_to_sorted_string, validate_context_jsonschema};
use crate::{
    api::{
        context::types::{
            ContextAction, ContextBulkResponse, DimensionCondition, MoveReq,
            PaginationParams, PutReq, PutResp, TransactionError,
        },
        dimension::get_all_dimension_schema_map,
    },
    db::{
        models::Context,
        schema::{
            contexts::{self, id},
            default_configs::dsl,
        },
    },
};
use actix_web::{
    delete,
    error::{self, ErrorBadRequest, ErrorInternalServerError, ErrorNotFound},
    get, put,
    web::{self, Path},
    HttpResponse, Responder, Result, Scope,
};
use anyhow::anyhow;
use chrono::Utc;
use dashboard_auth::types::User;
use diesel::{
    delete,
    r2d2::{ConnectionManager, PooledConnection},
    result::{DatabaseErrorKind::*, Error::DatabaseError},
    Connection, ExpressionMethods, PgConnection, QueryDsl, QueryResult, RunQueryDsl,
};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{from_value, json, Map, Value};
use service_utils::{helpers::ToActixErr, service::types::DbConnection};
use std::collections::HashMap;

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(put_handler)
        .service(move_handler)
        .service(delete_context)
        .service(bulk_operations)
        .service(list_contexts)
        .service(get_context)
}

type DBConnection = PooledConnection<ConnectionManager<PgConnection>>;

fn validate_dimensions_and_calculate_priority(
    object_key: &str,
    cond: &Value,
    dimension_schema_map: &HashMap<String, (JSONSchema, i32)>,
) -> Result<i32, String> {
    let get_priority = |key: &String, val: &Value| -> Result<i32, String> {
        if key == "var" {
            let dimension_name = val
                .as_str()
                .ok_or_else(|| "Dimension name should be of String type")?;
            dimension_schema_map
                .get(dimension_name)
                .map(|(_, priority)| priority)
                .ok_or(String::from(
                    "No matching `dimension` found in dimension table",
                ))
                .copied()
        } else {
            validate_dimensions_and_calculate_priority(key, val, dimension_schema_map)
        }
    };

    match cond {
        Value::Object(x) => x.iter().try_fold(0, |acc, (key, val)| {
            get_priority(key, val).map(|res| res + acc)
        }),
        Value::Array(arr) => {
            let mut val: Option<Value> = None;
            let mut condition: Option<DimensionCondition> = None;
            for i in arr {
                if let (None, Ok(x)) =
                    (&condition, from_value::<DimensionCondition>(json!(i)))
                {
                    condition = Some(x);
                } else if val == None {
                    val = Some(i.clone());
                }

                if let (Some(_dimension_value), Some(_dimension_condition)) =
                    (&val, &condition)
                {
                    break;
                }
            }

            if let (Some(dimension_value), Some(dimension_condition)) = (val, condition) {
                let expected_dimension_name = dimension_condition.var;
                let (dimension_value_schema, _) = dimension_schema_map
                    .get(&expected_dimension_name)
                    .ok_or(format!("No matching `dimension` {expected_dimension_name} in dimension table").as_str())?;

                validate_context_jsonschema(
                    object_key,
                    &dimension_value,
                    &dimension_value_schema,
                )?;
            }
            arr.iter().try_fold(0, |acc, item| {
                validate_dimensions_and_calculate_priority(
                    object_key,
                    item,
                    dimension_schema_map,
                )
                .map(|res| res + acc)
            })
        }
        _ => Ok(0),
    }
}

fn validate_override_with_default_configs(
    conn: &mut DBConnection,
    override_: &Map<String, Value>,
) -> anyhow::Result<()> {
    let keys_array: Vec<&String> = override_.keys().collect();
    let res: Vec<(String, Value)> = dsl::default_configs
        .filter(dsl::key.eq_any(keys_array))
        .select((dsl::key, dsl::schema))
        .get_results::<(String, Value)>(conn)?;

    let map = Map::from_iter(res);

    for (key, value) in override_.iter() {
        let schema = map
            .get(key)
            // .map(|resp| resp)
            .ok_or(anyhow!(format!(
                "failed to compile json schema for key {key}"
            )))?;
        let instance = value;
        let schema_compile_result = JSONSchema::options()
            .with_draft(Draft::Draft7)
            .compile(schema);
        let jschema = match schema_compile_result {
            Ok(jschema) => jschema,
            Err(e) => {
                log::info!("Failed to compile as a Draft-7 JSON schema: {e}");
                return Err(anyhow!("message: bad json schema"));
            }
        };
        if let Err(e) = jschema.validate(instance) {
            let verrors = e.collect::<Vec<ValidationError>>();
            log::error!("{:?}", verrors);
            return Err(anyhow!(json!(format!(
                "Schema validation failed for {key} with error {:?}",
                verrors
            ))));
        };
    }

    Ok(())
}

fn create_ctx_from_put_req(
    req: web::Json<PutReq>,
    conn: &mut DBConnection,
    user: &User,
) -> anyhow::Result<Context> {
    let ctx_condition = Value::Object(req.context.to_owned());
    let ctx_override: Value = req.r#override.to_owned().into();
    validate_override_with_default_configs(conn, &req.r#override)?;

    let dimension_schema_map = get_all_dimension_schema_map(conn)?;

    let priority = match validate_dimensions_and_calculate_priority(
        "context",
        &ctx_condition,
        &dimension_schema_map,
    ) {
        Ok(0) => {
            return Err(anyhow!("No dimension found in context"));
        }
        Err(e) => {
            return Err(anyhow!(e));
        }
        Ok(p) => p,
    };
    let context_id = hash(&ctx_condition);
    let override_id = hash(&ctx_override);
    Ok(Context {
        id: context_id.clone(),
        value: ctx_condition,
        priority,
        override_id: override_id.to_owned(),
        override_: ctx_override.to_owned(),
        created_at: Utc::now(),
        created_by: user.email.clone(),
    })
}

fn hash(val: &Value) -> String {
    let sorted_str: String = json_to_sorted_string(val);
    blake3::hash(sorted_str.as_bytes()).to_string()
}

fn update_override_of_existing_ctx(
    conn: &mut PgConnection,
    ctx: Context,
) -> anyhow::Result<PutResp> {
    use contexts::dsl;
    let mut new_override: Value = dsl::contexts
        .filter(dsl::id.eq(&ctx.id))
        .select(dsl::override_)
        .first(conn)?;
    cac_client::merge(&mut new_override, &ctx.override_);
    let new_override_id = hash(&new_override);
    let new_ctx = Context {
        override_: new_override,
        override_id: new_override_id,
        ..ctx
    };
    diesel::update(dsl::contexts)
        .filter(dsl::id.eq(&new_ctx.id))
        .set(&new_ctx)
        .execute(conn)?;
    Ok(get_put_resp(new_ctx))
}

fn get_put_resp(ctx: Context) -> PutResp {
    PutResp {
        context_id: ctx.id,
        override_id: ctx.override_id,
        priority: ctx.priority,
    }
}

fn put(
    req: web::Json<PutReq>,
    user: &User,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
) -> anyhow::Result<PutResp> {
    use contexts::dsl::contexts;

    let new_ctx = create_ctx_from_put_req(req, conn, user)?;

    if already_under_txn {
        diesel::sql_query("SAVEPOINT put_ctx_savepoint").execute(conn)?;
    }
    let insert = diesel::insert_into(contexts).values(&new_ctx).execute(conn);

    match insert {
        Ok(_) => Ok(get_put_resp(new_ctx)),
        Err(DatabaseError(UniqueViolation, _)) => {
            if already_under_txn {
                diesel::sql_query("ROLLBACK TO put_ctx_savepoint").execute(conn)?;
            }
            update_override_of_existing_ctx(conn, new_ctx)
        }
        Err(e) => {
            log::error!("update query failed with error: {e:?}");
            Err(anyhow!(e))
        }
    }
}

#[put("")]
async fn put_handler(
    req: web::Json<PutReq>,
    user: User,
    mut db_conn: DbConnection,
) -> actix_web::Result<web::Json<PutResp>> {
    put(req, &user, &mut db_conn, false)
        .map(|resp| web::Json(resp))
        .map_err(|e: anyhow::Error| {
            log::info!("context put failed with error: {:?}", e);
            if let Some(io_error) = e.downcast_ref::<std::io::Error>() {
                log::info!("{}", { io_error });
                ErrorInternalServerError("")
            } else if e.to_string().contains("Bad schema") {
                ErrorBadRequest("")
            } else {
                ErrorInternalServerError("")
            }
            //TODO: Check why this is not working
            // match e.downcast_ref::<std::io::Error>() {
            //     Some(err) => {
            //         if err.to_string().contains("Bad schema") {
            //             ErrorBadRequest("Schema Validation Failed")
            //         } else {
            //             ErrorInternalServerError("")
            //         }
            //     }
            //     None => ErrorInternalServerError(""),
            // }
        })
}

fn r#move(
    old_ctx_id: String,
    req: web::Json<MoveReq>,
    user: &User,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
) -> anyhow::Result<PutResp> {
    use contexts::dsl;
    let req = req.into_inner();
    let ctx_condition = Value::Object(req.context);
    let new_ctx_id = hash(&ctx_condition);
    let dimension_schema_map = get_all_dimension_schema_map(conn)?;
    let priority = match validate_dimensions_and_calculate_priority(
        "context",
        &ctx_condition,
        &dimension_schema_map,
    ) {
        Ok(0) => {
            return Err(anyhow!(String::from("No dimension found in context")));
        }
        Err(e) => {
            return Err(anyhow!(e));
        }
        Ok(p) => p,
    };

    if already_under_txn {
        diesel::sql_query("SAVEPOINT update_ctx_savepoint").execute(conn)?;
    }

    let context = diesel::update(dsl::contexts)
        .filter(dsl::id.eq(&old_ctx_id))
        .set((
            dsl::id.eq(&new_ctx_id),
            dsl::value.eq(&ctx_condition),
            dsl::priority.eq(priority),
        ))
        .get_result(conn);

    let contruct_new_ctx_with_old_overrides = |ctx: Context| Context {
        id: new_ctx_id,
        value: ctx_condition,
        priority,
        created_at: Utc::now(),
        created_by: user.email.clone(),
        override_id: ctx.override_id,
        override_: ctx.override_,
    };

    let handle_unique_violation =
        |db_conn: &mut DBConnection, already_under_txn: bool| {
            if already_under_txn {
                let deleted_ctxt = diesel::delete(dsl::contexts)
                    .filter(dsl::id.eq(&old_ctx_id))
                    .get_result(db_conn)?;

                let ctx = contruct_new_ctx_with_old_overrides(deleted_ctxt);
                update_override_of_existing_ctx(db_conn, ctx)
            } else {
                db_conn.build_transaction().read_write().run(|conn| {
                    let deleted_ctxt = diesel::delete(dsl::contexts)
                        .filter(dsl::id.eq(&old_ctx_id))
                        .get_result(conn)?;
                    let ctx = contruct_new_ctx_with_old_overrides(deleted_ctxt);
                    update_override_of_existing_ctx(conn, ctx)
                })
            }
        };

    match context {
        Ok(ctx) => Ok(get_put_resp(ctx)),
        Err(DatabaseError(UniqueViolation, _)) => {
            if already_under_txn {
                diesel::sql_query("ROLLBACK TO update_ctx_savepoint").execute(conn)?;
            }
            handle_unique_violation(conn, already_under_txn)
        }
        Err(e) => {
            log::error!("update query failed with error: {e:?}");
            Err(anyhow!(e))
        }
    }
}

#[put("/move/{ctx_id}")]
async fn move_handler(
    path: Path<String>,
    req: web::Json<MoveReq>,
    user: User,
    mut db_conn: DbConnection,
) -> actix_web::Result<web::Json<PutResp>> {
    r#move(path.into_inner(), req, &user, &mut db_conn, false)
        .map(|resp| web::Json(resp))
        .map_err(|e| {
            log::info!("move api failed with error: {:?}", e);
            ErrorInternalServerError("")
        })
}

#[get("/{ctx_id}")]
async fn get_context(
    path: web::Path<String>,
    db_conn: DbConnection,
) -> Result<impl Responder> {
    use crate::db::schema::contexts::dsl::*;

    let ctx_id = path.into_inner();
    let DbConnection(mut conn) = db_conn;

    let result: QueryResult<Vec<Context>> =
        contexts.filter(id.eq(ctx_id)).load(&mut conn);

    let ctx_vec = match result {
        Ok(ctx_vec) => ctx_vec,
        Err(e) => {
            log::info!("Failed to execute query, error: {e}");
            return Err(error::ErrorInternalServerError(""));
        }
    };

    match ctx_vec.first() {
        Some(ctx) => Ok(web::Json(ctx.clone())),
        _ => Err(error::ErrorNotFound("")),
    }
}

#[get("/list")]
async fn list_contexts(
    qparams: web::Query<PaginationParams>,
    db_conn: DbConnection,
) -> Result<impl Responder> {
    use crate::db::schema::contexts::dsl::*;
    let DbConnection(mut conn) = db_conn;

    let PaginationParams {
        page: opt_page,
        size: opt_size,
    } = qparams.into_inner();
    let default_page = 1;
    let page = opt_page.unwrap_or(default_page);
    let default_size = 20;
    let size = opt_size.unwrap_or(default_size);

    if page < 1 {
        return Err(error::ErrorBadRequest("Param 'page' has to be at least 1."));
    } else if size < 1 {
        return Err(error::ErrorBadRequest("Param 'size' has to be at least 1."));
    }

    let result: Vec<Context> = contexts
        .order(created_at)
        .limit(i64::from(size))
        .offset(i64::from(size * (page - 1)))
        .load(&mut conn)
        .map_err_to_internal_server("Failed to execute query, error", Value::Null)?;
    Ok(web::Json(result))
}

#[delete("/{ctx_id}")]
async fn delete_context(
    path: Path<String>,
    user: User,
    db_conn: DbConnection,
) -> actix_web::Result<HttpResponse> {
    use contexts::dsl;
    let DbConnection(mut conn) = db_conn;

    let ctx_id = path.into_inner();
    let deleted_row =
        delete(dsl::contexts.filter(dsl::id.eq(&ctx_id))).execute(&mut conn);
    match deleted_row {
        Ok(0) => Err(ErrorNotFound("")),
        Ok(_) => {
            log::info!("{ctx_id} context deleted by {}", user.email);
            Ok(HttpResponse::NoContent().finish())
        }
        Err(e) => {
            log::error!("context delete query failed with error: {e}");
            Err(ErrorInternalServerError(""))
        }
    }
}

#[put("/bulk-operations")]
async fn bulk_operations(
    reqs: web::Json<Vec<ContextAction>>,
    user: User,
    db_conn: DbConnection,
) -> actix_web::Result<web::Json<Vec<ContextBulkResponse>>> {
    use contexts::dsl::contexts;
    let DbConnection(mut conn) = db_conn;

    let mut resp = Vec::<ContextBulkResponse>::new();
    let result = conn.transaction::<_, TransactionError, _>(|transaction_conn| {
        for action in reqs.into_inner().into_iter() {
            match action {
                ContextAction::PUT(put_req) => {
                    let resp_result =
                        put(actix_web::web::Json(put_req), &user, transaction_conn, true);

                    match resp_result {
                        Ok(put_resp) => {
                            resp.push(ContextBulkResponse::PUT(put_resp));
                        }
                        Err(e) => {
                            log::error!("Failed at insert into contexts due to {:?}", e);
                            if e.to_string().contains("Bad schema") {
                                return Err(TransactionError::BadRequest(e.to_string()));
                            } else {
                                return Err(TransactionError::DieselError(
                                    diesel::result::Error::RollbackTransaction,
                                ));
                            }
                        }
                    }
                }
                ContextAction::DELETE(ctx_id) => {
                    let deleted_row =
                        delete(contexts.filter(id.eq(&ctx_id))).execute(transaction_conn);
                    let email = user.clone().email;
                    match deleted_row {
                        // Any kind of error would rollback the tranction but explicitly returning rollback tranction allows you to rollback from any point in transaction.
                        Ok(0) => {
                            return Err(TransactionError::DieselError(
                                diesel::result::Error::RollbackTransaction,
                            ))
                        }
                        Ok(_) => {
                            log::info!("{ctx_id} context deleted by {email}");
                            resp.push(ContextBulkResponse::DELETE(format!(
                                "{ctx_id} deleted succesfully"
                            )))
                        }
                        Err(e) => {
                            log::error!("Delete context failed due to {:?}", e);
                            return Err(TransactionError::DieselError(
                                diesel::result::Error::RollbackTransaction,
                            ));
                        }
                    };
                }
                ContextAction::MOVE((old_ctx_id, move_req)) => {
                    let move_context_resp = r#move(
                        old_ctx_id,
                        actix_web::web::Json(move_req),
                        &user,
                        transaction_conn,
                        true,
                    );

                    match move_context_resp {
                        Ok(move_resp) => resp.push(ContextBulkResponse::MOVE(move_resp)),
                        Err(e) => {
                            log::error!(
                                "Failed at moving context reponse due to {:?}",
                                e
                            );
                            return Err(TransactionError::DieselError(
                                diesel::result::Error::RollbackTransaction,
                            ));
                        }
                    };
                }
            }
        }
        Ok(()) // Commit the transaction
    });
    match result {
        Ok(_) => Ok(web::Json(resp)),
        Err(TransactionError::BadRequest(_)) => Err(ErrorBadRequest("")),
        Err(TransactionError::DieselError(_)) => Err(ErrorInternalServerError("")),
    }
}
