extern crate base64;
use std::str;

use crate::helpers::{json_to_sorted_string, validate_context_jsonschema};
use crate::{
    api::{
        context::types::{
            ContextAction, ContextBulkResponse, DimensionCondition, MoveReq,
            PaginationParams, PutReq, PutResp,
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
    delete, get, put,
    web::{Json, Path, Query},
    HttpResponse, Responder, Scope,
};
use chrono::Utc;
use diesel::{
    delete,
    r2d2::{ConnectionManager, PooledConnection},
    result::{DatabaseErrorKind::*, Error::DatabaseError},
    Connection, ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{from_value, json, Map, Value};
use service_utils::service::types::DbConnection;
use service_utils::{db_error, not_found, unexpected_error, validation_error};
use std::collections::HashMap;
use superposition_types::{SuperpositionUser, User};

use super::helpers::{
    validate_condition_with_functions, validate_override_with_functions,
};

use service_utils::{bad_argument, result as superposition};

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
) -> superposition::Result<i32> {
    let get_priority = |key: &String, val: &Value| -> superposition::Result<i32> {
        if key == "var" {
            let dimension_name = val
                .as_str()
                .ok_or(bad_argument!("Dimension name should be of `String` type"))?;
            dimension_schema_map
                .get(dimension_name)
                .map(|(_, priority)| priority)
                .ok_or(bad_argument!(
                    "No matching dimension ({}) found",
                    dimension_name
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
                    .ok_or(bad_argument!(
                        "No matching `dimension` {} in dimension table",
                        expected_dimension_name
                    ))?;

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
) -> superposition::Result<()> {
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
            .ok_or(bad_argument!("failed to get schema for config key {}", key))?;
        let instance = value;
        let schema_compile_result = JSONSchema::options()
            .with_draft(Draft::Draft7)
            .compile(schema);
        let jschema = match schema_compile_result {
            Ok(jschema) => jschema,
            Err(e) => {
                log::info!("Failed to compile as a Draft-7 JSON schema: {e}");
                return Err(bad_argument!(
                    "failed to compile ({}) config key schema",
                    key
                ));
            }
        };
        if let Err(e) = jschema.validate(instance) {
            let verrors = e.collect::<Vec<ValidationError>>();
            log::error!("({key}) config key validation error: {:?}", verrors);
            return Err(validation_error!(
                "schema validation failed for {key} with error {:?}",
                verrors
            ));
        };
    }

    Ok(())
}

fn create_ctx_from_put_req(
    req: Json<PutReq>,
    conn: &mut DBConnection,
    user: &User,
) -> superposition::Result<Context> {
    let ctx_condition = Value::Object(req.context.to_owned());
    let ctx_override: Value = req.r#override.to_owned().into();
    validate_override_with_default_configs(conn, &req.r#override)?;
    validate_condition_with_functions(conn, &ctx_condition)?;
    validate_override_with_functions(conn, &req.r#override)?;

    let dimension_schema_map = get_all_dimension_schema_map(conn)?;

    let priority = validate_dimensions_and_calculate_priority(
        "context",
        &ctx_condition,
        &dimension_schema_map,
    )?;

    if priority == 0 {
        return Err(bad_argument!("No dimension found in context"));
    }

    let context_id = hash(&ctx_condition);
    let override_id = hash(&ctx_override);
    Ok(Context {
        id: context_id.clone(),
        value: ctx_condition,
        priority,
        override_id: override_id.to_owned(),
        override_: ctx_override.to_owned(),
        created_at: Utc::now(),
        created_by: user.get_email(),
    })
}

fn hash(val: &Value) -> String {
    let sorted_str: String = json_to_sorted_string(val);
    blake3::hash(sorted_str.as_bytes()).to_string()
}

fn update_override_of_existing_ctx(
    conn: &mut PgConnection,
    ctx: Context,
) -> superposition::Result<PutResp> {
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
    req: Json<PutReq>,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
    user: &User,
) -> superposition::Result<PutResp> {
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
            log::error!("failed to update context with db error: {:?}", e);
            Err(db_error!(e))
        }
    }
}

#[put("")]
async fn put_handler(
    req: Json<PutReq>,
    mut db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<PutResp>> {
    put(req, &mut db_conn, false, &user)
        .map(|resp| Json(resp))
        .map_err(|err: superposition::AppError| {
            log::info!("context put failed with error: {:?}", err);
            err
        })
}

fn r#move(
    old_ctx_id: String,
    req: Json<MoveReq>,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
    user: &User,
) -> superposition::Result<PutResp> {
    use contexts::dsl;
    let req = req.into_inner();
    let ctx_condition = Value::Object(req.context);
    let new_ctx_id = hash(&ctx_condition);
    let dimension_schema_map = get_all_dimension_schema_map(conn)?;
    let priority = validate_dimensions_and_calculate_priority(
        "context",
        &ctx_condition,
        &dimension_schema_map,
    )?;

    if priority == 0 {
        return Err(bad_argument!("no dimension found in context"));
    }

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
        created_by: user.get_email(),
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
            log::error!("failed to move context with db error: {:?}", e);
            Err(db_error!(e))
        }
    }
}

#[put("/move/{ctx_id}")]
async fn move_handler(
    path: Path<String>,
    req: Json<MoveReq>,
    mut db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<PutResp>> {
    r#move(path.into_inner(), req, &mut db_conn, false, &user)
        .map(|resp| Json(resp))
        .map_err(|err| {
            log::info!("move api failed with error: {:?}", err);
            err
        })
}

#[get("/{ctx_id}")]
async fn get_context(
    path: Path<String>,
    db_conn: DbConnection,
) -> superposition::Result<impl Responder> {
    use crate::db::schema::contexts::dsl::*;

    let ctx_id = path.into_inner();
    let DbConnection(mut conn) = db_conn;

    let ctx: Context = contexts
        .filter(id.eq(ctx_id))
        .get_result::<Context>(&mut conn)?;

    Ok(Json(ctx))
}

#[get("/list")]
async fn list_contexts(
    qparams: Query<PaginationParams>,
    db_conn: DbConnection,
) -> superposition::Result<impl Responder> {
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
        return Err(bad_argument!("Param 'page' has to be at least 1."));
    } else if size < 1 {
        return Err(bad_argument!("Param 'size' has to be at least 1."));
    }

    let result: Vec<Context> = contexts
        .order(created_at)
        .limit(i64::from(size))
        .offset(i64::from(size * (page - 1)))
        .load(&mut conn)?;

    Ok(Json(result))
}

#[delete("/{ctx_id}")]
async fn delete_context(
    path: Path<String>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    use contexts::dsl;
    let DbConnection(mut conn) = db_conn;

    let ctx_id = path.into_inner();
    let deleted_row =
        delete(dsl::contexts.filter(dsl::id.eq(&ctx_id))).execute(&mut conn);
    match deleted_row {
        Ok(0) => Err(not_found!("Context Id `{}` doesn't exists", ctx_id)),
        Ok(_) => {
            log::info!("{ctx_id} context deleted by {}", user.get_email());
            Ok(HttpResponse::NoContent().finish())
        }
        Err(e) => {
            log::error!("context delete query failed with error: {e}");
            Err(unexpected_error!("Something went wrong."))
        }
    }
}

#[put("/bulk-operations")]
async fn bulk_operations(
    reqs: Json<Vec<ContextAction>>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Vec<ContextBulkResponse>>> {
    use contexts::dsl::contexts;
    let DbConnection(mut conn) = db_conn;

    let mut response = Vec::<ContextBulkResponse>::new();
    conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
        for action in reqs.into_inner().into_iter() {
            match action {
                ContextAction::PUT(put_req) => {
                    let put_resp = put(Json(put_req), transaction_conn, true, &user)
                        .map_err(|err| {
                            log::error!(
                                "Failed at insert into contexts due to {:?}",
                                err
                            );
                            err
                        })?;
                    response.push(ContextBulkResponse::PUT(put_resp));
                }
                ContextAction::DELETE(ctx_id) => {
                    let deleted_row =
                        delete(contexts.filter(id.eq(&ctx_id))).execute(transaction_conn);
                    let email: String = user.get_email();
                    match deleted_row {
                        // Any kind of error would rollback the tranction but explicitly returning rollback tranction allows you to rollback from any point in transaction.
                        Ok(0) => {
                            return Err(bad_argument!(
                                "context with id {} not found",
                                ctx_id
                            ))
                        }
                        Ok(_) => {
                            log::info!("{ctx_id} context deleted by {email}");
                            response.push(ContextBulkResponse::DELETE(format!(
                                "{ctx_id} deleted succesfully"
                            )))
                        }
                        Err(e) => {
                            log::error!("Delete context failed due to {:?}", e);
                            return Err(db_error!(e));
                        }
                    };
                }
                ContextAction::MOVE((old_ctx_id, move_req)) => {
                    let move_context_resp =
                        r#move(old_ctx_id, Json(move_req), transaction_conn, true, &user)
                            .map_err(|err| {
                                log::error!(
                                    "Failed at moving context reponse due to {:?}",
                                    err
                                );
                                err
                            })?;
                    response.push(ContextBulkResponse::MOVE(move_context_resp));
                }
            }
        }
        Ok(()) // Commit the transaction
    })?;
    Ok(Json(response))
}
