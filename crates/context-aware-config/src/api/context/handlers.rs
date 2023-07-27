use crate::{
    api::context::types::{PaginationParams, PutReq, PutResp},
    db::{
        models::{Context, Dimension},
        schema::cac_v1::{
            contexts::{self, id},
            dimensions::dsl::dimensions,
        },
    },
};
use actix_web::{
    delete,
    error::{self, ErrorBadRequest, ErrorInternalServerError, ErrorNotFound},
    get, put,
    web::{self, Data, Path},
    HttpResponse, Responder, Result, Scope,
};
use chrono::Utc;
use diesel::{
    delete,
    r2d2::{ConnectionManager, PooledConnection},
    result::{DatabaseErrorKind::*, Error::DatabaseError},
    Connection, ExpressionMethods, PgConnection, QueryDsl, QueryResult, RunQueryDsl,
};
use log::info;
use serde_json::{json, Value, Value::Null};
use service_utils::{
    helpers::ToActixErr,
    service::types::{AppState, AuthenticationInfo},
};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(put_handler)
        .service(list_contexts)
        .service(move_handler)
        .service(get_context)
        .service(delete_context)
        .service(bulk_operations)
}

type DBConnection = PooledConnection<ConnectionManager<PgConnection>>;

fn val_dimensions_cal_priority(
    conn: &mut DBConnection,
    cond: &Value,
) -> Result<i32, String> {
    let mut get_priority = |key: &String, val: &Value| -> Result<i32, String> {
        if key == "var" {
            let dimension_name = val
                .as_str()
                .ok_or_else(|| "Dimension name should be of String type")?;
            dimensions
                .find(dimension_name)
                .first(conn)
                .map(|d: Dimension| d.priority)
                .map_err(|e| format!("{dimension_name}: {}", e.to_string()))
        } else {
            val_dimensions_cal_priority(conn, val)
        }
    };

    match cond {
        Value::Object(x) => x.iter().try_fold(0, |acc, (key, val)| {
            get_priority(key, val).map(|res| res + acc)
        }),
        Value::Array(x) => x.iter().try_fold(0, |acc, item| {
            val_dimensions_cal_priority(conn, item).map(|res| res + acc)
        }),
        _ => Ok(0),
    }
}

fn create_ctx_from_put_req(
    req: web::Json<PutReq>,
    conn: &mut DBConnection,
    auth_info: &AuthenticationInfo,
) -> actix_web::Result<Context> {
    let ctx_condition = Value::Object(req.context.to_owned());
    let priority = match val_dimensions_cal_priority(conn, &ctx_condition) {
        Ok(0) => {
            return Err(ErrorBadRequest("No dimension found in context"));
        }
        Err(e) => {
            return Err(ErrorBadRequest(e));
        }
        Ok(p) => p,
    };
    let context_id = blake3::hash((ctx_condition).to_string().as_bytes()).to_string();
    let override_id = blake3::hash((req.r#override).to_string().as_bytes()).to_string();
    let AuthenticationInfo(email) = auth_info;
    Ok(Context {
        id: context_id.clone(),
        value: ctx_condition,
        priority,
        override_id: override_id.to_owned(),
        override_: req.r#override.to_owned(),
        created_at: Utc::now(),
        created_by: email.to_owned(),
    })
}

fn update_override_of_existing_ctx(
    conn: &mut PgConnection,
    ctx: Context,
) -> Result<PutResp, diesel::result::Error> {
    use contexts::dsl;
    let mut new_override: Value = dsl::contexts
        .filter(dsl::id.eq(&ctx.id))
        .select(dsl::override_)
        .first(conn)?;
    json_patch::merge(&mut new_override, &ctx.override_);
    let new_override_id = blake3::hash((new_override).to_string().as_bytes()).to_string();
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
//TO-DO : Need to create a custom error type which implements error traits
#[derive(Debug)]
enum Error {
    DIESEL(diesel::result::Error),
    STRTYPE(String),
}

fn put(
    req: web::Json<PutReq>,
    auth_info: &AuthenticationInfo,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> Result<PutResp, Error> {
    use contexts::dsl::contexts;

    let new_ctx = create_ctx_from_put_req(req, conn, auth_info).map_err(|e| {
        log::error!("context struct creation failed with err: {e:?}");
        Error::STRTYPE(e.to_string())
    })?;

    let insert = diesel::insert_into(contexts).values(&new_ctx).execute(conn);
    match insert {
        Ok(_) => Ok(get_put_resp(new_ctx)),
        Err(DatabaseError(UniqueViolation, _)) => {
            update_override_of_existing_ctx(conn, new_ctx).map_err(|e| Error::DIESEL(e))
        }
        Err(e) => {
            log::error!("update query failed with error: {e:?}");
            Err(Error::DIESEL(e))
        }
    }
}

#[put("")]
async fn put_handler(
    req: web::Json<PutReq>,
    state: Data<AppState>,
    auth_info: AuthenticationInfo,
) -> actix_web::Result<web::Json<PutResp>> {
    let conn = &mut state
        .db_pool
        .get()
        .map_err_to_internal_server("unable to get db connection from pool", "")?;
    put(req, &auth_info, conn)
        .map(|resp| web::Json(resp))
        .map_err(|e| {
            println!("context put failed with error: {:?}", e);
            ErrorInternalServerError("")
        })
}

fn r#move(
    old_ctx_id: String,
    req: web::Json<PutReq>,
    auth_info: &AuthenticationInfo,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    with_transaction: bool,
) -> Result<PutResp, Error> {
    use contexts::dsl;
    let new_ctx = create_ctx_from_put_req(req, conn, auth_info).map_err(|e| {
        log::error!("update query failed with error: {e:?}");
        Error::STRTYPE(e.to_string())
    })?;
    let update = diesel::update(dsl::contexts)
        .filter(dsl::id.eq(&old_ctx_id))
        .set((&new_ctx, dsl::id.eq(&new_ctx.id)))
        .execute(conn);

    let handle_unique_violation =
        |db_conn: &mut DBConnection, new_ctx: Context, with_transaction: bool| {
            if with_transaction {
                db_conn.build_transaction().read_write().run(|conn| {
                    diesel::delete(dsl::contexts)
                        .filter(dsl::id.eq(&old_ctx_id))
                        .execute(conn)?;
                    update_override_of_existing_ctx(conn, new_ctx)
                })
            } else {
                diesel::delete(dsl::contexts)
                    .filter(dsl::id.eq(&old_ctx_id))
                    .execute(db_conn)?;
                update_override_of_existing_ctx(db_conn, new_ctx)
            }
        };

    match update {
        Ok(0) => Err(Error::STRTYPE(format!(
            "context with id: {old_ctx_id} not found"
        ))),
        Ok(_) => Ok(get_put_resp(new_ctx)),
        Err(DatabaseError(UniqueViolation, _)) => {
            handle_unique_violation(conn, new_ctx, with_transaction)
                .map_err(|e| Error::DIESEL(e))
        }
        Err(e) => {
            log::error!("update query failed with error: {e:?}");
            Err(Error::DIESEL(e))
        }
    }
}

#[put("/move/{ctx_id}")]
async fn move_handler(
    state: Data<AppState>,
    path: Path<String>,
    req: web::Json<PutReq>,
    auth_info: AuthenticationInfo,
) -> actix_web::Result<web::Json<PutResp>> {
    let conn = &mut state
        .db_pool
        .get()
        .map_err_to_internal_server("unable to get db connection from pool", "")?;

    r#move(path.into_inner(), req, &auth_info, conn, true)
        .map(|resp| web::Json(resp))
        .map_err(|e| {
            println!("move api failed with error: {:?}", e);
            ErrorInternalServerError("")
        })
}

#[get("/{ctx_id}")]
async fn get_context(
    path: web::Path<String>,
    state: Data<AppState>,
) -> Result<impl Responder> {
    use crate::db::schema::cac_v1::contexts::dsl::*;

    let ctx_id = path.into_inner();
    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            println!("Unable to get db connection from pool, error: {e}");
            return Err(error::ErrorInternalServerError(""));
        }
    };

    let result: QueryResult<Vec<Context>> =
        contexts.filter(id.eq(ctx_id)).load(&mut conn);

    let ctx_vec = match result {
        Ok(ctx_vec) => ctx_vec,
        Err(e) => {
            println!("Failed to execute query, error: {e}");
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
    state: Data<AppState>,
) -> Result<impl Responder> {
    use crate::db::schema::cac_v1::contexts::dsl::*;

    let mut conn = state
        .db_pool
        .get()
        .map_err_to_internal_server("Unable to get db connection from pool", Null)?;

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
        .map_err_to_internal_server("Failed to execute query, error", Null)?;
    Ok(web::Json(result))
}

#[delete("/{ctx_id}")]
async fn delete_context(
    state: Data<AppState>,
    path: Path<String>,
    auth_info: AuthenticationInfo,
) -> actix_web::Result<HttpResponse> {
    use contexts::dsl;
    let mut conn = state
        .db_pool
        .get()
        .map_err_to_internal_server("Unable to get db connection from pool", "")?;
    let ctx_id = path.into_inner();
    let deleted_row =
        delete(dsl::contexts.filter(dsl::id.eq(&ctx_id))).execute(&mut conn);
    let AuthenticationInfo(email) = auth_info;
    match deleted_row {
        Ok(0) => Err(ErrorNotFound("")),
        Ok(_) => {
            info!("{ctx_id} context deleted by {email}");
            Ok(HttpResponse::NoContent().finish())
        }
        Err(e) => {
            log::error!("context delete query failed with error: {e}");
            Err(ErrorInternalServerError(""))
        }
    }
}

#[derive(serde::Deserialize)]
enum ContextAction {
    PUT(PutReq),
    DELETE(String),
    MOVE((String, PutReq)),
}

#[put("/bulk-operations")]
async fn bulk_operations(
    reqs: web::Json<Vec<ContextAction>>,
    state: Data<AppState>,
    auth_info: AuthenticationInfo,
) -> actix_web::Result<HttpResponse> {
    use contexts::dsl::contexts;
    let mut conn = state
        .db_pool
        .get()
        .map_err_to_internal_server("Unable to get db connection from pool", "")?;

    let mut resp = Vec::<Result<Value, String>>::new();
    let result = conn.transaction::<_, diesel::result::Error, _>(|transaction_conn| {
        for action in reqs.into_inner().into_iter() {
            match action {
                ContextAction::PUT(put_req) => {
                    let resp_result =
                        put(actix_web::web::Json(put_req), &auth_info, transaction_conn);

                    match resp_result {
                        Ok(put_resp) => {
                            resp.push(Ok(json!(put_resp)));
                        }
                        Err(e) => {
                            log::error!("Failed at insert into contexts due to {:?}", e);
                            return Err(diesel::result::Error::RollbackTransaction);
                        }
                    }
                }
                ContextAction::DELETE(ctx_id) => {
                    let deleted_row =
                        delete(contexts.filter(id.eq(&ctx_id))).execute(transaction_conn);
                    let AuthenticationInfo(email) = auth_info.clone();
                    match deleted_row {
                        Ok(0) => return Err(diesel::result::Error::RollbackTransaction),
                        Ok(_) => {
                            info!("{ctx_id} context deleted by {email}");
                            resp.push(Ok(json!(format!("{ctx_id} deleted succesfully"))))
                        }
                        Err(e) => {
                            log::error!("Delete context failed due to {:?}", e);
                            return Err(diesel::result::Error::RollbackTransaction);
                        }
                    };
                }
                ContextAction::MOVE((old_ctx_id, put_req)) => {
                    let move_context_resp = r#move(
                        old_ctx_id,
                        actix_web::web::Json(put_req),
                        &auth_info,
                        transaction_conn,
                        false,
                    );

                    match move_context_resp {
                        Ok(move_resp) => resp.push(Ok(json!(move_resp))),
                        Err(e) => {
                            log::error!(
                                "Failed at moving context reponse due to {:?}",
                                e
                            );
                            return Err(diesel::result::Error::RollbackTransaction);
                        }
                    };
                }
            }
        }
        Ok(()) // Commit the transaction
    });

    match result {
        Ok(_) => Ok(HttpResponse::Ok().json(resp)), // If the transaction was successful, return the responses
        Err(_) => Err(ErrorInternalServerError("")), // If the transaction failed, return an error
    }
}
