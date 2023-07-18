use crate::{
    db::utils::AppState,
    v1::{
        api::{
            context::types::{PaginationParams, PutReq, PutResp},
            types::AuthenticationInfo,
        },
        db::{
            models::{Context, Dimension},
            schema::cac_v1::{contexts, dimensions::dsl::dimensions},
        },
        helpers::ToActixErr,
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
    ExpressionMethods, PgConnection, QueryDsl, QueryResult, RunQueryDsl,
};
use serde_json::{Value, Value::Null};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(put_context)
        .service(get_context)
        .service(list_contexts)
        .service(delete_context)
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
    auth_info: AuthenticationInfo,
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
        created_by: email,
    })
}

fn update_override_of_existing_ctx(
    conn: &mut PgConnection,
    ctx: Context,
) -> Result<web::Json<PutResp>, diesel::result::Error> {
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
    Ok(web::Json(get_put_resp(new_ctx)))
}

fn get_put_resp(ctx: Context) -> PutResp {
    PutResp {
        context_id: ctx.id,
        override_id: ctx.override_id,
        priority: ctx.priority,
    }
}

#[put("")]
async fn put_context(
    req: web::Json<PutReq>,
    state: Data<AppState>,
    auth_info: AuthenticationInfo,
) -> actix_web::Result<web::Json<PutResp>> {
    use contexts::dsl::contexts;
    let mut conn = state
        .db_pool
        .get()
        .map_err_to_internal_server("unable to get db connection from pool", "")?;

    let new_ctx = create_ctx_from_put_req(req, &mut conn, auth_info)?;

    let insert = diesel::insert_into(contexts)
        .values(&new_ctx)
        .execute(&mut conn);

    match insert {
        Ok(_) => Ok(web::Json(get_put_resp(new_ctx))),
        Err(DatabaseError(UniqueViolation, _)) => {
            update_override_of_existing_ctx(&mut conn, new_ctx)
                .map_err_to_internal_server(
                    "override update of existing context failed",
                    "",
                )
        }
        e => {
            println!("context insert failed with error: {e:?}");
            Err(ErrorInternalServerError(""))
        }
    }
}

#[get("/{ctx_id}")]
async fn get_context(
    path: web::Path<String>,
    state: Data<AppState>,
) -> Result<impl Responder> {
    use crate::v1::db::schema::cac_v1::contexts::dsl::*;

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
    use crate::v1::db::schema::cac_v1::contexts::dsl::*;

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
) -> actix_web::Result<HttpResponse> {
    use contexts::dsl;

    let mut conn = state
        .db_pool
        .get()
        .map_err_to_internal_server("Unable to get db connection from pool", "")?;
    let ctx_id = path.into_inner();
    let deleted_row = delete(dsl::contexts.filter(dsl::id.eq(ctx_id))).execute(&mut conn);
    match deleted_row {
        Ok(0) => Err(ErrorNotFound("")),
        Ok(_) => Ok(HttpResponse::NoContent().finish()),
        Err(e) => {
            log::error!("context delete query failed with error: {e}");
            Err(ErrorInternalServerError(""))
        }
    }
}
