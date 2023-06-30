use crate::{
    db::utils::AppState,
    v1::{
        api::context::types::{AddContextReq, AddContextResp, PaginationParams},
        db::{
            models::{Context, Dimension},
            schema::{contexts::dsl::contexts, dimensions::dsl::dimensions},
        },
        helpers::ToActixErr,
    },
};
use actix_web::{
    error, get, put,
    web::{self, Data},
    HttpResponse, Responder, Result, Scope,
};
use chrono::Utc;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    result::{DatabaseErrorKind::*, Error::DatabaseError},
    ExpressionMethods, PgConnection, QueryDsl, QueryResult, RunQueryDsl,
};
use serde_json::{json, Value, Value::Null};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(add_contexts_overrides)
        .service(get_context)
        .service(list_contexts)
}

type DBConnection = PooledConnection<ConnectionManager<PgConnection>>;

fn val_dimensions_cal_priority(conn: &mut DBConnection, cond: &Value) -> Result<i32, String> {
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

#[put("add")]
async fn add_contexts_overrides(
    req: web::Json<AddContextReq>,
    state: Data<AppState>,
) -> HttpResponse {
    let ctxt_cond = json!({
        "and": req.context
    });

    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            println!("unable to get db connection from pool, error: {e}");
            return HttpResponse::InternalServerError().finish();
        }
    };

    let priority = match val_dimensions_cal_priority(&mut conn, &ctxt_cond) {
        Ok(0) => {
            return HttpResponse::BadRequest().body("No dimension found in contexts");
        }
        Err(e) => {
            return HttpResponse::BadRequest().body(e);
        }
        Ok(p) => p,
    };
    let context_id = blake3::hash((ctxt_cond).to_string().as_bytes()).to_string();
    let override_id = blake3::hash((req.r#override).to_string().as_bytes()).to_string();

    let new_ctxt = Context {
        id: context_id.clone(),
        value: ctxt_cond,
        priority,
        override_id: override_id.to_owned(),
        override_: req.r#override.to_owned(),
        created_at: Utc::now(),
        created_by: "some_user".to_string(),
    };

    let insert = diesel::insert_into(contexts)
        .values(&new_ctxt)
        .execute(&mut conn);

    let resp = AddContextResp {
        context_id,
        override_id,
        priority,
    };

    match insert {
        Ok(_) => HttpResponse::Created()
            .insert_header(("x-info", "new context created"))
            .json(resp),
        Err(DatabaseError(UniqueViolation, _)) => HttpResponse::Ok()
            .insert_header(("x-info", "context already exists"))
            .json(resp),
        e => {
            println!("DB transaction failed with error: {e:?}");
            return HttpResponse::InternalServerError().finish();
        }
    }
}

#[get("/{ctx_id}")]
async fn get_context(path: web::Path<String>, state: Data<AppState>) -> Result<impl Responder> {
    use crate::v1::db::schema::contexts::dsl::*;

    let ctx_id = path.into_inner();
    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            println!("Unable to get db connection from pool, error: {e}");
            return Err(error::ErrorInternalServerError(""));
        }
    };

    let result: QueryResult<Vec<Context>> = contexts.filter(id.eq(ctx_id)).load(&mut conn);

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
    use crate::v1::db::schema::contexts::dsl::*;

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
