extern crate base64;

use std::collections::HashMap;
use std::str;
use std::{cmp::min, collections::HashSet};

use actix_web::{
    delete, get, post, put,
    web::{Data, Json, Path},
    HttpResponse, Scope,
};
use bigdecimal::BigDecimal;
use cac_client::utils::json_to_sorted_string;
use chrono::Utc;
use diesel::{
    delete,
    r2d2::{ConnectionManager, PooledConnection},
    result::{DatabaseErrorKind::*, Error::DatabaseError},
    upsert::excluded,
    Connection, ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::{from_value, json, Map, Value};
#[cfg(feature = "high-performance-mode")]
use service_utils::service::types::Tenant;
use service_utils::{
    helpers::{parse_config_tags, validation_err_to_str},
    service::types::{AppHeader, AppState, CustomHeaders, DbConnection},
};
use superposition_macros::{
    bad_argument, db_error, not_found, unexpected_error, validation_error,
};
use superposition_types::{
    cac::{
        models::Context,
        schema::{
            contexts::{self, id},
            default_configs::dsl,
        },
    },
    custom_query::{self as superposition_query, CustomQuery, DimensionQuery, QueryMap},
    result as superposition, Cac, Contextual, Overridden, Overrides, PaginatedResponse,
    TenantConfig, User,
};

#[cfg(feature = "high-performance-mode")]
use crate::helpers::put_config_in_redis;
use crate::{
    api::{
        context::types::{
            ContextAction, ContextBulkResponse, ContextFilterSortBy, ContextFilters,
            DimensionCondition, MoveReq, PriorityRecomputeResponse, PutReq, PutResp,
            WeightRecomputeResponse,
        },
        dimension::{get_dimension_data, get_dimension_data_map},
    },
    helpers::{
        add_config_version, calculate_context_priority, calculate_context_weight,
        validate_context_jsonschema, DimensionData,
    },
};

use super::helpers::{
    validate_condition_with_functions, validate_condition_with_mandatory_dimensions,
    validate_override_with_functions,
};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(put_handler)
        .service(update_override_handler)
        .service(move_handler)
        .service(delete_context)
        .service(bulk_operations)
        .service(list_contexts)
        .service(get_context_from_condition)
        .service(get_context)
        .service(priority_recompute)
        .service(weight_recompute)
}

type DBConnection = PooledConnection<ConnectionManager<PgConnection>>;

pub fn validate_dimensions_and_calculate_priority(
    object_key: &str,
    cond: &Value,
    dimension_schema_map: &HashMap<String, DimensionData>,
) -> superposition::Result<i32> {
    let get_priority = |key: &String, val: &Value| -> superposition::Result<i32> {
        if key == "var" {
            let dimension_name = val
                .as_str()
                .ok_or(bad_argument!("Dimension name should be of `String` type"))?;
            dimension_schema_map
                .get(dimension_name)
                .map(|dimension_val| &dimension_val.priority)
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
                } else if val.is_none() {
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
                let dimension_data = dimension_schema_map
                    .get(&expected_dimension_name)
                    .ok_or(bad_argument!(
                    "No matching `dimension` {} in dimension table",
                    expected_dimension_name
                ))?;

                validate_context_jsonschema(
                    object_key,
                    &dimension_value,
                    &dimension_data.schema,
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
                "schema validation failed for {key}: {}",
                validation_err_to_str(verrors)
                    .first()
                    .unwrap_or(&String::new())
            ));
        };
    }

    Ok(())
}

fn create_ctx_from_put_req(
    req: Json<PutReq>,
    conn: &mut DBConnection,
    user: &User,
    tenant_config: &TenantConfig,
) -> superposition::Result<Context> {
    let ctx_condition = req.context.to_owned().into_inner();
    let condition_val = Value::Object(ctx_condition.clone().into());
    let r_override = req.r#override.clone().into_inner();
    let ctx_override = Value::Object(r_override.clone().into());
    validate_condition_with_mandatory_dimensions(
        &ctx_condition,
        &tenant_config.mandatory_dimensions,
    )?;
    validate_override_with_default_configs(conn, &r_override)?;
    validate_condition_with_functions(conn, &ctx_condition)?;
    validate_override_with_functions(conn, &r_override)?;

    let dimension_data = get_dimension_data(conn)?;
    let dimension_data_map = get_dimension_data_map(&dimension_data)?;
    let priority = validate_dimensions_and_calculate_priority(
        "context",
        &condition_val,
        &dimension_data_map,
    )?;

    let weight = calculate_context_weight(&condition_val, &dimension_data_map)
        .map_err(|_| unexpected_error!("Something Went Wrong"))?;
    if priority == 0 {
        return Err(bad_argument!("No dimension found in context"));
    }

    let context_id = hash(&condition_val);
    let override_id = hash(&ctx_override);
    Ok(Context {
        id: context_id,
        value: ctx_condition,
        priority,
        override_id: override_id,
        override_: r_override,
        created_at: Utc::now(),
        created_by: user.get_email(),
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
        weight,
    })
}

pub fn hash(val: &Value) -> String {
    let sorted_str: String = json_to_sorted_string(val);
    blake3::hash(sorted_str.as_bytes()).to_string()
}

fn update_override_of_existing_ctx(
    conn: &mut PgConnection,
    ctx: Context,
    user: &User,
) -> superposition::Result<PutResp> {
    use contexts::dsl;
    let mut new_override: Value = dsl::contexts
        .filter(dsl::id.eq(&ctx.id))
        .select(dsl::override_)
        .first(conn)?;
    cac_client::merge(
        &mut new_override,
        &Value::Object(ctx.override_.clone().into()),
    );
    let new_override_id = hash(&new_override);
    let new_ctx = Context {
        override_: Cac::<Overrides>::validate_db_data(
            new_override.as_object().cloned().unwrap_or(Map::new()),
        )
        .map_err(|err| {
            log::error!(
                "update_override_of_existing_ctx : failed to decode context from db {}",
                err
            );
            unexpected_error!(err)
        })?
        .into_inner(),
        override_id: new_override_id,
        ..ctx
    };
    db_update_override(conn, new_ctx, user)
}

fn replace_override_of_existing_ctx(
    conn: &mut PgConnection,
    ctx: Context,
    user: &User,
) -> superposition::Result<PutResp> {
    let new_override = ctx.override_;
    let new_override_id = hash(&Value::Object(new_override.clone().into()));
    let new_ctx = Context {
        override_: new_override,
        override_id: new_override_id,
        ..ctx
    };
    db_update_override(conn, new_ctx, user)
}

fn db_update_override(
    conn: &mut PgConnection,
    ctx: Context,
    user: &User,
) -> superposition::Result<PutResp> {
    use contexts::dsl;
    let update_resp = diesel::update(dsl::contexts)
        .filter(dsl::id.eq(&ctx.id))
        .set((
            dsl::override_.eq(ctx.override_),
            dsl::override_id.eq(ctx.override_id),
            dsl::last_modified_at.eq(Utc::now().naive_utc()),
            dsl::last_modified_by.eq(user.get_email()),
        ))
        .get_result::<Context>(conn)?;
    Ok(get_put_resp(update_resp))
}

fn get_put_resp(ctx: Context) -> PutResp {
    PutResp {
        context_id: ctx.id,
        override_id: ctx.override_id,
        priority: ctx.priority,
        weight: ctx.weight,
    }
}

pub fn put(
    req: Json<PutReq>,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
    user: &User,
    tenant_config: &TenantConfig,
    replace: bool,
) -> superposition::Result<PutResp> {
    use contexts::dsl::contexts;
    let new_ctx = create_ctx_from_put_req(req, conn, user, tenant_config)?;

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
            if replace {
                replace_override_of_existing_ctx(conn, new_ctx, user) // no need for .map(Json)
            } else {
                update_override_of_existing_ctx(conn, new_ctx, user)
            }
        }
        Err(e) => {
            log::error!("failed to update context with db error: {:?}", e);
            Err(db_error!(e))
        }
    }
}

#[put("")]
async fn put_handler(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: Json<PutReq>,
    mut db_conn: DbConnection,
    user: User,
    #[cfg(feature = "high-performance-mode")] tenant: Tenant,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let (put_response, version_id) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let put_response =
                put(req, transaction_conn, true, &user, &tenant_config, false).map_err(
                    |err: superposition::AppError| {
                        log::info!("context put failed with error: {:?}", err);
                        err
                    },
                )?;
            let version_id = add_config_version(&state, tags, transaction_conn)?;
            Ok((put_response, version_id))
        })?;
    let mut http_resp = HttpResponse::Ok();

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));
    cfg_if::cfg_if! {
        if #[cfg(feature = "high-performance-mode")] {
            let DbConnection(mut conn) = db_conn;
            put_config_in_redis(version_id, state, tenant, &mut conn).await?;
        }
    }
    Ok(http_resp.json(put_response))
}

#[put("/overrides")]
async fn update_override_handler(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: Json<PutReq>,
    mut db_conn: DbConnection,
    user: User,
    #[cfg(feature = "high-performance-mode")] tenant: Tenant,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let (override_resp, version_id) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let override_resp =
                put(req, transaction_conn, true, &user, &tenant_config, true).map_err(
                    |err: superposition::AppError| {
                        log::info!("context put failed with error: {:?}", err);
                        err
                    },
                )?;
            let version_id = add_config_version(&state, tags, transaction_conn)?;
            Ok((override_resp, version_id))
        })?;
    let mut http_resp = HttpResponse::Ok();

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));
    cfg_if::cfg_if! {
        if #[cfg(feature = "high-performance-mode")] {
            let DbConnection(mut conn) = db_conn;
            put_config_in_redis(version_id, state, tenant, &mut conn).await?;
        }
    }
    Ok(http_resp.json(override_resp))
}

fn r#move(
    old_ctx_id: String,
    req: Json<MoveReq>,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
    user: &User,
    tenant_config: &TenantConfig,
) -> superposition::Result<PutResp> {
    use contexts::dsl;
    let req = req.into_inner();
    let ctx_condition = req.context.to_owned().into_inner();
    let ctx_condition_value = Value::Object(ctx_condition.clone().into());
    let new_ctx_id = hash(&ctx_condition_value);

    let dimension_data = get_dimension_data(conn)?;
    let dimension_data_map = get_dimension_data_map(&dimension_data)?;
    let priority = validate_dimensions_and_calculate_priority(
        "context",
        &ctx_condition_value,
        &dimension_data_map,
    )?;
    let weight = calculate_context_weight(&ctx_condition_value, &dimension_data_map)
        .map_err(|_| unexpected_error!("Something Went Wrong"))?;

    validate_condition_with_mandatory_dimensions(
        &req.context.into_inner(),
        &tenant_config.mandatory_dimensions,
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
            dsl::value.eq(&ctx_condition_value),
            dsl::priority.eq(priority),
            dsl::last_modified_at.eq(Utc::now().naive_utc()),
            dsl::last_modified_by.eq(user.get_email()),
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
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
        weight,
    };

    let handle_unique_violation =
        |db_conn: &mut DBConnection, already_under_txn: bool| {
            if already_under_txn {
                let deleted_ctxt = diesel::delete(dsl::contexts)
                    .filter(dsl::id.eq(&old_ctx_id))
                    .get_result(db_conn)?;

                let ctx = contruct_new_ctx_with_old_overrides(deleted_ctxt);
                update_override_of_existing_ctx(db_conn, ctx, user)
            } else {
                db_conn.build_transaction().read_write().run(|conn| {
                    let deleted_ctxt = diesel::delete(dsl::contexts)
                        .filter(dsl::id.eq(&old_ctx_id))
                        .get_result(conn)?;
                    let ctx = contruct_new_ctx_with_old_overrides(deleted_ctxt);
                    update_override_of_existing_ctx(conn, ctx, user)
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
    state: Data<AppState>,
    path: Path<String>,
    custom_headers: CustomHeaders,
    req: Json<MoveReq>,
    mut db_conn: DbConnection,
    user: User,
    #[cfg(feature = "high-performance-mode")] tenant: Tenant,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let (move_response, version_id) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let move_response = r#move(
                path.into_inner(),
                req,
                transaction_conn,
                true,
                &user,
                &tenant_config,
            )
            .map_err(|err| {
                log::info!("move api failed with error: {:?}", err);
                err
            })?;
            let version_id = add_config_version(&state, tags, transaction_conn)?;
            Ok((move_response, version_id))
        })?;
    let mut http_resp = HttpResponse::Ok();

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));
    cfg_if::cfg_if! {
        if #[cfg(feature = "high-performance-mode")] {
            let DbConnection(mut conn) = db_conn;
            put_config_in_redis(version_id, state, tenant, &mut conn).await?;
        }
    }
    Ok(http_resp.json(move_response))
}

#[post("/get")]
async fn get_context_from_condition(
    db_conn: DbConnection,
    req: Json<Map<String, Value>>,
) -> superposition::Result<Json<Context>> {
    use superposition_types::cac::schema::contexts::dsl::*;

    let context_id = hash(&Value::Object(req.into_inner()));
    let DbConnection(mut conn) = db_conn;

    let ctx: Context = contexts
        .filter(id.eq(context_id))
        .get_result::<Context>(&mut conn)?;

    Ok(Json(ctx))
}

#[get("/{ctx_id}")]
async fn get_context(
    path: Path<String>,
    db_conn: DbConnection,
) -> superposition::Result<Json<Context>> {
    use superposition_types::cac::schema::contexts::dsl::*;

    let ctx_id = path.into_inner();
    let DbConnection(mut conn) = db_conn;

    let ctx: Context = contexts
        .filter(id.eq(ctx_id))
        .get_result::<Context>(&mut conn)?;

    Ok(Json(ctx))
}

#[get("/list")]
async fn list_contexts(
    filter_params: superposition_query::Query<ContextFilters>,
    dimension_params: DimensionQuery<QueryMap>,
    db_conn: DbConnection,
) -> superposition::Result<Json<PaginatedResponse<Context>>> {
    use superposition_types::cac::schema::contexts::dsl::*;
    let DbConnection(mut conn) = db_conn;

    let filter_params = filter_params.into_inner();
    let page = filter_params.page.unwrap_or(1);
    let size = filter_params.size.unwrap_or(10);

    if page < 1 {
        return Err(bad_argument!("Param 'page' has to be at least 1."));
    } else if size < 1 {
        return Err(bad_argument!("Param 'size' has to be at least 1."));
    }

    let dimension_params = dimension_params.into_inner();

    let mut builder = contexts.into_boxed();
    match filter_params.sort_by.unwrap_or_default() {
        ContextFilterSortBy::PriorityAsc => builder = builder.order(priority.asc()),
        ContextFilterSortBy::PriorityDesc => builder = builder.order(priority.desc()),
        ContextFilterSortBy::CreatedAtAsc => builder = builder.order(created_at.asc()),
        ContextFilterSortBy::CreatedAtDesc => builder = builder.order(created_at.desc()),
    }

    if let Some(created_by_filter) = filter_params.created_by.clone() {
        builder = builder.filter(created_by.eq_any(created_by_filter.0))
    }

    let (data, total_items) =
        if dimension_params.len() > 0 || filter_params.prefix.is_some() {
            let mut all_contexts: Vec<Context> = builder.load(&mut conn)?;
            if let Some(prefix) = filter_params.prefix {
                let prefix_list = HashSet::from_iter(prefix.0);
                all_contexts = all_contexts
                    .into_iter()
                    .filter_map(|mut context| {
                        Context::filter_keys_by_prefix(&context, &prefix_list)
                            .map(|filtered_overrides_map| {
                                context.override_ = filtered_overrides_map.into_inner();
                                context
                            })
                            .ok()
                    })
                    .collect()
            }
            let dimension_keys = dimension_params.keys().cloned().collect::<Vec<_>>();
            let dimension_filter_contexts =
                Context::filter_by_dimension(all_contexts, &dimension_keys);
            let eval_filter_contexts =
                Context::filter_by_eval(dimension_filter_contexts, &dimension_params);

            let total_items = eval_filter_contexts.len();
            let start = (size * (page - 1)) as usize;
            let end = min((size * page) as usize, total_items);
            let data = eval_filter_contexts
                .get(start..end)
                .map_or(vec![], |slice| slice.to_vec());

            (data, total_items as i64)
        } else {
            let mut total_count_builder = contexts.into_boxed();
            if let Some(created_bys) = filter_params.created_by {
                total_count_builder =
                    total_count_builder.filter(created_by.eq_any(created_bys.0))
            }
            let total_items: i64 = total_count_builder.count().get_result(&mut conn)?;
            let data = builder
                .limit(i64::from(size))
                .offset(i64::from(size * (page - 1)))
                .load::<Context>(&mut conn)?;

            (data, total_items)
        };

    Ok(Json(PaginatedResponse {
        total_pages: (total_items as f64 / size as f64).ceil() as i64,
        total_items,
        data,
    }))
}

pub fn delete_context_api(
    ctx_id: String,
    user: User,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<()> {
    use contexts::dsl;
    diesel::update(dsl::contexts)
        .filter(dsl::id.eq(&ctx_id))
        .set((
            dsl::last_modified_at.eq(Utc::now().naive_utc()),
            dsl::last_modified_by.eq(user.get_email()),
        ))
        .execute(conn)?;
    let deleted_row = delete(dsl::contexts.filter(dsl::id.eq(&ctx_id))).execute(conn);
    match deleted_row {
        Ok(0) => Err(not_found!("Context Id `{}` doesn't exists", ctx_id)),
        Ok(_) => {
            log::info!("{ctx_id} context deleted by {}", user.get_email());
            Ok(())
        }
        Err(e) => {
            log::error!("context delete query failed with error: {e}");
            Err(unexpected_error!("Something went wrong."))
        }
    }
}

#[delete("/{ctx_id}")]
async fn delete_context(
    state: Data<AppState>,
    path: Path<String>,
    custom_headers: CustomHeaders,
    user: User,
    #[cfg(feature = "high-performance-mode")] tenant: Tenant,
    mut db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let ctx_id = path.into_inner();
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let version_id =
        db_conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            delete_context_api(ctx_id, user, transaction_conn)?;
            let version_id = add_config_version(&state, tags, transaction_conn)?;
            Ok(version_id)
        })?;
    cfg_if::cfg_if! {
        if #[cfg(feature = "high-performance-mode")] {
            let DbConnection(mut conn) = db_conn;
            put_config_in_redis(version_id, state, tenant, &mut conn).await?;
        }
    }
    Ok(HttpResponse::NoContent()
        .insert_header((
            AppHeader::XConfigVersion.to_string().as_str(),
            version_id.to_string().as_str(),
        ))
        .finish())
}

#[put("/bulk-operations")]
async fn bulk_operations(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    reqs: Json<Vec<ContextAction>>,
    db_conn: DbConnection,
    user: User,
    #[cfg(feature = "high-performance-mode")] tenant: Tenant,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    use contexts::dsl::contexts;
    let DbConnection(mut conn) = db_conn;
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let (response, version_id) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut response = Vec::<ContextBulkResponse>::new();
            for action in reqs.into_inner().into_iter() {
                match action {
                    ContextAction::Put(put_req) => {
                        let put_resp = put(
                            Json(put_req),
                            transaction_conn,
                            true,
                            &user,
                            &tenant_config,
                            false,
                        )
                        .map_err(|err| {
                            log::error!(
                                "Failed at insert into contexts due to {:?}",
                                err
                            );
                            err
                        })?;
                        response.push(ContextBulkResponse::Put(put_resp));
                    }
                    ContextAction::Delete(ctx_id) => {
                        let deleted_row = delete(contexts.filter(id.eq(&ctx_id)))
                            .execute(transaction_conn);
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
                                response.push(ContextBulkResponse::Delete(format!(
                                    "{ctx_id} deleted succesfully"
                                )))
                            }
                            Err(e) => {
                                log::error!("Delete context failed due to {:?}", e);
                                return Err(db_error!(e));
                            }
                        };
                    }
                    ContextAction::Move((old_ctx_id, move_req)) => {
                        let move_context_resp = r#move(
                            old_ctx_id,
                            Json(move_req),
                            transaction_conn,
                            true,
                            &user,
                            &tenant_config,
                        )
                        .map_err(|err| {
                            log::error!(
                                "Failed at moving context reponse due to {:?}",
                                err
                            );
                            err
                        })?;
                        response.push(ContextBulkResponse::Move(move_context_resp));
                    }
                }
            }

            let version_id = add_config_version(&state, tags, transaction_conn)?;
            Ok((response, version_id))
        })?;
    let mut http_resp = HttpResponse::Ok();
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));

    // Commit the transaction
    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(version_id, state, tenant, &mut conn).await?;
    Ok(http_resp.json(response))
}

#[put("/priority/recompute")]
async fn priority_recompute(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    #[cfg(feature = "high-performance-mode")] tenant: Tenant,
    _user: User,
) -> superposition::Result<HttpResponse> {
    use superposition_types::cac::schema::contexts::dsl::*;
    let DbConnection(mut conn) = db_conn;

    let result: Vec<Context> = contexts.load(&mut conn).map_err(|err| {
        log::error!("failed to fetch contexts with error: {}", err);
        unexpected_error!("Something went wrong")
    })?;

    let dimension_data = get_dimension_data(&mut conn)?;
    let dimension_data_map = get_dimension_data_map(&dimension_data)?;
    let mut response: Vec<PriorityRecomputeResponse> = vec![];
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let update_contexts = result
        .clone()
        .into_iter()
        .map(|context| {
            let new_priority = calculate_context_priority(
                "context",
                &Value::Object(context.value.clone().into()),
                &dimension_data_map,
            )
            .map_err(|err| {
                log::error!("failed to calculate context priority: {}", err);
                unexpected_error!("Something went wrong")
            });

            match new_priority {
                Ok(val) => {
                    response.push(PriorityRecomputeResponse {
                        id: context.id.clone(),
                        condition: context.value.clone(),
                        old_priority: context.priority,
                        new_priority: val,
                    });
                    Ok(Context {
                        priority: val,
                        ..context.clone()
                    })
                }
                Err(e) => Err(e),
            }
        })
        .collect::<superposition::Result<Vec<Context>>>()?;

    let config_version_id =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let insert = diesel::insert_into(contexts)
                .values(&update_contexts)
                .on_conflict(id)
                .do_update()
                .set(priority.eq(excluded(priority)))
                .execute(transaction_conn);
            let version_id = add_config_version(&state, tags, transaction_conn)?;
            match insert {
                Ok(_) => Ok(version_id),
                Err(err) => {
                    log::error!(
                    "Failed to execute query while recomputing priority, error: {err}"
                );
                    Err(db_error!(err))
                }
            }
        })?;
    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(config_version_id, state, tenant, &mut conn).await?;
    let mut http_resp = HttpResponse::Ok();
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version_id.to_string(),
    ));
    Ok(http_resp.json(response))
}

#[put("/weight/recompute")]
async fn weight_recompute(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    #[cfg(feature = "high-performance-mode")] tenant: Tenant,
    user: User,
) -> superposition::Result<HttpResponse> {
    use superposition_types::cac::schema::contexts::dsl::*;
    let DbConnection(mut conn) = db_conn;

    let result: Vec<Context> = contexts.load(&mut conn).map_err(|err| {
        log::error!("failed to fetch contexts with error: {}", err);
        unexpected_error!("Something went wrong")
    })?;

    let dimension_data = get_dimension_data(&mut conn)?;
    let dimension_data_map = get_dimension_data_map(&dimension_data)?;
    let mut response: Vec<WeightRecomputeResponse> = vec![];
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let contexts_new_weight: Vec<(BigDecimal, String)> = result
        .clone()
        .into_iter()
        .map(|context| {
            let new_weight = calculate_context_weight(
                &Value::Object(context.value.clone().into()),
                &dimension_data_map,
            );

            match new_weight {
                Ok(val) => {
                    response.push(WeightRecomputeResponse {
                        id: context.id.clone(),
                        condition: context.value.clone(),
                        old_weight: context.weight.clone(),
                        new_weight: val.clone(),
                    });
                    Ok((val, context.id.clone()))
                }
                Err(e) => {
                    log::error!("failed to calculate context priority: {}", e);
                    Err(unexpected_error!("Something went wrong"))
                }
            }
        })
        .collect::<superposition::Result<Vec<(BigDecimal, String)>>>()?;

    let last_modified_time = Utc::now().naive_utc();
    let config_version_id =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            for (context_weight, context_id) in contexts_new_weight {
                diesel::update(contexts.filter(id.eq(context_id)))
                    .set((weight.eq(context_weight), last_modified_at.eq(last_modified_time.clone()), last_modified_by.eq(user.get_email())))
                    .execute(transaction_conn).map_err(|err| {
                        log::error!(
                            "Failed to execute query while recomputing priority, error: {err}"
                        );
                        db_error!(err)
                    })?;
            }
            let version_id = add_config_version(&state, tags, transaction_conn)?;
            Ok(version_id)
        })?;
    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(config_version_id, state, tenant, &mut conn).await?;
    let mut http_resp = HttpResponse::Ok();
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version_id.to_string(),
    ));
    Ok(http_resp.json(response))
}
