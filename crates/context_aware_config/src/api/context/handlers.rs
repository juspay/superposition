extern crate base64;

use std::{cmp::min, collections::HashSet};

#[cfg(feature = "high-performance-mode")]
use crate::helpers::put_config_in_redis;
use crate::{
    api::{
        context::{
            hash,
            helpers::ensure_description,
            operations,
            types::{
                ContextAction, ContextBulkResponse, ContextFilterSortOn, ContextFilters,
                MoveReq, PutReq, WeightRecomputeResponse,
            },
        },
        dimension::{get_dimension_data, get_dimension_data_map},
    },
    helpers::{add_config_version, calculate_context_weight},
};

use actix_web::{
    delete, get, post, put,
    web::{Data, Json, Path},
    HttpResponse, Scope,
};
use bigdecimal::BigDecimal;
use chrono::Utc;
use diesel::SelectableHelper;
use diesel::{delete, Connection, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::{Map, Value};
use service_utils::service::types::Tenant;
use service_utils::{
    helpers::parse_config_tags,
    service::types::{AppHeader, AppState, CustomHeaders, DbConnection},
};
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    custom_query::{self as superposition_query, CustomQuery, DimensionQuery, QueryMap},
    database::{
        models::cac::Context,
        schema::contexts::{self, id},
    },
    result as superposition, Contextual, Overridden, PaginatedResponse, SortBy,
    TenantConfig, User,
};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(put_handler)
        .service(update_override_handler)
        .service(move_handler)
        .service(delete_context_handler)
        .service(bulk_operations)
        .service(list_contexts)
        .service(get_context_from_condition)
        .service(get_context)
        .service(weight_recompute)
}

#[put("")]
async fn put_handler(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: Json<PutReq>,
    mut db_conn: DbConnection,
    user: User,
    tenant: Tenant,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let (put_response, version_id) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut req_mut = req.into_inner();

            // Use the helper function to ensure the description
            if req_mut.description.is_none() {
                req_mut.description = Some(ensure_description(
                    Value::Object(req_mut.context.clone().into_inner().into()),
                    transaction_conn,
                    &tenant,
                )?);
            }
            let put_response = operations::put(
                Json(req_mut.clone()),
                transaction_conn,
                true,
                &user,
                &tenant,
                &tenant_config,
                false,
            )
            .map_err(|err: superposition::AppError| {
                log::info!("context put failed with error: {:?}", err);
                err
            })?;
            let description = req_mut.description.unwrap_or_default();
            let change_reason = req_mut.change_reason;

            let version_id = add_config_version(
                &state,
                tags,
                description,
                change_reason,
                transaction_conn,
                &tenant,
            )?;
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
    tenant: Tenant,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let (override_resp, version_id) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut req_mut = req.into_inner();
            if req_mut.description.is_none() {
                req_mut.description = Some(ensure_description(
                    Value::Object(req_mut.context.clone().into_inner().into()),
                    transaction_conn,
                    &tenant,
                )?);
            }
            let override_resp = operations::put(
                Json(req_mut.clone()),
                transaction_conn,
                true,
                &user,
                &tenant,
                &tenant_config,
                true,
            )
            .map_err(|err: superposition::AppError| {
                log::info!("context put failed with error: {:?}", err);
                err
            })?;
            let version_id = add_config_version(
                &state,
                tags,
                req_mut.description.unwrap().clone(),
                req_mut.change_reason.clone(),
                transaction_conn,
                &tenant,
            )?;
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

#[put("/move/{ctx_id}")]
async fn move_handler(
    state: Data<AppState>,
    path: Path<String>,
    custom_headers: CustomHeaders,
    req: Json<MoveReq>,
    mut db_conn: DbConnection,
    user: User,
    tenant: Tenant,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let (move_response, version_id) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let move_response = operations::r#move(
                path.into_inner(),
                req,
                transaction_conn,
                true,
                &user,
                &tenant,
                &tenant_config,
            )
            .map_err(|err| {
                log::info!("move api failed with error: {:?}", err);
                err
            })?;
            let version_id = add_config_version(
                &state,
                tags,
                move_response.description.clone(),
                move_response.change_reason.clone(),
                transaction_conn,
                &tenant,
            )?;

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
    tenant: Tenant,
) -> superposition::Result<Json<Context>> {
    use superposition_types::database::schema::contexts::dsl::*;

    let context_id = hash(&Value::Object(req.into_inner()));
    let DbConnection(mut conn) = db_conn;

    let ctx: Context = contexts
        .filter(id.eq(context_id))
        .schema_name(&tenant)
        .get_result::<Context>(&mut conn)?;

    Ok(Json(ctx))
}

#[get("/{ctx_id}")]
async fn get_context(
    path: Path<String>,
    db_conn: DbConnection,
    tenant: Tenant,
) -> superposition::Result<Json<Context>> {
    use superposition_types::database::schema::contexts::dsl::*;

    let ctx_id = path.into_inner();
    let DbConnection(mut conn) = db_conn;

    let ctx: Context = contexts
        .filter(id.eq(ctx_id))
        .schema_name(&tenant)
        .get_result::<Context>(&mut conn)?;

    Ok(Json(ctx))
}

#[get("/list")]
async fn list_contexts(
    filter_params: superposition_query::Query<ContextFilters>,
    dimension_params: DimensionQuery<QueryMap>,
    db_conn: DbConnection,
    tenant: Tenant,
) -> superposition::Result<Json<PaginatedResponse<Context>>> {
    use superposition_types::database::schema::contexts::dsl::*;
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
    let builder = contexts.schema_name(&tenant).into_boxed();

    #[rustfmt::skip]
    let mut builder = match (filter_params.sort_on.unwrap_or_default(), filter_params.sort_by.unwrap_or(SortBy::Asc)) {
        (ContextFilterSortOn::Weight,  SortBy::Asc)  => builder.order(weight.asc()),
        (ContextFilterSortOn::Weight,  SortBy::Desc) => builder.order(weight.desc()),
        (ContextFilterSortOn::CreatedAt, SortBy::Asc)  => builder.order(created_at.asc()),
        (ContextFilterSortOn::CreatedAt, SortBy::Desc) => builder.order(created_at.desc()),
    };

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
            let mut total_count_builder = contexts.schema_name(&tenant).into_boxed();
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

#[delete("/{ctx_id}")]
async fn delete_context_handler(
    state: Data<AppState>,
    path: Path<String>,
    custom_headers: CustomHeaders,
    user: User,
    tenant: Tenant,
    mut db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    use superposition_types::database::schema::contexts::dsl::{
        contexts as contexts_table, id as context_id,
    };
    let ctx_id = path.into_inner();
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let version_id =
        db_conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let context = contexts_table
                .filter(context_id.eq(ctx_id.clone()))
                .schema_name(&tenant)
                .first::<Context>(transaction_conn)?;
            operations::delete(ctx_id.clone(), user.clone(), transaction_conn, &tenant)?;
            let description = context.description;
            let change_reason = format!("Deleted context by {}", user.username);
            let version_id = add_config_version(
                &state,
                tags,
                description,
                change_reason,
                transaction_conn,
                &tenant,
            )?;
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
    tenant: Tenant,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    use contexts::dsl::contexts;
    let DbConnection(mut conn) = db_conn;
    let mut all_descriptions = Vec::new();
    let mut all_change_reasons = Vec::new();

    let tags = parse_config_tags(custom_headers.config_tags)?;
    let (response, version_id) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut response = Vec::<ContextBulkResponse>::new();
            for action in reqs.into_inner().into_iter() {
                match action {
                    ContextAction::Put(put_req) => {
                        let put_resp = operations::put(
                            Json(put_req.clone()),
                            transaction_conn,
                            true,
                            &user,
                            &tenant,
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

                        let ctx_condition = put_req.context.to_owned().into_inner();
                        let ctx_condition_value =
                            Value::Object(ctx_condition.clone().into());

                        let description = if put_req.description.is_none() {
                            ensure_description(
                                ctx_condition_value.clone(),
                                transaction_conn,
                                &tenant,
                            )?
                        } else {
                            put_req
                                .description
                                .expect("Description should not be empty")
                        };
                        all_descriptions.push(description);
                        all_change_reasons.push(put_req.change_reason.clone());
                        response.push(ContextBulkResponse::Put(put_resp));
                    }
                    ContextAction::Delete(ctx_id) => {
                        let context: Context = contexts
                            .filter(id.eq(&ctx_id))
                            .schema_name(&tenant)
                            .first::<Context>(transaction_conn)?;

                        let deleted_row = delete(contexts)
                            .filter(id.eq(&ctx_id))
                            .schema_name(&tenant)
                            .execute(transaction_conn);
                        let description = context.description;

                        let email: String = user.clone().get_email();
                        let change_reason =
                            format!("Context deleted by {}", email.clone());
                        all_descriptions.push(description.clone());
                        all_change_reasons.push(change_reason.clone());
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
                        let move_context_resp = operations::r#move(
                            old_ctx_id,
                            Json(move_req),
                            transaction_conn,
                            true,
                            &user,
                            &tenant,
                            &tenant_config,
                        )
                        .map_err(|err| {
                            log::error!(
                                "Failed at moving context reponse due to {:?}",
                                err
                            );
                            err
                        })?;
                        all_descriptions.push(move_context_resp.description.clone());
                        all_change_reasons.push(move_context_resp.change_reason.clone());

                        response.push(ContextBulkResponse::Move(move_context_resp));
                    }
                }
            }

            let combined_description = all_descriptions.join(",");

            let combined_change_reasons = all_change_reasons.join(",");

            let version_id = add_config_version(
                &state,
                tags,
                combined_description,
                combined_change_reasons,
                transaction_conn,
                &tenant,
            )?;
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

#[put("/weight/recompute")]
async fn weight_recompute(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    tenant: Tenant,
    user: User,
) -> superposition::Result<HttpResponse> {
    use superposition_types::database::schema::contexts::dsl::{
        contexts, last_modified_at, last_modified_by, weight,
    };
    let DbConnection(mut conn) = db_conn;

    let result: Vec<Context> =
        contexts
            .schema_name(&tenant)
            .load(&mut conn)
            .map_err(|err| {
                log::error!("failed to fetch contexts with error: {}", err);
                unexpected_error!("Something went wrong")
            })?;

    let dimension_data = get_dimension_data(&mut conn, &tenant)?;
    let dimension_data_map = get_dimension_data_map(&dimension_data)?;
    let mut response: Vec<WeightRecomputeResponse> = vec![];
    let tags = parse_config_tags(custom_headers.config_tags)?;

    // Recompute weights and add descriptions
    let contexts_new_weight: Vec<(BigDecimal, String, String, String)> = result
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
                        description: context.description.clone(),
                        change_reason: context.change_reason.clone(),
                    });
                    Ok((
                        val,
                        context.id.clone(),
                        context.description.clone(),
                        context.change_reason.clone(),
                    ))
                }
                Err(e) => {
                    log::error!("failed to calculate context weight: {}", e);
                    Err(unexpected_error!("Something went wrong"))
                }
            }
        })
        .collect::<superposition::Result<Vec<(BigDecimal, String, String, String)>>>()?;

    // Update database and add config version
    let last_modified_time = Utc::now().naive_utc();
    let config_version_id =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            for (context_weight, context_id, _description, _change_reason) in contexts_new_weight.clone() {
                diesel::update(contexts.filter(id.eq(context_id)))
                    .set((
                        weight.eq(context_weight),
                        last_modified_at.eq(last_modified_time.clone()),
                        last_modified_by.eq(user.get_email())
                    ))
                    .schema_name(&tenant)
                    .returning(Context::as_returning())
                    .execute(transaction_conn).map_err(|err| {
                        log::error!(
                            "Failed to execute query while recomputing weight, error: {err}"
                        );
                        db_error!(err)
                    })?;
            }
            let description = "Recomputed weight".to_string();
            let change_reason = "Recomputed weight".to_string();
            let version_id = add_config_version(&state, tags, description, change_reason, transaction_conn, &tenant)?;
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
