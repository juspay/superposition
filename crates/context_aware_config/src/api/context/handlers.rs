use std::{cmp::min, collections::HashSet};

#[cfg(feature = "high-performance-mode")]
use crate::helpers::put_config_in_redis;
use crate::{
    api::{
        context::{
            hash,
            helpers::{query_description, validate_ctx},
            operations,
        },
        dimension::{get_dimension_data, get_dimension_data_map},
    },
    helpers::{add_config_version, calculate_context_weight},
};

use actix_web::{
    delete, get, post, put,
    web::{Data, Json, Path},
    Either, HttpResponse, Scope,
};
use bigdecimal::BigDecimal;
use chrono::Utc;
use diesel::{
    delete,
    dsl::sql,
    sql_types::{Bool, Text},
    Connection, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
};
use serde_json::{Map, Value};
use service_utils::{
    helpers::parse_config_tags,
    service::types::{AppHeader, AppState, CustomHeaders, DbConnection, SchemaName},
};
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    api::context::{
        BulkOperation, BulkOperationResponse, ContextAction, ContextBulkResponse,
        ContextListFilters, ContextValidationRequest, MoveRequest, PutRequest, SortOn,
        UpdateRequest, WeightRecomputeResponse,
    },
    custom_query::{
        self as superposition_query, CustomQuery, DimensionQuery, PaginationParams,
        QueryMap,
    },
    database::{
        models::{cac::Context, ChangeReason, Description},
        schema::contexts::{self, id},
    },
    result as superposition, Contextual, ListResponse, Overridden, PaginatedResponse,
    SortBy, User,
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
        .service(validate_context)
}

#[put("")]
async fn put_handler(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: Json<PutRequest>,
    mut db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let description = match req.description.clone() {
        Some(val) => val,
        None => query_description(
            Value::Object(req.context.clone().into_inner().into()),
            &mut db_conn,
            &schema_name,
        )?,
    };
    let req_change_reason = req.change_reason.clone();

    let (put_response, version_id) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            // Use the helper function to ensure the description

            let put_response = operations::upsert(
                req.into_inner(),
                description,
                transaction_conn,
                true,
                &user,
                &schema_name,
                false,
            )
            .map_err(|err: superposition::AppError| {
                log::error!("context put failed with error: {:?}", err);
                err
            })?;

            let version_id = add_config_version(
                &state,
                tags,
                req_change_reason.into(),
                transaction_conn,
                &schema_name,
            )?;
            Ok((put_response, version_id))
        })?;

    let mut http_resp = HttpResponse::Ok();

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));

    #[cfg(feature = "high-performance-mode")]
    {
        let DbConnection(mut conn) = db_conn;
        put_config_in_redis(version_id, state, &schema_name, &mut conn).await?;
    }

    Ok(http_resp.json(put_response))
}

#[put("/overrides")]
async fn update_override_handler(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: Json<UpdateRequest>,
    mut db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let req_change_reason = req.change_reason.clone();
    let (override_resp, version_id) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let override_resp = operations::update(
                req.into_inner(),
                transaction_conn,
                &user,
                &schema_name,
            )
            .map_err(|err: superposition::AppError| {
                log::error!("context update failed with error: {:?}", err);
                err
            })?;
            let version_id = add_config_version(
                &state,
                tags,
                req_change_reason.into(),
                transaction_conn,
                &schema_name,
            )?;
            Ok((override_resp, version_id))
        })?;
    let mut http_resp = HttpResponse::Ok();

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));

    #[cfg(feature = "high-performance-mode")]
    {
        let DbConnection(mut conn) = db_conn;
        put_config_in_redis(version_id, state, &schema_name, &mut conn).await?;
    }

    Ok(http_resp.json(override_resp))
}

#[allow(clippy::too_many_arguments)]
#[put("/move/{ctx_id}")]
async fn move_handler(
    state: Data<AppState>,
    path: Path<String>,
    custom_headers: CustomHeaders,
    req: Json<MoveRequest>,
    mut db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let description = match req.description.clone() {
        Some(val) => val,
        None => query_description(
            Value::Object(req.context.clone().into_inner().into()),
            &mut db_conn,
            &schema_name,
        )?,
    };

    let (move_response, version_id) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let move_response = operations::r#move(
                path.into_inner(),
                req,
                description,
                transaction_conn,
                true,
                &user,
                &schema_name,
            )
            .map_err(|err| {
                log::error!("move api failed with error: {:?}", err);
                err
            })?;
            let version_id = add_config_version(
                &state,
                tags,
                move_response.change_reason.clone().into(),
                transaction_conn,
                &schema_name,
            )?;

            Ok((move_response, version_id))
        })?;
    let mut http_resp = HttpResponse::Ok();

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));

    #[cfg(feature = "high-performance-mode")]
    {
        let DbConnection(mut conn) = db_conn;
        put_config_in_redis(version_id, state, &schema_name, &mut conn).await?;
    }

    Ok(http_resp.json(move_response))
}

#[post("/get")]
async fn get_context_from_condition(
    db_conn: DbConnection,
    req: Json<Map<String, Value>>,
    schema_name: SchemaName,
) -> superposition::Result<Json<Context>> {
    use superposition_types::database::schema::contexts::dsl::*;

    let context_id = hash(&Value::Object(req.into_inner()));
    let DbConnection(mut conn) = db_conn;

    let ctx: Context = contexts
        .filter(id.eq(context_id))
        .schema_name(&schema_name)
        .get_result::<Context>(&mut conn)?;

    Ok(Json(ctx))
}

#[get("/{ctx_id}")]
async fn get_context(
    path: Path<String>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<Context>> {
    use superposition_types::database::schema::contexts::dsl::*;

    let ctx_id = path.into_inner();
    let DbConnection(mut conn) = db_conn;

    let ctx: Context = contexts
        .filter(id.eq(ctx_id))
        .schema_name(&schema_name)
        .get_result::<Context>(&mut conn)?;

    Ok(Json(ctx))
}

#[get("/list")]
async fn list_contexts(
    filter_params: superposition_query::Query<ContextListFilters>,
    pagination_params: superposition_query::Query<PaginationParams>,
    dimension_params: DimensionQuery<QueryMap>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<Context>>> {
    use superposition_types::database::schema::contexts::dsl::*;
    let DbConnection(mut conn) = db_conn;

    let filter_params = filter_params.into_inner();
    let pagination_params = pagination_params.into_inner();

    let page = pagination_params.page.unwrap_or(1);
    let count = pagination_params.count.unwrap_or(10);
    let show_all = pagination_params.all.unwrap_or_default();
    let offset = count * (page - 1);

    let dimension_params = dimension_params.into_inner();

    let get_base_query = || {
        let mut builder = contexts.schema_name(&schema_name).into_boxed();
        if let Some(creators) = filter_params.created_by.clone() {
            builder = builder.filter(created_by.eq_any(creators.0))
        }

        if let Some(last_modifiers) = filter_params.last_modified_by.clone() {
            builder = builder.filter(last_modified_by.eq_any(last_modifiers.0))
        }

        if let Some(plaintext) = filter_params.plaintext.clone() {
            builder = builder.filter(
                sql::<Bool>("override::text ILIKE ")
                    .bind::<Text, _>(format!("%{plaintext}%")),
            )
        }

        builder
    };

    let base_query = get_base_query();

    #[rustfmt::skip]
    let base_query = match (filter_params.sort_on.unwrap_or_default(), filter_params.sort_by.unwrap_or_default()) {
        (SortOn::Weight,         SortBy::Asc)  => base_query.order(weight.asc()),
        (SortOn::Weight,         SortBy::Desc) => base_query.order(weight.desc()),
        (SortOn::CreatedAt,      SortBy::Asc)  => base_query.order(created_at.asc()),
        (SortOn::CreatedAt,      SortBy::Desc) => base_query.order(created_at.desc()),
        (SortOn::LastModifiedAt, SortBy::Asc)  => base_query.order(last_modified_at.asc()),
        (SortOn::LastModifiedAt, SortBy::Desc) => base_query.order(last_modified_at.desc()),
    };

    let perform_in_memory_filter =
        !dimension_params.is_empty() || filter_params.prefix.is_some();

    let paginated_response = if perform_in_memory_filter {
        let mut all_contexts: Vec<Context> = base_query.load(&mut conn)?;
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

        if show_all {
            PaginatedResponse::all(eval_filter_contexts)
        } else {
            let total_items = eval_filter_contexts.len();
            let start = offset as usize;
            let end = min((offset + count) as usize, total_items);
            let data = eval_filter_contexts
                .get(start..end)
                .map(|slice| slice.to_vec())
                .unwrap_or_default();

            PaginatedResponse {
                total_pages: (total_items as f64 / count as f64).ceil() as i64,
                total_items: total_items as i64,
                data,
            }
        }
    } else if show_all {
        let data = base_query.load::<Context>(&mut conn)?;
        PaginatedResponse::all(data)
    } else {
        let total_items = get_base_query().count().get_result(&mut conn)?;

        let data = base_query
            .limit(count)
            .offset(offset)
            .load::<Context>(&mut conn)?;

        PaginatedResponse {
            total_pages: (total_items as f64 / count as f64).ceil() as i64,
            total_items,
            data,
        }
    };

    Ok(Json(paginated_response))
}

#[delete("/{ctx_id}")]
async fn delete_context_handler(
    state: Data<AppState>,
    path: Path<String>,
    custom_headers: CustomHeaders,
    user: User,
    schema_name: SchemaName,
    mut db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    use superposition_types::database::schema::contexts::dsl::{
        contexts as contexts_table, id as context_id,
    };
    let ctx_id = path.into_inner();
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let version_id =
        db_conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            contexts_table
                .filter(context_id.eq(ctx_id.clone()))
                .schema_name(&schema_name)
                .first::<Context>(transaction_conn)?;
            operations::delete(ctx_id.clone(), &user, transaction_conn, &schema_name)?;
            let config_version_desc =
                Description::try_from(format!("Deleted context by {}", user.username))
                    .map_err(|e| unexpected_error!(e))?;
            let version_id = add_config_version(
                &state,
                tags,
                config_version_desc,
                transaction_conn,
                &schema_name,
            )?;
            Ok(version_id)
        })?;

    #[cfg(feature = "high-performance-mode")]
    {
        let DbConnection(mut conn) = db_conn;
        put_config_in_redis(version_id, state, &schema_name, &mut conn).await?;
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
    req: Either<Json<Vec<ContextAction>>, Json<BulkOperation>>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    use contexts::dsl::contexts;
    let DbConnection(mut conn) = db_conn;
    let mut is_v2 = false;
    let ops = match req {
        Either::Left(o) => o.into_inner(),
        Either::Right(bo) => {
            is_v2 = true;
            bo.into_inner().operations
        }
    };
    // Marking immutable.
    let is_v2 = is_v2;
    let mut all_descriptions = Vec::new();
    let mut all_change_reasons = Vec::new();

    let tags = parse_config_tags(custom_headers.config_tags)?;
    let (response, version_id) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut response = Vec::<ContextBulkResponse>::new();
            for action in ops.into_iter() {
                match action {
                    ContextAction::Put(put_req) => {
                        let ctx_condition = put_req.context.to_owned().into_inner();
                        let ctx_condition_value =
                            Value::Object(ctx_condition.clone().into());

                        let description = if put_req.description.is_none() {
                            query_description(
                                ctx_condition_value,
                                transaction_conn,
                                &schema_name,
                            )?
                        } else {
                            put_req
                                .description
                                .clone()
                                .expect("Description should not be empty")
                        };

                        let put_resp = operations::upsert(
                            put_req.clone(),
                            description.clone(),
                            transaction_conn,
                            true,
                            &user,
                            &schema_name,
                            false,
                        )
                        .map_err(|err| {
                            log::error!(
                                "Failed at insert into contexts due to {:?}",
                                err
                            );
                            err
                        })?;

                        all_descriptions.push(description);
                        all_change_reasons.push(put_req.change_reason.clone());
                        response.push(ContextBulkResponse::Put(put_resp));
                    }
                    ContextAction::Replace(update_request) => {
                        all_change_reasons.push(update_request.change_reason.clone());
                        let update_resp = operations::update(
                            update_request,
                            transaction_conn,
                            &user,
                            &schema_name,
                        )
                        .map_err(|err| {
                            log::error!(
                                "Failed at update into contexts due to {:?}",
                                err
                            );
                            err
                        })?;

                        response.push(ContextBulkResponse::Replace(update_resp));
                    }
                    ContextAction::Delete(ctx_id) => {
                        let context: Context = contexts
                            .filter(id.eq(&ctx_id))
                            .schema_name(&schema_name)
                            .first::<Context>(transaction_conn)?;

                        let deleted_row = delete(contexts)
                            .filter(id.eq(&ctx_id))
                            .schema_name(&schema_name)
                            .execute(transaction_conn);

                        let description = context.description;

                        let email: String = user.clone().get_email();
                        let change_reason = ChangeReason::try_from(format!(
                            "Context deleted by {}",
                            email.clone()
                        ))
                        .map_err(|e| unexpected_error!(e))?;
                        all_descriptions.push(description.clone());
                        all_change_reasons.push(change_reason);

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
                        let description = match move_req.description.clone() {
                            Some(val) => val,
                            None => query_description(
                                Value::Object(
                                    move_req.context.clone().into_inner().into(),
                                ),
                                transaction_conn,
                                &schema_name,
                            )?,
                        };

                        let move_context_resp = operations::r#move(
                            old_ctx_id,
                            Json(move_req),
                            description,
                            transaction_conn,
                            true,
                            &user,
                            &schema_name,
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

            let version_id = add_config_version(
                &state,
                tags,
                Description::try_from_change_reasons(all_change_reasons)
                    .unwrap_or_default(),
                transaction_conn,
                &schema_name,
            )?;
            Ok((response, version_id))
        })?;
    let mut resp_builder = HttpResponse::Ok();
    resp_builder.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));

    // Commit the transaction
    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(version_id, state, &schema_name, &mut conn).await?;

    let http_resp = if is_v2 {
        resp_builder.json(BulkOperationResponse { output: response })
    } else {
        resp_builder.json(response)
    };
    Ok(http_resp)
}

#[put("/weight/recompute")]
async fn weight_recompute(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    schema_name: SchemaName,
    user: User,
) -> superposition::Result<HttpResponse> {
    use superposition_types::database::schema::contexts::dsl::{
        contexts, last_modified_at, last_modified_by, weight,
    };
    let DbConnection(mut conn) = db_conn;

    let result: Vec<Context> = contexts
        .schema_name(&schema_name)
        .load(&mut conn)
        .map_err(|err| {
            log::error!("failed to fetch contexts with error: {}", err);
            unexpected_error!("Something went wrong")
        })?;

    let dimension_data = get_dimension_data(&mut conn, &schema_name)?;
    let dimension_data_map = get_dimension_data_map(&dimension_data)?;
    let mut response: Vec<WeightRecomputeResponse> = vec![];
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let contexts_new_weight = result
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
                    log::error!("failed to calculate context weight: {}", e);
                    Err(unexpected_error!("Something went wrong"))
                }
            }
        })
        .collect::<superposition::Result<Vec<(BigDecimal, String)>>>()?;

    // Update database and add config version
    let last_modified_time = Utc::now();
    let config_version_id =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            for (context_weight, context_id) in contexts_new_weight.clone() {
                diesel::update(contexts.filter(id.eq(context_id)))
                    .set((
                        weight.eq(context_weight),
                        last_modified_at.eq(last_modified_time),
                        last_modified_by.eq(user.get_email())
                    ))
                    .schema_name(&schema_name)
                    .returning(Context::as_returning())
                    .execute(transaction_conn).map_err(|err| {
                        log::error!(
                            "Failed to execute query while recomputing weight, error: {err}"
                        );
                        db_error!(err)
                    })?;
            }
            let config_version_desc = Description::try_from("Recomputed weight".to_string()).map_err(|e| unexpected_error!(e))?;
            let version_id = add_config_version(&state, tags, config_version_desc, transaction_conn, &schema_name)?;
            Ok(version_id)
        })?;
    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(config_version_id, state, &schema_name, &mut conn).await?;

    let mut http_resp = HttpResponse::Ok();
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version_id.to_string(),
    ));
    Ok(http_resp.json(ListResponse::new(response)))
}

#[post("/validate")]
async fn validate_context(
    db_conn: DbConnection,
    schema_name: SchemaName,
    request: Json<ContextValidationRequest>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let ctx_condition = request.context.to_owned().into_inner();
    log::debug!("Context {:?} is being checked for validity", ctx_condition);
    validate_ctx(&mut conn, &schema_name, ctx_condition.clone())?;
    log::debug!("Context {:?} is valid", ctx_condition);
    Ok(HttpResponse::Ok().finish())
}
