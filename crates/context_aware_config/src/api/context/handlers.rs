use std::{cmp::min, collections::HashMap, collections::HashSet};

use actix_web::{
    Either, HttpResponse, Scope, delete, get, post, put, routes,
    web::{Data, Json, Path},
};
use bigdecimal::BigDecimal;
use chrono::Utc;
use diesel::{
    Connection, ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl,
    SelectableHelper,
    dsl::sql,
    sql_types::{Bool, Text},
};
use serde_json::{Map, Value};
use service_utils::{
    helpers::{
        WebhookData, fetch_dimensions_info_map, notify_change, parse_config_tags,
    },
    middlewares::auth_z::{Action as AuthZAction, AuthZ},
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, SchemaName, WorkspaceContext,
    },
};
use superposition_core::helpers::{calculate_context_weight, hash};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    Contextual, DBConnection, DimensionInfo, InternalUserContext, ListResponse,
    Overridden, Overrides, PaginatedResponse, Resource, SortBy, User,
    api::{
        DimensionMatchStrategy,
        context::{
            BulkOperation, BulkOperationResponse, ContextAction, ContextBulkResponse,
            ContextListFilters, ContextValidationRequest, Identifier, MoveRequest,
            PutRequest, SortOn, UpdateRequest, WeightRecomputeResponse,
        },
        webhook::Action,
    },
    custom_query::{
        self as superposition_query, CustomQuery, DimensionQuery, PaginationParams,
        QueryMap,
    },
    database::{
        models::{ChangeReason, Description, cac::Context, others::WebhookEvent},
        schema::contexts::{self, dsl, id},
    },
    logic::evaluate_local_cohorts_skip_unresolved,
    result::{self as superposition, AppError},
};

use crate::{
    api::context::{
        helpers::{
            changed_keys, create_ctx_from_put_req, query_description, validate_ctx,
            validate_override_with_functions,
        },
        operations,
    },
    helpers::{add_config_version, put_config_in_redis, validate_change_reason},
};

declare_resource!(Context);

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_handler)
        .service(update_handler)
        .service(move_handler)
        .service(delete_handler)
        .service(bulk_operations_handler)
        .service(list_handler)
        .service(get_from_condition_handler)
        .service(get_handler)
        .service(weight_recompute_handler)
        .service(validate_handler)
}

async fn create_authorized<A: AuthZAction>(
    auth_z: &AuthZ<A>,
    override_map: &Overrides,
) -> superposition::Result<()> {
    let keys = override_map.keys().collect::<Vec<_>>();
    auth_z
        .action_authorized(&AuthZActionCreate::get(), &keys)
        .await
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[put("")]
async fn create_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: Json<PutRequest>,
    mut db_conn: DbConnection,
    user: User,
    internal_user: InternalUserContext,
) -> superposition::Result<HttpResponse> {
    let req = req.into_inner();
    create_authorized(&_auth_z, &req.r#override).await?;

    let tags = parse_config_tags(custom_headers.config_tags)?;
    let description = match req.description.clone() {
        Some(val) => val,
        None => {
            // TODO: get rid of `query_description` function altogether
            let resp = query_description(
                Value::Object(req.context.clone().into_inner().into()),
                &mut db_conn,
                &workspace_context.schema_name,
            );
            match resp {
                Err(AppError::DbError(diesel::result::Error::NotFound)) => {
                    return Err(bad_argument!(
                        "Description is required when context does not exist"
                    ));
                }
                Ok(desc) => desc,
                Err(e) => return Err(e),
            }
        }
    };
    let req_change_reason = req.change_reason.clone();

    validate_change_reason(
        &workspace_context,
        &req_change_reason,
        &mut db_conn,
        &state.master_encryption_key,
    )
    .await?;

    let new_ctx = create_ctx_from_put_req(
        req,
        description,
        &mut db_conn,
        &user,
        &workspace_context,
        &state.master_encryption_key,
        &internal_user,
    )
    .await?;

    let (put_response, config_version) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let put_response = operations::upsert(
                transaction_conn,
                true,
                &user,
                &workspace_context,
                false,
                new_ctx,
            )
            .map_err(|err: superposition::AppError| {
                log::error!("context put failed with error: {:?}", err);
                err
            })?;

            let config_version = add_config_version(
                &state,
                tags,
                req_change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok((put_response, config_version))
        })?;

    let DbConnection(mut conn) = db_conn;
    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        &mut conn,
    )
    .await;

    let data = WebhookData {
        payload: &put_response,
        resource: Resource::Context,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Create,
    };

    let webhook_status =
        notify_change(data, &workspace_context, &state, &mut conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));

    Ok(http_resp.json(put_response))
}

async fn update_authorized<A: AuthZAction>(
    auth_z: &AuthZ<A>,
    context: &Identifier,
    new_overrides: &Overrides,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    let overrides =
        operations::get_overrides_from_identifier(context, schema_name, conn)?;

    auth_z
        .action_authorized(
            &AuthZActionUpdate::get(),
            &changed_keys(&overrides, new_overrides),
        )
        .await
}

#[authorized]
#[routes]
#[put("/overrides")]
#[patch("/overrides")]
async fn update_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: Json<UpdateRequest>,
    mut db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let req_change_reason = req.change_reason.clone();

    update_authorized(
        &_auth_z,
        &req.context,
        &req.override_,
        &workspace_context.schema_name,
        &mut db_conn,
    )
    .await?;

    validate_change_reason(
        &workspace_context,
        &req_change_reason,
        &mut db_conn,
        &state.master_encryption_key,
    )
    .await?;

    let DbConnection(mut conn) = db_conn;

    let (context_id, context) = match &req.context {
        Identifier::Context(context) => {
            let ctx_value: Map<String, Value> = context.clone().into_inner().into();
            (hash(&Value::Object(ctx_value.clone())), ctx_value)
        }
        Identifier::Id(i) => {
            let ctx_value: Context = dsl::contexts
                .filter(dsl::id.eq(i.clone()))
                .schema_name(&workspace_context.schema_name)
                .get_result::<Context>(&mut conn)?;
            (i.clone(), ctx_value.value.into())
        }
    };
    let r_override = req.override_.clone().into_inner();

    validate_override_with_functions(
        &workspace_context,
        &mut conn,
        &r_override,
        &context,
        &state.master_encryption_key,
    )
    .await?;

    let (override_resp, config_version) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let override_resp = operations::update(
                &workspace_context,
                req.into_inner(),
                transaction_conn,
                &user,
                context_id,
            )
            .map_err(|err: superposition::AppError| {
                log::error!("context update failed with error: {:?}", err);
                err
            })?;

            let config_version = add_config_version(
                &state,
                tags,
                req_change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok((override_resp, config_version))
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        &mut conn,
    )
    .await;

    let data = WebhookData {
        payload: &override_resp,
        resource: Resource::Context,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Update,
    };

    let webhook_status =
        notify_change(data, &workspace_context, &state, &mut conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));

    Ok(http_resp.json(override_resp))
}

async fn move_authorized<A: AuthZAction>(
    auth_z: &AuthZ<A>,
    ctx_id: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    let overrides = operations::get_overrides_from_ctx_id(ctx_id, schema_name, conn)?;
    auth_z
        .action_authorized(
            &AuthZActionMove::get(),
            &overrides.keys().collect::<Vec<_>>(),
        )
        .await
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[put("/move/{ctx_id}")]
async fn move_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    path: Path<String>,
    custom_headers: CustomHeaders,
    req: Json<MoveRequest>,
    mut db_conn: DbConnection,
    user: User,
    internal_user: InternalUserContext,
) -> superposition::Result<HttpResponse> {
    let ctx_id = path.into_inner();
    move_authorized(
        &_auth_z,
        &ctx_id,
        &workspace_context.schema_name,
        &mut db_conn,
    )
    .await?;

    let tags = parse_config_tags(custom_headers.config_tags)?;

    let description = match req.description.clone() {
        Some(val) => val,
        None => {
            // TODO: get rid of `query_description` function altogether
            let resp = query_description(
                Value::Object(req.context.clone().into_inner().into()),
                &mut db_conn,
                &workspace_context.schema_name,
            );
            match resp {
                Err(AppError::DbError(diesel::result::Error::NotFound)) => {
                    return Err(bad_argument!(
                        "Description is required when context does not exist"
                    ));
                }
                Ok(desc) => desc,
                Err(e) => return Err(e),
            }
        }
    };

    validate_change_reason(
        &workspace_context,
        &req.change_reason,
        &mut db_conn,
        &state.master_encryption_key,
    )
    .await?;

    let DbConnection(mut conn) = db_conn;

    let ctx_condition = req.context.clone().into_inner();

    let dimension_data_map = validate_ctx(
        &mut conn,
        &workspace_context,
        ctx_condition,
        Overrides::default(),
        &state.master_encryption_key,
        &internal_user,
    )
    .await?;

    let (move_response, config_version) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let move_response = operations::r#move(
                &workspace_context,
                ctx_id,
                req,
                description,
                transaction_conn,
                true,
                &user,
                &dimension_data_map,
            )
            .map_err(|err| {
                log::error!("move api failed with error: {:?}", err);
                err
            })?;
            let config_version = add_config_version(
                &state,
                tags,
                move_response.context.change_reason.clone().into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;

            Ok((move_response, config_version))
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        &mut conn,
    )
    .await;

    let data = WebhookData {
        payload: vec![&move_response.deleted_context, &move_response.context],
        resource: Resource::Context,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Batch(vec![Action::Delete, move_response.action]),
    };

    let webhook_status =
        notify_change(data, &workspace_context, &state, &mut conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));

    Ok(http_resp.json(move_response.context))
}

#[authorized]
#[post("/get")]
async fn get_from_condition_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    req: Json<Map<String, Value>>,
) -> superposition::Result<Json<Context>> {
    use superposition_types::database::schema::contexts::dsl::*;

    let context_id = hash(&Value::Object(req.into_inner()));
    let DbConnection(mut conn) = db_conn;

    let ctx: Context = contexts
        .filter(id.eq(context_id))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Context>(&mut conn)?;

    Ok(Json(ctx))
}

#[authorized]
#[get("/{ctx_id}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    path: Path<String>,
    db_conn: DbConnection,
) -> superposition::Result<Json<Context>> {
    use superposition_types::database::schema::contexts::dsl::*;

    let ctx_id = path.into_inner();
    let DbConnection(mut conn) = db_conn;

    let ctx: Context = contexts
        .filter(id.eq(ctx_id))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Context>(&mut conn)?;

    Ok(Json(ctx))
}

#[authorized]
#[routes]
#[get("/list")]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    filter_params: superposition_query::Query<ContextListFilters>,
    pagination_params: superposition_query::Query<PaginationParams>,
    dimension_params: DimensionQuery<QueryMap>,
    db_conn: DbConnection,
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
        let mut builder = contexts
            .schema_name(&workspace_context.schema_name)
            .into_boxed();
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

    let base_query = match (
        filter_params.sort_on.unwrap_or_default(),
        filter_params.sort_by.unwrap_or_default(),
    ) {
        (SortOn::Weight, SortBy::Asc) => {
            base_query.order((weight.asc(), created_at.asc()))
        }
        (SortOn::Weight, SortBy::Desc) => {
            base_query.order((weight.desc(), created_at.desc()))
        }
        (SortOn::CreatedAt, SortBy::Asc) => {
            base_query.order((created_at.asc(), weight.asc()))
        }
        (SortOn::CreatedAt, SortBy::Desc) => {
            base_query.order((created_at.desc(), weight.desc()))
        }
        (SortOn::LastModifiedAt, SortBy::Asc) => {
            base_query.order((last_modified_at.asc(), weight.asc()))
        }
        (SortOn::LastModifiedAt, SortBy::Desc) => {
            base_query.order((last_modified_at.desc(), weight.desc()))
        }
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
        let dimensions_info =
            fetch_dimensions_info_map(&mut conn, &workspace_context.schema_name)?;
        let original_req_keys = dimension_params.keys().collect::<Vec<_>>();
        let dimension_params =
            evaluate_local_cohorts_skip_unresolved(&dimensions_info, &dimension_params);

        let filter_fn = match filter_params.dimension_match_strategy.unwrap_or_default() {
            DimensionMatchStrategy::Exact => Context::filter_exact_match,
            DimensionMatchStrategy::Subset => Context::filter_by_eval,
        };

        let eval_filter_contexts = filter_fn(all_contexts, &dimension_params);

        let eval_filter_contexts = Context::filter_by_dimension(
            eval_filter_contexts,
            &original_req_keys,
            &dimensions_info,
        );

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

async fn delete_authorized<A: AuthZAction>(
    auth_z: &AuthZ<A>,
    ctx_id: &str,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    let overrides = operations::get_overrides_from_ctx_id(ctx_id, schema_name, conn)?;
    auth_z
        .action_authorized(
            &AuthZActionDelete::get(),
            &overrides.keys().collect::<Vec<_>>(),
        )
        .await
}

#[authorized]
#[delete("/{ctx_id}")]
async fn delete_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    path: Path<String>,
    custom_headers: CustomHeaders,
    user: User,
    mut db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    use superposition_types::database::schema::contexts::dsl::{
        contexts as contexts_table, id as context_id,
    };
    let ctx_id = path.into_inner();
    delete_authorized(
        &_auth_z,
        &ctx_id,
        &workspace_context.schema_name,
        &mut db_conn,
    )
    .await?;

    let tags = parse_config_tags(custom_headers.config_tags)?;
    let (config_version, deleted_ctx) = db_conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            contexts_table
                .filter(context_id.eq(ctx_id.clone()))
                .schema_name(&workspace_context.schema_name)
                .first::<Context>(transaction_conn)?;
            let deleted_ctx = operations::delete(
                ctx_id.clone(),
                &user,
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            let config_version_desc =
                Description::try_from(format!("Deleted context by {}", user.get_email()))
                    .map_err(|e| unexpected_error!(e))?;
            let config_version = add_config_version(
                &state,
                tags,
                config_version_desc,
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok((config_version, deleted_ctx))
        })?;

    let DbConnection(mut conn) = db_conn;
    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        &mut conn,
    )
    .await;
    let data = WebhookData {
        payload: &deleted_ctx,
        resource: Resource::Context,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Delete,
    };

    let webhook_status =
        notify_change(data, &workspace_context, &state, &mut conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::NoContent()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };

    Ok(http_resp
        .insert_header((
            AppHeader::XConfigVersion.to_string().as_str(),
            config_version.id.to_string().as_str(),
        ))
        .finish())
}

async fn bulk_authorized<A: AuthZAction>(
    auth_z: &AuthZ<A>,
    operations: &Vec<ContextAction>,
    schema_name: &SchemaName,
    conn: &mut DBConnection,
) -> superposition::Result<()> {
    for op in operations {
        match op {
            ContextAction::Put(put_req) => {
                create_authorized(auth_z, &put_req.r#override).await?;
            }
            ContextAction::Replace(update_req) => {
                update_authorized(
                    auth_z,
                    &update_req.context,
                    &update_req.override_,
                    schema_name,
                    conn,
                )
                .await?;
            }
            ContextAction::Delete(ctx_id) => {
                delete_authorized(auth_z, ctx_id, schema_name, conn).await?;
            }
            ContextAction::Move { id: ctx_id, .. } => {
                move_authorized(auth_z, ctx_id, schema_name, conn).await?;
            }
        }
    }
    Ok(())
}

enum PreparedOperation {
    Put {
        new_ctx: Context,
        change_reason: ChangeReason,
    },
    Replace {
        update_request: UpdateRequest,
        context_id: String,
        change_reason: ChangeReason,
    },
    Delete {
        ctx_id: String,
    },
    Move {
        old_ctx_id: String,
        move_req: MoveRequest,
        description: Description,
        dimension_data_map: HashMap<String, DimensionInfo>,
    },
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[put("/bulk-operations")]
async fn bulk_operations_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: Either<Json<Vec<ContextAction>>, Json<BulkOperation>>,
    db_conn: DbConnection,
    user: User,
    internal_user: InternalUserContext,
) -> superposition::Result<HttpResponse> {
    use contexts::dsl::contexts;

    let DbConnection(mut conn) = db_conn;
    let is_v2 = matches!(req, Either::Right(_));
    let ops = match req {
        Either::Left(o) => o.into_inner(),
        Either::Right(bo) => bo.into_inner().operations,
    };
    bulk_authorized(&_auth_z, &ops, &workspace_context.schema_name, &mut conn).await?;

    let mut webhook_actions: Vec<Action> = Vec::new();
    let mut webhook_contexts: Vec<Context> = Vec::new();

    let tags = parse_config_tags(custom_headers.config_tags)?;
    // ── Phase 1: async validation & preparation ──
    let mut prepared_ops =
        Vec::with_capacity(if ops.len() > 100 { 100 } else { ops.len() });

    for action in ops.into_iter() {
        match action {
            ContextAction::Put(put_req) => {
                let ctx_condition = put_req.context.to_owned().into_inner();
                let ctx_condition_value = Value::Object(ctx_condition.clone().into());

                validate_change_reason(
                    &workspace_context,
                    &put_req.change_reason,
                    &mut conn,
                    &state.master_encryption_key,
                )
                .await?;

                let description = match put_req.description.clone() {
                    Some(val) => val,
                    None => query_description(
                        ctx_condition_value,
                        &mut conn,
                        &workspace_context.schema_name,
                    )?,
                };
                let change_reason = put_req.change_reason.clone();
                let new_ctx = create_ctx_from_put_req(
                    put_req,
                    description.clone(),
                    &mut conn,
                    &user,
                    &workspace_context,
                    &state.master_encryption_key,
                    &internal_user,
                )
                .await?;

                prepared_ops.push(PreparedOperation::Put {
                    new_ctx,
                    change_reason,
                });
            }
            ContextAction::Replace(update_request) => {
                let change_reason = update_request.change_reason.clone();
                let (context_id, context) = match &update_request.context {
                    Identifier::Context(context) => {
                        let ctx_value: Map<String, Value> =
                            context.clone().into_inner().into();
                        (hash(&Value::Object(ctx_value.clone())), ctx_value)
                    }
                    Identifier::Id(i) => {
                        let ctx_value: Context = dsl::contexts
                            .filter(dsl::id.eq(i.clone()))
                            .schema_name(&workspace_context.schema_name)
                            .get_result::<Context>(&mut conn)?;
                        (i.clone(), ctx_value.value.into())
                    }
                };
                let r_override = update_request.override_.clone().into_inner();

                validate_override_with_functions(
                    &workspace_context,
                    &mut conn,
                    &r_override,
                    &context,
                    &state.master_encryption_key,
                )
                .await?;

                prepared_ops.push(PreparedOperation::Replace {
                    update_request,
                    context_id,
                    change_reason,
                });
            }
            ContextAction::Delete(ctx_id) => {
                prepared_ops.push(PreparedOperation::Delete { ctx_id });
            }
            ContextAction::Move {
                id: old_ctx_id,
                request: move_req,
            } => {
                let description = match move_req.description.clone() {
                    Some(val) => val,
                    None => query_description(
                        Value::Object(move_req.context.clone().into_inner().into()),
                        &mut conn,
                        &workspace_context.schema_name,
                    )?,
                };

                let ctx_condition = move_req.context.clone().into_inner();

                let dimension_data_map = validate_ctx(
                    &mut conn,
                    &workspace_context,
                    ctx_condition,
                    Overrides::default(),
                    &state.master_encryption_key,
                    &internal_user,
                )
                .await?;

                prepared_ops.push(PreparedOperation::Move {
                    old_ctx_id,
                    move_req,
                    description,
                    dimension_data_map,
                });
            }
        }
    }

    // ── Phase 2: single transaction for all DB writes ──
    let (response, config_version) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut response = Vec::<ContextBulkResponse>::new();
            let mut all_change_reasons = Vec::new();

            for prepared in prepared_ops {
                match prepared {
                    PreparedOperation::Put {
                        new_ctx,
                        change_reason,
                    } => {
                        let put_resp = operations::upsert(
                            transaction_conn,
                            true,
                            &user,
                            &workspace_context,
                            false,
                            new_ctx,
                        )
                        .map_err(|err| {
                            log::error!(
                                "Failed at insert into contexts due to {:?}",
                                err
                            );
                            err
                        })?;

                        all_change_reasons.push(change_reason);
                        webhook_contexts.push(put_resp.clone());
                        webhook_actions.push(Action::Create);
                        response.push(ContextBulkResponse::Put(put_resp));
                    }
                    PreparedOperation::Replace {
                        update_request,
                        context_id,
                        change_reason,
                    } => {
                        let update_resp = operations::update(
                            &workspace_context,
                            update_request,
                            transaction_conn,
                            &user,
                            context_id,
                        )
                        .map_err(|err| {
                            log::error!(
                                "Failed at update into contexts due to {:?}",
                                err
                            );
                            err
                        })?;
                        all_change_reasons.push(change_reason);
                        webhook_contexts.push(update_resp.clone());
                        webhook_actions.push(Action::Update);
                        response.push(ContextBulkResponse::Replace(update_resp));
                    }
                    PreparedOperation::Delete { ctx_id } => {
                        let deleted_ctx = diesel::delete(contexts)
                            .filter(id.eq(&ctx_id))
                            .schema_name(&workspace_context.schema_name)
                            .get_result::<Context>(transaction_conn)
                            .optional();

                        let change_reason = ChangeReason::try_from(format!(
                            "Context deleted by {}",
                            user.get_email()
                        ))
                        .map_err(|e| unexpected_error!(e))?;
                        all_change_reasons.push(change_reason);

                        match deleted_ctx {
                            // Any kind of error would rollback the tranction but explicitly returning rollback tranction allows you to rollback from any point in transaction.
                            Ok(None) => {
                                return Err(bad_argument!(
                                    "context with id {} not found",
                                    ctx_id
                                ));
                            }
                            Ok(Some(ctx)) => {
                                log::info!(
                                    "{ctx_id} context deleted by {}",
                                    user.get_email()
                                );
                                response.push(ContextBulkResponse::Delete(format!(
                                    "{ctx_id} deleted succesfully"
                                )));
                                webhook_contexts.push(ctx);
                                webhook_actions.push(Action::Delete);
                            }
                            Err(e) => {
                                log::error!("Delete context failed due to {:?}", e);
                                return Err(db_error!(e));
                            }
                        };
                    }
                    PreparedOperation::Move {
                        old_ctx_id,
                        move_req,
                        description,
                        dimension_data_map,
                    } => {
                        let move_context_resp = operations::r#move(
                            &workspace_context,
                            old_ctx_id,
                            Json(move_req),
                            description,
                            transaction_conn,
                            true,
                            &user,
                            &dimension_data_map,
                        )
                        .map_err(|err| {
                            log::error!(
                                "Failed at moving context reponse due to {:?}",
                                err
                            );
                            err
                        })?;
                        all_change_reasons
                            .push(move_context_resp.context.change_reason.clone());

                        webhook_contexts.extend([
                            move_context_resp.deleted_context,
                            move_context_resp.context.clone(),
                        ]);
                        webhook_actions
                            .extend([Action::Delete, move_context_resp.action]);

                        response
                            .push(ContextBulkResponse::Move(move_context_resp.context));
                    }
                }
            }

            let config_version = add_config_version(
                &state,
                tags,
                Description::try_from_change_reasons(all_change_reasons)
                    .unwrap_or_default(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok((response, config_version))
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        &mut conn,
    )
    .await;

    let data = WebhookData {
        payload: &webhook_contexts,
        resource: Resource::Context,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Batch(webhook_actions),
    };

    let webhook_status =
        notify_change(data, &workspace_context, &state, &mut conn).await;

    let mut resp_builder = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    resp_builder.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));

    let http_resp = if is_v2 {
        resp_builder.json(BulkOperationResponse { output: response })
    } else {
        resp_builder.json(response)
    };
    Ok(http_resp)
}

#[authorized]
#[put("/weight/recompute")]
async fn weight_recompute_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    use superposition_types::database::schema::contexts::dsl::{
        contexts, last_modified_at, last_modified_by, weight,
    };

    let DbConnection(mut conn) = db_conn;

    let result: Vec<Context> = contexts
        .schema_name(&workspace_context.schema_name)
        .load(&mut conn)
        .map_err(|err| {
            log::error!("failed to fetch contexts with error: {}", err);
            unexpected_error!("Something went wrong")
        })?;

    let dimension_info_map =
        fetch_dimensions_info_map(&mut conn, &workspace_context.schema_name)?;
    let mut response: Vec<WeightRecomputeResponse> = vec![];
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let contexts_new_weight = result
        .clone()
        .into_iter()
        .map(|context| {
            let new_weight =
                calculate_context_weight(&context.value, &dimension_info_map);

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
    let config_version =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            for (context_weight, context_id) in contexts_new_weight.clone() {
                diesel::update(contexts.filter(id.eq(context_id)))
                    .set((
                        weight.eq(context_weight),
                        last_modified_at.eq(last_modified_time),
                        last_modified_by.eq(user.get_email())
                    ))
                    .schema_name(&workspace_context.schema_name)
                    .returning(Context::as_returning())
                    .execute(transaction_conn).map_err(|err| {
                        log::error!(
                            "Failed to execute query while recomputing weight, error: {err}"
                        );
                        db_error!(err)
                    })?;
            }
            let config_version_desc = Description::try_from("Recomputed weight".to_string()).map_err(|e| unexpected_error!(e))?;
            add_config_version(&state, tags, config_version_desc, transaction_conn, &workspace_context.schema_name)
        })?;
    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        &mut conn,
    )
    .await;

    let data = WebhookData {
        payload: &response,
        resource: Resource::Context,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Batch(vec![Action::Update; response.len()]),
    };

    let webhook_status =
        notify_change(data, &workspace_context, &state, &mut conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));
    Ok(http_resp.json(ListResponse::new(response)))
}

#[authorized]
#[post("/validate")]
async fn validate_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    request: Json<ContextValidationRequest>,
    state: Data<AppState>,
    internal_user: InternalUserContext,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let ctx_condition = request.context.to_owned().into_inner();
    log::debug!("Context {:?} is being checked for validity", ctx_condition);

    validate_ctx(
        &mut conn,
        &workspace_context,
        ctx_condition.clone(),
        Overrides::default(),
        &state.master_encryption_key,
        &internal_user,
    )
    .await?;
    log::debug!("Context {:?} is valid", ctx_condition);
    Ok(HttpResponse::Ok().finish())
}
