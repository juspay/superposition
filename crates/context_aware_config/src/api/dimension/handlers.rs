use actix_web::{
    HttpResponse, Scope, delete, get, post, routes,
    web::{self, Data, Json, Path, Query},
};
use chrono::Utc;
use diesel::{
    Connection, ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl,
    SelectableHelper,
};
use service_utils::{
    helpers::{WebhookData, execute_webhook_call, parse_config_tags},
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, WorkspaceContext,
    },
};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::{bad_argument, not_found, unexpected_error};
use superposition_types::{
    PaginatedResponse, Resource, User,
    api::{
        dimension::{
            CreateRequest, DeleteRequest, DimensionName, DimensionResponse, UpdateRequest,
        },
        webhook::Action,
    },
    custom_query::PaginationParams,
    database::{
        models::{
            Description,
            cac::{Dimension, DimensionType},
            others::WebhookEvent,
        },
        schema::dimensions::{self, dsl::*},
    },
    result as superposition,
};

use crate::helpers::put_config_in_redis;
use crate::{
    api::dimension::{
        operations::{
            create_dimension_entry, is_mandatory_dimension, update_dimension_entry,
        },
        utils::{get_dimension_usage_context_ids, remove_connections_with_dependents},
    },
    helpers::{add_config_version, validate_change_reason},
};

declare_resource!(Dimension);

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_handler)
        .service(update_handler)
        .service(get_handler)
        .service(list_handler)
        .service(delete_handler)
}

#[authorized]
#[post("")]
async fn create_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    req: web::Json<CreateRequest>,
    user: User,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let create_req = req.into_inner();
    let create_change_reason = create_req.change_reason.clone();
    let tags = parse_config_tags(custom_headers.config_tags)?;

    validate_change_reason(
        &workspace_context,
        &create_req.change_reason,
        &mut conn,
        &state.master_encryption_key,
    )
    .await?;

    let (inserted_dimension, is_mandatory, config_version) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let inserted_dimension = create_dimension_entry(
                transaction_conn,
                &workspace_context,
                &user,
                create_req,
            )?;
            let is_mandatory =
                is_mandatory_dimension(&workspace_context, &inserted_dimension.dimension);

            let config_version = add_config_version(
                &state,
                tags,
                create_change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok((inserted_dimension, is_mandatory, config_version))
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        &mut conn,
    )
    .await;

    let data = WebhookData {
        payload: &inserted_dimension,
        resource: Resource::Dimension,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Create,
    };

    let webhook_status =
        execute_webhook_call(data, &workspace_context, &state, &mut conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Created()
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
    Ok(http_resp.json(DimensionResponse::new(inserted_dimension, is_mandatory)))
}

#[authorized]
#[get("/{name}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    req: Path<String>,
) -> superposition::Result<Json<DimensionResponse>> {
    let DbConnection(mut conn) = db_conn;

    let result: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(req.into_inner()))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Dimension>(&mut conn)?;

    let is_mandatory = workspace_context
        .settings
        .mandatory_dimensions
        .unwrap_or_default()
        .contains(&result.dimension);

    Ok(Json(DimensionResponse::new(result, is_mandatory)))
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[routes]
#[put("/{name}")]
#[patch("/{name}")]
async fn update_handler(
    workspace_context: WorkspaceContext,
    path: Path<DimensionName>,
    state: Data<AppState>,
    req: web::Json<UpdateRequest>,
    user: User,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let name: String = path.clone().into();
    let DbConnection(mut conn) = db_conn;
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let update_req = req.into_inner();

    validate_change_reason(
        &workspace_context,
        &update_req.change_reason,
        &mut conn,
        &state.master_encryption_key,
    )
    .await?;

    let update_change_reason = update_req.change_reason.clone();

    let (result, is_mandatory, config_version) = conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let result = update_dimension_entry(
                transaction_conn,
                &workspace_context,
                &user,
                name.clone(),
                update_req.clone(),
            )?;

            let is_mandatory = is_mandatory_dimension(&workspace_context, &result.dimension);

            let config_version = add_config_version(
                &state,
                tags,
                update_change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;

            Ok((result, is_mandatory, config_version))
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        &mut conn,
    )
    .await;

    let data = WebhookData {
        payload: &result,
        resource: Resource::Dimension,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Update,
    };

    let webhook_status =
        execute_webhook_call(data, &workspace_context, &state, &mut conn).await;

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
    Ok(http_resp.json(DimensionResponse::new(result, is_mandatory)))
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<DimensionResponse>>> {
    let DbConnection(mut conn) = db_conn;

    let (total_pages, total_items, result) = match filters.all {
        Some(true) => {
            let result: Vec<Dimension> = dimensions
                .schema_name(&workspace_context.schema_name)
                .get_results(&mut conn)?;
            (1, result.len() as i64, result)
        }
        _ => {
            let n_dimensions: i64 = dimensions
                .count()
                .schema_name(&workspace_context.schema_name)
                .get_result(&mut conn)?;
            let limit = filters.count.unwrap_or(10);
            let mut builder = dimensions
                .schema_name(&workspace_context.schema_name)
                .order(created_at.desc())
                .limit(limit)
                .into_boxed();
            if let Some(page) = filters.page {
                let offset = (page - 1) * limit;
                builder = builder.offset(offset);
            }
            let result: Vec<Dimension> = builder.load(&mut conn)?;
            let total_pages = (n_dimensions as f64 / limit as f64).ceil() as i64;
            (total_pages, n_dimensions, result)
        }
    };

    let mandatory_dimensions = workspace_context
        .settings
        .mandatory_dimensions
        .unwrap_or_default();

    let dimensions_with_mandatory: Vec<DimensionResponse> = result
        .into_iter()
        .map(|ele| {
            let is_mandatory = mandatory_dimensions.contains(&ele.dimension);
            DimensionResponse::new(ele, is_mandatory)
        })
        .collect();

    Ok(Json(PaginatedResponse {
        total_pages,
        total_items,
        data: dimensions_with_mandatory,
    }))
}

#[authorized]
#[delete("/{name}")]
async fn delete_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    path: Path<DeleteRequest>,
    user: User,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let name: String = path.into_inner().into();
    let DbConnection(mut conn) = db_conn;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(&name))
        .select(Dimension::as_select())
        .schema_name(&workspace_context.schema_name)
        .get_result(&mut conn)?;

    let is_mandatory = workspace_context
        .settings
        .mandatory_dimensions
        .as_ref()
        .is_some_and(|dims| dims.contains(&dimension_data.dimension));

    if is_mandatory {
        return Err(bad_argument!(
            "Dimension `{}` is mandatory and cannot be deleted",
            name
        ));
    }

    let context_ids = get_dimension_usage_context_ids(
        &name,
        &mut conn,
        &workspace_context.schema_name,
    )?;

    if context_ids.is_empty() {
        let (config_version, dimension_data) = conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            use dimensions::dsl;

            if !dimension_data.dependency_graph.is_empty() {
                return Err(bad_argument!("The dimension {} currently has other dimensions that are using it in their cohort definitions. To delete this dimension, you need to delete these cohorts", &dimension_data.dimension))
            }

            match dimension_data.dimension_type {
                DimensionType::LocalCohort(ref cohort_based_on)
                | DimensionType::RemoteCohort(ref cohort_based_on) => {
                    // Remove dependency graphs of all dimensions that
                    // depend on the cohort_based_on dimension as well as
                    // the cohorted dimension itself
                    remove_connections_with_dependents(
                        &dimension_data.dimension,
                        cohort_based_on,
                        &user.get_email(),
                        &workspace_context.schema_name,
                        transaction_conn,
                    )?
                }
                DimensionType::Regular{} => (),
            }
            diesel::update(dsl::dimensions)
                .filter(dsl::dimension.eq(&name))
                .set((
                    dsl::last_modified_at.eq(Utc::now()),
                    dsl::last_modified_by.eq(user.get_email()),
                ))
                .returning(Dimension::as_returning())
                .schema_name(&workspace_context.schema_name)
                .execute(transaction_conn)?;

            let deleted_row = diesel::delete(dsl::dimensions.filter(dsl::dimension.eq(&name)))
                .schema_name(&workspace_context.schema_name)
                .get_result::<Dimension>(transaction_conn)
                .optional()?;

            diesel::update(dimensions::dsl::dimensions)
                .filter(dimensions::position.gt(dimension_data.position))
                .set(dimensions::position.eq(dimensions::position - 1))
                .returning(Dimension::as_returning())
                .schema_name(&workspace_context.schema_name)
                .execute(transaction_conn)?;

            match deleted_row {
                None => Err(not_found!("Dimension `{}` doesn't exists", name))?,
                Some(dimension_data) => {
                    let config_version_desc = Description::try_from(format!(
                        "Dimension Deleted by {}",
                        user.get_email()
                    ))
                    .map_err(|e| unexpected_error!(e))?;
                    let config_version = add_config_version(
                        &state,
                        tags,
                        config_version_desc,
                        transaction_conn,
                        &workspace_context.schema_name,
                    )?;
                    log::info!(
                        "Dimension: {name} deleted by {}",
                        user.get_email()
                    );
                    Ok((config_version, dimension_data))
                }
            }
        })?;

        let _ = put_config_in_redis(
            &config_version,
            &state,
            &workspace_context.schema_name,
            &mut conn,
        )
        .await;
        let data = WebhookData {
            payload: &dimension_data,
            resource: Resource::Dimension,
            event: WebhookEvent::ConfigChanged,
            config_version_opt: Some(config_version.id.to_string()),
            action: Action::Delete,
        };

        let webhook_status =
            execute_webhook_call(data, &workspace_context, &state, &mut conn).await;

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

        Ok(http_resp.finish())
    } else {
        Err(bad_argument!(
            "Given key already in use in contexts: {}",
            context_ids.join(",")
        ))
    }
}
