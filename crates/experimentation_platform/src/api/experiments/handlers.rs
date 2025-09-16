use std::{
    cmp::min,
    collections::{HashMap, HashSet},
    vec,
};

use actix_http::header::{self};
use actix_web::{
    get, patch, post, put, route,
    web::{self, Data, Json, Path, Query},
    Either, HttpRequest, HttpResponse, HttpResponseBuilder, Scope,
};
use chrono::{DateTime, Duration, Utc};
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    BoolExpressionMethods, Connection, ExpressionMethods, PgConnection, QueryDsl,
    RunQueryDsl, SelectableHelper, TextExpressionMethods,
};
use experimentation_client::{
    get_applicable_buckets_from_group, get_applicable_variants_from_group_response,
};
use reqwest::{Method, StatusCode};
use serde_json::{Map, Value};
use service_utils::{
    helpers::{
        construct_request_headers, execute_webhook_call, generate_snowflake_id, request,
    },
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, SchemaName, WorkspaceContext,
    },
};
use superposition_macros::{bad_argument, unexpected_error};
use superposition_types::{
    api::{
        context::{
            ContextAction, ContextBulkResponse, Identifier, MoveRequest, PutRequest,
            UpdateRequest,
        },
        default_config::DefaultConfigUpdateRequest,
        experiment_groups::ExpGroupMemberRequest,
        experiments::{
            ApplicableVariantsQuery, ApplicableVariantsRequest, AuditQueryFilters,
            ConcludeExperimentRequest, ExperimentCreateRequest, ExperimentListFilters,
            ExperimentResponse, ExperimentSortOn, ExperimentStateChangeRequest,
            OverrideKeysUpdateRequest, RampRequest,
        },
        DimensionMatchStrategy,
    },
    custom_query::{
        self as superposition_query, CustomQuery, DimensionQuery, PaginationParams,
        QueryMap,
    },
    database::{
        models::{
            experimentation::{
                EventLog, Experiment, ExperimentGroup, ExperimentStatusType,
                ExperimentType, TrafficPercentage, Variant, VariantType, Variants,
            },
            others::WebhookEvent,
            ChangeReason,
        },
        schema::{
            event_log::dsl as event_log, experiment_groups::dsl as experiment_groups,
            experiments::dsl as experiments,
        },
    },
    result as superposition, Cac, Condition, Contextual, Exp, ListResponse, Overrides,
    PaginatedResponse, SortBy, User,
};

use crate::api::{
    experiment_groups::helpers::{
        add_members, create_system_generated_experiment_group,
        detach_experiment_from_group, update_experiment_group_buckets,
    },
    experiments::{
        helpers::{
            fetch_webhook_by_event, get_workspace, validate_control_overrides,
            validate_delete_experiment_variants,
        },
        types::StartedByChangeSet,
    },
};

use super::{
    cac_api::{
        construct_header_map, get_context_override,
        process_cac_bulk_operation_http_response,
    },
    helpers::{
        add_variant_dimension_to_ctx, check_variant_types,
        check_variants_override_coverage, extract_override_keys, fetch_cac_config,
        fetch_experiment, handle_experiment_group_membership, hash, validate_experiment,
        validate_override_keys,
    },
};

pub fn endpoints(scope: Scope) -> Scope {
    scope
        .service(get_audit_logs)
        .service(create)
        .service(conclude_handler)
        .service(discard_handler)
        .service(list_experiments)
        .service(get_applicable_variants)
        .service(get_experiment_handler)
        .service(ramp)
        .service(update_overrides)
        .service(pause_handler)
        .service(resume_handler)
}

fn add_config_version_to_header(
    config_version: &Option<String>,
    resp_builder: &mut HttpResponseBuilder,
) {
    if let Some(val) = config_version {
        resp_builder.insert_header((AppHeader::XConfigVersion.to_string(), val.clone()));
    }
}

#[allow(clippy::too_many_arguments)]
#[post("")]
async fn create(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: Json<ExperimentCreateRequest>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<HttpResponse> {
    use superposition_types::database::schema::experiments::dsl::experiments;
    let mut variants = req.variants.to_vec();
    let DbConnection(mut conn) = db_conn;
    let description = req.description.clone();
    let change_reason = req.change_reason.clone();

    let workspace_settings = get_workspace(&workspace_request.schema_name, &mut conn)?;

    // Checking if experiment has exactly 1 control variant, and
    // atleast 1 experimental variant
    check_variant_types(&variants)?;
    let unique_override_keys: Vec<String> =
        extract_override_keys(&variants[0].overrides.clone().into_inner())
            .into_iter()
            .collect();

    let unique_ids_of_variants_from_req: HashSet<&str> =
        HashSet::from_iter(variants.iter().map(|v| v.id.as_str()));

    if unique_ids_of_variants_from_req.len() != variants.len() {
        return Err(bad_argument!(
            "Variant ids are expected to be unique. Provide unqiue variant IDs"
        ));
    }

    // validating context
    let exp_context = req.context.clone().into_inner();
    let exp_context_id = hash(&Value::Object(exp_context.clone().into()));

    // Checking if all the variants are overriding the mentioned keys
    let variant_overrides = variants
        .iter()
        .map(|variant| variant.overrides.clone().into_inner())
        .collect::<Vec<Overrides>>();

    match req.experiment_type {
        ExperimentType::Default => {
            let are_valid_variants = check_variants_override_coverage(
                &variant_overrides,
                &unique_override_keys,
            );
            if !are_valid_variants {
                return Err(bad_argument!(
                    "all variants should contain the keys mentioned in override_keys. Check if any of the following keys [{}] are missing from keys in your variants",
                        unique_override_keys.join(",")
                    )
                );
            }

            // Validate control overrides against resolved config when auto-populate is enabled
            if workspace_settings.auto_populate_control {
                let control_variant = variants
                    .iter()
                    .find(|v| v.variant_type == VariantType::CONTROL)
                    .ok_or_else(|| {
                        log::error!(
                            "Control variant not found in existing experiment variants"
                        );
                        unexpected_error!(
                            "Control variant not found in existing experiment variants"
                        )
                    })?;

                validate_control_overrides(
                    &control_variant.overrides,
                    &exp_context,
                    &workspace_request,
                    &user,
                    &state,
                )
                .await?;
            }

            // validating experiment against other active experiments based on permission flags
            let flags = &state.experimentation_flags;
            let (valid, reason) = validate_experiment(
                &exp_context,
                &unique_override_keys,
                None,
                flags,
                &workspace_request.schema_name,
                &mut conn,
            )?;
            if !valid {
                return Err(bad_argument!(reason));
            }
        }
        ExperimentType::DeleteOverrides => {
            validate_delete_experiment_variants(
                &user,
                &state,
                &exp_context,
                &exp_context_id,
                &workspace_request,
                &variants,
            )
            .await?;
        }
    }

    // generating snowflake id for experiment
    let experiment_id = generate_snowflake_id(&state)?;

    //create overrides in CAC, if successfull then create experiment in DB
    let mut cac_operations: Vec<ContextAction> = Vec::new();
    for variant in &mut variants {
        let variant_id = experiment_id.to_string() + "-" + &variant.id;

        // updating variant.id to => experiment_id + variant.id
        variant.id = variant_id.to_string();

        let updated_cacccontext =
            add_variant_dimension_to_ctx(&exp_context, variant_id.to_string())?;

        let payload = PutRequest {
            context: updated_cacccontext
                .as_object()
                .ok_or_else(|| unexpected_error!("Failed to convert context to object"))?
                .clone()
                .try_into()
                .map_err(|e: String| unexpected_error!(e))?,
            r#override: variant.overrides.clone().into(),
            description: Some(description.clone()),
            change_reason: change_reason.clone(),
        };
        cac_operations.push(ContextAction::Put(payload));
    }

    // creating variants' context in CAC
    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/context/bulk-operations";
    let user_str = serde_json::to_string(&user).map_err(|err| {
        log::error!("Something went wrong, failed to stringify user data {err}");
        unexpected_error!(
            "Something went wrong, failed to stringify user data {}",
            err
        )
    })?;

    let extra_headers = vec![
        ("x-user", Some(user_str)),
        ("x-config-tags", custom_headers.config_tags),
    ]
    .into_iter()
    .filter_map(|(key, val)| val.map(|v| (key, v)))
    .collect::<Vec<_>>();

    let headers_map = construct_header_map(
        &workspace_request.workspace_id,
        &workspace_request.organisation_id,
        extra_headers,
    )?;

    // Step 1: Perform the HTTP request and handle errors
    let response = http_client
        .put(&url)
        .headers(headers_map.into())
        .header(
            header::AUTHORIZATION,
            format!("Internal {}", state.superposition_token),
        )
        .json(&cac_operations)
        .send()
        .await;

    // directly return an error response if not a 200 response
    let (resp_contexts, config_version_id) =
        process_cac_bulk_operation_http_response(response).await?;
    let created_contexts = resp_contexts
        .into_iter()
        .map(|item| match item {
            ContextBulkResponse::Put(context) => Ok(context),
            _ => Err(format!("Unexpected response item: {item:?}")),
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| {
            log::error!(
                "Something went wrong, failed to parse bulk operations response {err}"
            );
            unexpected_error!("Something went wrong")
        })?;

    for i in 0..created_contexts.len() {
        let created_context = &created_contexts[i];
        variants[i].context_id = Some(created_context.id.clone());
        variants[i].override_id = Some(created_context.override_id.clone());
    }

    let now = Utc::now();
    // inserting experiment in db
    let new_experiment = Experiment {
        id: experiment_id,
        created_by: user.get_email(),
        created_at: now,
        last_modified: now,
        name: req.name.to_string(),
        experiment_type: req.experiment_type,
        override_keys: unique_override_keys.to_vec(),
        traffic_percentage: TrafficPercentage::default(),
        status: ExperimentStatusType::CREATED,
        started_by: None,
        started_at: None,
        context: req.context.clone().into_inner(),
        variants: Variants::new(variants),
        last_modified_by: user.get_email(),
        chosen_variant: None,
        description,
        change_reason,
        metrics: req.metrics.clone().unwrap_or(workspace_settings.metrics),
        experiment_group_id: req.experiment_group_id,
    };

    let inserted_experiment: Experiment =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let inserted_experiment = diesel::insert_into(experiments)
                .values(&new_experiment)
                .returning(Experiment::as_returning())
                .schema_name(&workspace_request.schema_name)
                .get_result(transaction_conn)?;

            if let Some(experiment_group_id) = &req.experiment_group_id {
                add_members(
                    experiment_group_id,
                    &[inserted_experiment.clone()],
                    ExpGroupMemberRequest {
                        change_reason: ChangeReason::try_from(format!("Adding experiment {experiment_id} to the group, while creating the experiment.")).map_err(|e| unexpected_error!(e))?,
                        member_experiment_ids: vec![experiment_id],
                    },
                    transaction_conn,
                    &workspace_request.schema_name,
                    &user,
                )?;
            }

            Ok(inserted_experiment)
        })?;

    let response = ExperimentResponse::from(inserted_experiment);
    let webhook_status = if let Ok(webhook) = fetch_webhook_by_event(
        &state,
        &user,
        &WebhookEvent::ExperimentCreated,
        &workspace_request,
    )
    .await
    {
        execute_webhook_call(
            &webhook,
            &response,
            &config_version_id,
            &workspace_request,
            WebhookEvent::ExperimentCreated,
            &state,
        )
        .await
    } else {
        true
    };

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            StatusCode::from_u16(512).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    add_config_version_to_header(&config_version_id, &mut http_resp);
    Ok(http_resp.json(response))
}

#[allow(clippy::too_many_arguments)]
#[patch("/{experiment_id}/conclude")]
async fn conclude_handler(
    state: Data<AppState>,
    path: web::Path<i64>,
    custom_headers: CustomHeaders,
    req: web::Json<ConcludeExperimentRequest>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let (response, config_version_id) = conclude(
        &state,
        path.into_inner(),
        custom_headers.config_tags,
        req.into_inner(),
        &mut conn,
        &workspace_request,
        &user,
    )
    .await?;

    let experiment_response = ExperimentResponse::from(response);

    let webhook_status = if let Ok(webhook) = fetch_webhook_by_event(
        &state,
        &user,
        &WebhookEvent::ExperimentConcluded,
        &workspace_request,
    )
    .await
    {
        execute_webhook_call(
            &webhook,
            &experiment_response,
            &config_version_id,
            &workspace_request,
            WebhookEvent::ExperimentConcluded,
            &state,
        )
        .await
    } else {
        true
    };

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            StatusCode::from_u16(512).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR),
        )
    };

    add_config_version_to_header(&config_version_id, &mut http_resp);
    Ok(http_resp.json(experiment_response))
}

#[allow(clippy::too_many_arguments)]
pub async fn conclude(
    state: &Data<AppState>,
    experiment_id: i64,
    config_tags: Option<String>,
    req: ConcludeExperimentRequest,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    workspace_request: &WorkspaceContext,
    user: &User,
) -> superposition::Result<(Experiment, Option<String>)> {
    use superposition_types::database::schema::experiments::dsl;

    let change_reason = ChangeReason::try_from(format!(
        "Experiment concluded with variant id {:?}",
        req.chosen_variant
    ))
    .map_err(|err| {
        log::error!("Failed to convert change reason: {}", err);
        unexpected_error!("Failed to convert change reason")
    })?;

    let winner_variant_id: String = req.chosen_variant.to_owned();

    let experiment: Experiment = dsl::experiments
        .find(experiment_id)
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(conn)?;

    let exp_context_id = hash(&Value::Object(experiment.context.clone().into()));
    let description = match req.description.clone() {
        Some(desc) => desc,
        None => experiment.description.clone(),
    };
    if !experiment.status.concludable() {
        return Err(bad_argument!(
            "experiment with id {} is {}, and cannot be concluded",
            experiment_id,
            experiment.status
        ));
    }

    let mut operations: Vec<ContextAction> = vec![];

    let mut is_valid_winner_variant = false;
    for variant in experiment.variants.clone().into_inner() {
        let context_id = variant.context_id.ok_or_else(|| {
            log::error!("context id not available for variant {:?}", variant.id);
            unexpected_error!("Something went wrong, failed to conclude experiment")
        })?;

        if variant.id != winner_variant_id {
            operations.push(ContextAction::Delete(context_id));
            continue;
        }

        if !experiment.context.is_empty() {
            match (experiment.experiment_type, variant.variant_type) {
                (ExperimentType::Default, _) => {
                    let context_move_req = MoveRequest {
                        context: experiment
                            .context
                            .clone()
                            .try_into()
                            .map_err(|e: String| unexpected_error!(e))?,
                        description: Some(description.clone()),
                        change_reason: change_reason.clone(),
                    };
                    operations.push(ContextAction::Move((context_id, context_move_req)));
                }
                (ExperimentType::DeleteOverrides, VariantType::CONTROL) => {
                    operations.push(ContextAction::Delete(context_id));
                }
                (ExperimentType::DeleteOverrides, _) => {
                    let current_context = get_context_override(
                        user,
                        state,
                        workspace_request,
                        exp_context_id.clone(),
                    )
                    .await?;

                    let mut context_override: Map<String, Value> =
                        current_context.override_.into();
                    for key in variant.overrides.into_inner().keys() {
                        context_override.remove(key);
                    }

                    if context_override.is_empty() {
                        operations.push(ContextAction::Delete(exp_context_id.clone()));
                    } else {
                        let payload = UpdateRequest {
                            context: Identifier::Id(exp_context_id.clone()),
                            override_: Cac::<Overrides>::try_from(context_override).map_err(|err| {
                                log::error!("failed to convert variant overrides to cac override {err}");
                                bad_argument!("failed to convert variant overrides to cac override")
                            })?,
                            description: None,
                            change_reason: change_reason.clone(),
                        };
                        operations.push(ContextAction::Replace(payload));
                    }
                    operations.push(ContextAction::Delete(context_id));
                }
            }
        } else {
            let user_str = serde_json::to_string(&user).map_err(|err| {
                log::error!("Something went wrong, failed to stringify user data {err}");
                unexpected_error!(
                    "Something went wrong, failed to stringify user data {}",
                    err
                )
            })?;

            for (key, val) in variant.overrides.into_inner() {
                let update_request = DefaultConfigUpdateRequest {
                    value: Some(val),
                    change_reason: change_reason.clone(),
                    schema: None,
                    function_name: None,
                    autocomplete_function_name: None,
                    description: None,
                };

                let url = format!("{}/default-config/{}", state.cac_host, key);

                let headers = construct_request_headers(&[
                    ("x-tenant", &workspace_request.workspace_id),
                    (
                        "Authorization",
                        &format!("Internal {}", state.superposition_token),
                    ),
                    ("x-user", user_str.as_str()),
                    ("x-org-id", &workspace_request.organisation_id),
                ])
                .map_err(|err| unexpected_error!(err))?;

                let _ =
                    request::<_, Value>(url, Method::PUT, Some(update_request), headers)
                        .await
                        .map_err(|err| unexpected_error!(err))?;
            }
            operations.push(ContextAction::Delete(context_id));
        }

        is_valid_winner_variant = true;
    }

    if !is_valid_winner_variant {
        return Err(bad_argument!(
            "winner variant not found. A wrong variant id may have been sent, check and try again"
        ));
    }

    // calling CAC bulk api with operations as payload
    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/context/bulk-operations";
    let user_str = serde_json::to_string(&user).map_err(|err| {
        log::error!("Something went wrong, failed to stringify user data {err}");
        unexpected_error!(
            "Something went wrong, failed to stringify user data {}",
            err
        )
    })?;
    let extra_headers = vec![("x-user", Some(user_str)), ("x-config-tags", config_tags)]
        .into_iter()
        .filter_map(|(key, val)| val.map(|v| (key, v)))
        .collect::<Vec<_>>();

    let headers_map = construct_header_map(
        &workspace_request.workspace_id,
        &workspace_request.organisation_id,
        extra_headers,
    )?;

    let response = http_client
        .put(&url)
        .headers(headers_map.into())
        .header(
            header::AUTHORIZATION,
            format!("Internal {}", state.superposition_token),
        )
        .json(&operations)
        .send()
        .await;

    let (_, config_version_id) =
        process_cac_bulk_operation_http_response(response).await?;

    let updated_experiment =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            if let Some(experiment_group_id) = experiment.experiment_group_id {
                detach_experiment_from_group(
                    &experiment,
                    experiment_group_id,
                    transaction_conn,
                    workspace_request,
                    user,
                )?;
            }

            let updated_experiment = diesel::update(dsl::experiments)
                .filter(dsl::id.eq(experiment_id))
                .set((
                    dsl::status.eq(ExperimentStatusType::CONCLUDED),
                    dsl::last_modified.eq(Utc::now()),
                    dsl::last_modified_by.eq(user.get_email()),
                    dsl::chosen_variant.eq(Some(winner_variant_id)),
                    dsl::change_reason.eq(req.change_reason),
                    dsl::experiment_group_id.eq(None as Option<i64>),
                ))
                .returning(Experiment::as_returning())
                .schema_name(&workspace_request.schema_name)
                .get_result::<Experiment>(transaction_conn)?;
            Ok(updated_experiment)
        })?;

    Ok((updated_experiment, config_version_id))
}

#[allow(clippy::too_many_arguments)]
#[patch("/{experiment_id}/discard")]
async fn discard_handler(
    state: Data<AppState>,
    path: Path<i64>,
    custom_headers: CustomHeaders,
    req: Json<ExperimentStateChangeRequest>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let (response, config_version_id) = discard(
        &state,
        path.into_inner(),
        custom_headers.config_tags,
        req.into_inner(),
        &mut conn,
        &workspace_request,
        &user,
    )
    .await?;

    let experiment_response = ExperimentResponse::from(response);

    let webhook_status = if let Ok(webhook) = fetch_webhook_by_event(
        &state,
        &user,
        &WebhookEvent::ExperimentDiscarded,
        &workspace_request,
    )
    .await
    {
        execute_webhook_call(
            &webhook,
            &experiment_response,
            &config_version_id,
            &workspace_request,
            WebhookEvent::ExperimentDiscarded,
            &state,
        )
        .await
    } else {
        true
    };

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            StatusCode::from_u16(512).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    add_config_version_to_header(&config_version_id, &mut http_resp);
    Ok(http_resp.json(experiment_response))
}

pub async fn discard(
    state: &Data<AppState>,
    experiment_id: i64,
    config_tags: Option<String>,
    req: ExperimentStateChangeRequest,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    workspace_request: &WorkspaceContext,
    user: &User,
) -> superposition::Result<(Experiment, Option<String>)> {
    use superposition_types::database::schema::experiments::dsl;

    let experiment: Experiment = dsl::experiments
        .find(experiment_id)
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(conn)?;

    if !experiment.status.discardable() {
        return Err(bad_argument!(
            "experiment with id {} cannot be discarded",
            experiment_id
        ));
    }

    let operations: Vec<ContextAction> = experiment
        .variants
        .clone()
        .into_inner()
        .into_iter()
        .map(|variant| {
            variant
                .context_id
                .map(ContextAction::Delete)
                .ok_or_else(|| {
                    log::error!("context id not available for variant {:?}", variant.id);
                    unexpected_error!(
                        "Something went wrong, failed to discard experiment"
                    )
                })
        })
        .collect::<superposition::Result<Vec<ContextAction>>>()?;

    // calling CAC bulk api with operations as payload
    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/context/bulk-operations";
    let user_str = serde_json::to_string(&user).map_err(|err| {
        log::error!("Something went wrong, failed to stringify user data {err}");
        unexpected_error!(
            "Something went wrong, failed to stringify user data {}",
            err
        )
    })?;

    let extra_headers = vec![("x-user", Some(user_str)), ("x-config-tags", config_tags)]
        .into_iter()
        .filter_map(|(key, val)| val.map(|v| (key, v)))
        .collect::<Vec<_>>();

    let headers_map = construct_header_map(
        &workspace_request.workspace_id,
        &workspace_request.organisation_id,
        extra_headers,
    )?;

    let response = http_client
        .put(&url)
        .headers(headers_map.into())
        .header(
            header::AUTHORIZATION,
            format!("Internal {}", state.superposition_token),
        )
        .json(&operations)
        .send()
        .await;

    let (_, config_version_id) =
        process_cac_bulk_operation_http_response(response).await?;

    let updated_experiment =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            if let Some(experiment_group_id) = experiment.experiment_group_id {
                detach_experiment_from_group(
                    &experiment,
                    experiment_group_id,
                    transaction_conn,
                    workspace_request,
                    user,
                )?;
            }

            // updating experiment status in db
            let updated_experiment = diesel::update(dsl::experiments)
                .filter(dsl::id.eq(experiment_id))
                .set((
                    req,
                    dsl::status.eq(ExperimentStatusType::DISCARDED),
                    dsl::last_modified.eq(Utc::now()),
                    dsl::last_modified_by.eq(user.get_email()),
                    dsl::chosen_variant.eq(None as Option<String>),
                    dsl::experiment_group_id.eq(None as Option<i64>),
                ))
                .returning(Experiment::as_returning())
                .schema_name(&workspace_request.schema_name)
                .get_result::<Experiment>(transaction_conn)?;

            Ok(updated_experiment)
        })?;

    Ok((updated_experiment, config_version_id))
}

#[route("/applicable-variants", method = "GET", method = "POST")]
async fn get_applicable_variants(
    req: HttpRequest,
    db_conn: DbConnection,
    req_body: Option<Json<ApplicableVariantsRequest>>,
    query_data: Option<Query<ApplicableVariantsQuery>>,
    schema_name: SchemaName,
) -> superposition::Result<Either<Json<Vec<Variant>>, Json<ListResponse<Variant>>>> {
    use superposition_types::database::schema::experiments::dsl;

    let DbConnection(mut conn) = db_conn;
    let req_data = match (req.method().clone(), query_data, req_body) {
        (actix_web::http::Method::GET, Some(query_data), None) => query_data.into_inner(),
        (actix_web::http::Method::POST, None, Some(req_body)) => {
            req_body.into_inner().into()
        }
        _ => {
            return Err(bad_argument!("Invalid input for the method"));
        }
    };

    let experiment_groups = experiment_groups::experiment_groups
        .schema_name(&schema_name)
        .load::<ExperimentGroup>(&mut conn)?;

    let buckets = get_applicable_buckets_from_group(
        &experiment_groups,
        &Value::Object(req_data.context.clone()),
        &req_data.identifier,
    );

    let exp_ids = buckets
        .iter()
        .filter_map(|(_, bucket)| bucket.experiment_id.parse::<i64>().ok())
        .collect::<HashSet<_>>();

    let exps = dsl::experiments
        .filter(
            dsl::id
                .eq_any(exp_ids)
                .and(dsl::status.eq(ExperimentStatusType::INPROGRESS)),
        )
        .schema_name(&schema_name)
        .load::<Experiment>(&mut conn)?
        .into_iter()
        .map(|exp| {
            let exp_response = ExperimentResponse::from(exp);
            let id = exp_response.id.clone();
            (id, exp_response)
        })
        .collect::<HashMap<String, ExperimentResponse>>();

    let applicable_variants = get_applicable_variants_from_group_response(
        &exps,
        &Value::Object(req_data.context),
        &buckets,
    );

    let variants = exps
        .into_iter()
        .filter_map(|(_, experiment)| {
            experiment
                .variants
                .into_inner()
                .into_iter()
                .find(|variant| applicable_variants.contains(&variant.id))
        })
        .collect::<Vec<_>>();

    match *req.method() {
        actix_web::http::Method::POST => {
            Ok(Either::Right(Json(ListResponse::new(variants))))
        }
        _ => Ok(Either::Left(Json(variants))),
    }
}

#[get("")]
async fn list_experiments(
    req: HttpRequest,
    pagination_params: superposition_query::Query<PaginationParams>,
    filters: superposition_query::Query<ExperimentListFilters>,
    dimension_params: DimensionQuery<QueryMap>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let max_event_timestamp: Option<DateTime<Utc>> = event_log::event_log
        .filter(event_log::table_name.eq("experiments"))
        .select(diesel::dsl::max(event_log::timestamp))
        .schema_name(&schema_name)
        .first(&mut conn)?;

    let last_modified = req
        .headers()
        .get("If-Modified-Since")
        .and_then(|header_val| header_val.to_str().ok())
        .and_then(|header_str| {
            DateTime::parse_from_rfc2822(header_str)
                .map(|datetime| datetime.with_timezone(&Utc))
                .ok()
        });

    if max_event_timestamp.is_some() && max_event_timestamp < last_modified {
        return Ok(HttpResponse::NotModified().finish());
    };

    let dimension_params = dimension_params.into_inner();

    let query_builder = |filters: &ExperimentListFilters| {
        let mut builder = experiments::experiments
            .schema_name(&schema_name)
            .into_boxed();
        if let Some(ref states) = filters.status {
            builder = builder.filter(experiments::status.eq_any(states.0.clone()));
        }
        if let Some(ref experiment_name) = filters.experiment_name {
            builder =
                builder.filter(experiments::name.like(format!("%{}%", experiment_name)));
        }
        if let Some(ref created_by) = filters.created_by {
            builder =
                builder.filter(experiments::created_by.eq_any(created_by.0.clone()));
        }
        if let Some(experiment_ids) = filters.experiment_ids.clone() {
            let experiment_ids: HashSet<i64> = experiment_ids
                .0
                .iter()
                .filter_map(|i| i.parse::<i64>().ok())
                .collect();
            builder = builder.filter(experiments::id.eq_any(experiment_ids));
        }
        if let Some(experiment_group_ids) = filters.experiment_group_ids.clone() {
            let experiment_group_ids: HashSet<i64> = experiment_group_ids
                .0
                .iter()
                .filter_map(|i| i.parse::<i64>().ok())
                .collect();
            builder = builder
                .filter(experiments::experiment_group_id.eq_any(experiment_group_ids));
        }
        if let Some(from_data) = filters.from_date {
            builder = builder.filter(experiments::last_modified.ge(from_data));
        }
        if let Some(to_date) = filters.to_date {
            builder = builder.filter(experiments::last_modified.le(to_date));
        }

        builder
    };

    let filters = filters.into_inner();
    let base_query = query_builder(&filters);

    let sort_by = filters.sort_by.clone().unwrap_or(SortBy::Desc);
    let sort_on = filters.sort_on.unwrap_or_default();

    #[rustfmt::skip]
    let base_query = match (sort_on, sort_by) {
        (ExperimentSortOn::LastModifiedAt, SortBy::Desc) => base_query.order(experiments::last_modified.desc()),
        (ExperimentSortOn::LastModifiedAt, SortBy::Asc)  => base_query.order(experiments::last_modified.asc()),
        (ExperimentSortOn::CreatedAt, SortBy::Desc)      => base_query.order(experiments::created_at.desc()),
        (ExperimentSortOn::CreatedAt, SortBy::Asc)       => base_query.order(experiments::created_at.asc()),
    };

    let pagination_params = pagination_params.into_inner();
    let show_all = pagination_params.all.unwrap_or_default();
    let limit = pagination_params.count.unwrap_or(10);
    let offset = (pagination_params.page.unwrap_or(1) - 1) * limit;

    let perform_in_memory_filter = !dimension_params.is_empty()
        || filters.global_experiments_only.unwrap_or_default();

    let paginated_response = if perform_in_memory_filter {
        let all_experiments: Vec<Experiment> = base_query.load(&mut conn)?;
        let filtered_experiments = if filters.global_experiments_only.unwrap_or_default()
        {
            all_experiments
                .into_iter()
                .filter(|experiment| experiment.context.is_empty())
                .collect()
        } else {
            let dimension_keys = dimension_params.keys().cloned().collect::<Vec<_>>();
            let dimension_filtered_experiments =
                Experiment::filter_by_dimension(all_experiments, &dimension_keys);

            let filter_fn = match filters.dimension_match_strategy.unwrap_or_default() {
                DimensionMatchStrategy::Exact => Experiment::filter_exact_match,
                DimensionMatchStrategy::Subset => Experiment::filter_by_eval,
            };

            filter_fn(dimension_filtered_experiments, &dimension_params)
        };

        let experiments = filtered_experiments
            .into_iter()
            .map(ExperimentResponse::from)
            .collect::<Vec<_>>();

        if show_all {
            PaginatedResponse::all(experiments)
        } else {
            let total_items = experiments.len();
            let start = offset as usize;
            let end = min((offset + limit) as usize, total_items);
            let data = experiments
                .get(start..end)
                .map(|slice| slice.to_vec())
                .unwrap_or_default();

            PaginatedResponse {
                total_pages: (total_items as f64 / limit as f64).ceil() as i64,
                total_items: total_items as i64,
                data,
            }
        }
    } else if show_all {
        let result = base_query.load::<Experiment>(&mut conn)?;
        PaginatedResponse::all(result.into_iter().map(ExperimentResponse::from).collect())
    } else {
        let count_query = query_builder(&filters);
        let number_of_experiments = count_query.count().get_result(&mut conn)?;
        let query = base_query.limit(limit).offset(offset);
        let experiment_list = query.load::<Experiment>(&mut conn)?;

        PaginatedResponse {
            total_pages: (number_of_experiments as f64 / limit as f64).ceil() as i64,
            total_items: number_of_experiments,
            data: experiment_list
                .into_iter()
                .map(ExperimentResponse::from)
                .collect(),
        }
    };

    Ok(HttpResponse::Ok().json(paginated_response))
}

#[get("/{id}")]
async fn get_experiment_handler(
    params: web::Path<i64>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<ExperimentResponse>> {
    let DbConnection(mut conn) = db_conn;
    let response = fetch_experiment(&params.into_inner(), &mut conn, &schema_name)?;
    Ok(Json(ExperimentResponse::from(response)))
}

pub fn user_allowed_to_ramp(
    experiment: &Experiment,
    user: &User,
    allow_experiment_self_approval: bool,
) -> bool {
    allow_experiment_self_approval
        || !(experiment.status == ExperimentStatusType::CREATED
            && experiment.created_by == user.get_email())
}

#[allow(clippy::too_many_arguments)]
#[patch("/{id}/ramp")]
async fn ramp(
    state: Data<AppState>,
    params: web::Path<i64>,
    req: web::Json<RampRequest>,
    db_conn: DbConnection,
    user: User,
    workspace_request: WorkspaceContext,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let exp_id = params.into_inner();
    let change_reason = req.change_reason.clone();

    let experiment: Experiment = experiments::experiments
        .find(exp_id)
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(&mut conn)?;

    if !experiment.status.active() {
        return Err(bad_argument!(
            "Experiment is not active, cannot ramp a concluded experiment"
        ));
    }

    let workspace_settings = get_workspace(&workspace_request.schema_name, &mut conn)?;

    if !user_allowed_to_ramp(
        &experiment,
        &user,
        workspace_settings.allow_experiment_self_approval,
    ) {
        return Err(bad_argument!(
            "Experiment creator is not allowed to start experiment, if this is not intended, please change the workspace settings to allow self-approval"
        ));
    }

    let experiment_variants = experiment.variants.clone().into_inner();

    match experiment.experiment_type {
        ExperimentType::Default => {
            // Validate control overrides against resolved config when auto-populate is enabled and experiment is in CREATED state
            if workspace_settings.auto_populate_control
                && experiment.status == ExperimentStatusType::CREATED
            {
                let control_variant = experiment_variants
                    .iter()
                    .find(|v| v.variant_type == VariantType::CONTROL)
                    .ok_or_else(|| {
                        log::error!(
                            "Error finding control variant in the experiment variants"
                        );
                        unexpected_error!(
                            "Error finding control variant in the experiment variants"
                        )
                    })?;

                validate_control_overrides(
                    &control_variant.overrides,
                    &experiment.context,
                    &workspace_request,
                    &user,
                    &state,
                )
                .await?;
            }
        }
        ExperimentType::DeleteOverrides => {
            validate_delete_experiment_variants(
                &user,
                &state,
                &experiment.context,
                &hash(&Value::Object(experiment.context.clone().into())),
                &workspace_request,
                &experiment.variants,
            )
            .await?;
        }
    }

    let old_traffic_percentage = experiment.traffic_percentage;
    let new_traffic_percentage = &req.traffic_percentage;
    let variants_count = experiment.variants.clone().into_inner().len() as u8;

    new_traffic_percentage
        .check_max_allowed(variants_count)
        .map_err(|e| bad_argument!(e))?;

    new_traffic_percentage
        .compare_old(&old_traffic_percentage)
        .map_err(|e| bad_argument!(e))?;

    let now = Utc::now();
    let started_by_request = match experiment.status {
        ExperimentStatusType::CREATED => StartedByChangeSet {
            started_by: Some(user.get_email()),
            started_at: Some(now),
        },
        _ => StartedByChangeSet {
            started_by: None,
            started_at: None,
        },
    };

    let mut experiment_group_id = experiment.experiment_group_id;

    let updated_experiment =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            if experiment.status == ExperimentStatusType::CREATED
                && experiment_group_id.is_none()
            {
                // make a system generated experiment group
                let experiment_group = create_system_generated_experiment_group(
                    &experiment,
                    new_traffic_percentage,
                    &state,
                    transaction_conn,
                    &workspace_request.schema_name,
                    &user,
                )?;
                experiment_group_id = Some(experiment_group.id);
            } else if let Some(experiment_group_id) = experiment_group_id {
                update_experiment_group_buckets(
                    &experiment,
                    &experiment_group_id,
                    new_traffic_percentage,
                    transaction_conn,
                    &workspace_request.schema_name,
                    &user,
                )?;
            }

            let updated_experiment: Experiment = diesel::update(experiments::experiments)
                .filter(experiments::id.eq(exp_id))
                .set((
                    started_by_request,
                    experiments::traffic_percentage.eq(new_traffic_percentage),
                    experiments::last_modified.eq(now),
                    experiments::last_modified_by.eq(user.get_email()),
                    experiments::status.eq(ExperimentStatusType::INPROGRESS),
                    experiments::change_reason.eq(change_reason),
                    experiments::experiment_group_id.eq(experiment_group_id),
                ))
                .returning(Experiment::as_returning())
                .schema_name(&workspace_request.schema_name)
                .get_result(transaction_conn)?;
            Ok(updated_experiment)
        })?;

    let (_, config_version_id) = fetch_cac_config(&state, &workspace_request).await?;
    let experiment_response = ExperimentResponse::from(updated_experiment);

    let webhook_event = if matches!(experiment.status, ExperimentStatusType::CREATED) {
        WebhookEvent::ExperimentStarted
    } else {
        WebhookEvent::ExperimentInprogress
    };
    let webhook_status = if let Ok(webhook) =
        fetch_webhook_by_event(&state, &user, &webhook_event, &workspace_request).await
    {
        execute_webhook_call(
            &webhook,
            &experiment_response,
            &config_version_id,
            &workspace_request,
            webhook_event,
            &state,
        )
        .await
    } else {
        true
    };

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            StatusCode::from_u16(512).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    Ok(http_resp.json(experiment_response))
}

#[allow(clippy::too_many_arguments)]
#[put("/{id}/overrides")]
async fn update_overrides(
    params: web::Path<i64>,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    req: Json<OverrideKeysUpdateRequest>,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let experiment_id = params.into_inner();
    let experiment_group_id = req.experiment_group_id.clone();
    let description = req.description.clone();
    let change_reason = req.change_reason.clone();

    let payload = req.into_inner();
    let variants = payload.variants;

    let first_variant = variants.first().ok_or(bad_argument!(
        "Variant not found in request. Provide at least one entry in variant's list",
    ))?;
    let override_keys =
        extract_override_keys(&first_variant.overrides.clone().into_inner())
            .into_iter()
            .collect();

    // fetch the current variants of the experiment
    let experiment = experiments::experiments
        .find(experiment_id)
        .schema_name(&workspace_request.schema_name)
        .first::<Experiment>(&mut conn)?;

    if experiment.status != ExperimentStatusType::CREATED {
        return Err(bad_argument!(
            "Only experiments in CREATED state can be updated"
        ));
    }

    let experiment_variants: Vec<Variant> = experiment.variants.clone().into_inner();

    let id_to_existing_variant: HashMap<String, &Variant> = HashMap::from_iter(
        experiment_variants
            .iter()
            .map(|variant| (variant.id.to_string(), variant))
            .collect::<Vec<(String, &Variant)>>(),
    );

    // checking if variants passed with correct existing variant ids
    if variants.len() != id_to_existing_variant.len() {
        Err(bad_argument!(
            "Number of variants passed in the request does not match with existing experiment variants"
        ))?;
    }

    let workspace_settings = get_workspace(&workspace_request.schema_name, &mut conn)?;

    /****************** Validating override_keys and variant overrides *********************/

    validate_override_keys(&override_keys)?;

    let variant_ids: HashSet<String> = HashSet::from_iter(
        variants
            .iter()
            .map(|variant| variant.id.to_string())
            .collect::<Vec<String>>(),
    );
    for existing_id in id_to_existing_variant.keys() {
        if !variant_ids.contains(existing_id) {
            Err(bad_argument!(
                "Some variant ids do not match with exisiting experiment variants. Provide all existing variants of the experiment"
            ))?;
        }
    }
    // Checking if all the variants are overriding the mentioned keys
    let mut new_variants: Vec<Variant> = variants
        .clone()
        .into_iter()
        .map(|variant| {
            let existing_variant: &Variant =
                id_to_existing_variant.get(&variant.id).ok_or_else(|| {
                    log::error!(
                        "Variant with id {} not found in existing variants",
                        variant.id
                    );
                    unexpected_error!("Something went wrong")
                })?;
            Ok(Variant {
                overrides: variant.overrides,
                override_id: None,
                ..existing_variant.clone()
            })
        })
        .collect::<superposition::Result<_>>()?;

    let variant_overrides = new_variants
        .iter()
        .map(|variant| variant.overrides.clone().into_inner())
        .collect::<Vec<Overrides>>();

    let experiment_condition =
        Exp::<Condition>::validate_db_data(experiment.context.clone().into())
            .map_err(|err| {
                log::error!(
                    "update_overrides : failed to decode condition from db with error {}",
                    err
                );
                unexpected_error!(err)
            })?
            .into_inner();
    let exp_context_id = hash(&Value::Object(experiment_condition.clone().into()));
    match experiment.experiment_type {
        ExperimentType::Default => {
            let are_valid_variants =
                check_variants_override_coverage(&variant_overrides, &override_keys);
            if !are_valid_variants {
                return Err(
                    bad_argument!(
                        "All variants should contain the keys mentioned in override_keys. Check if any of the following keys [{}] are missing from keys in your variants",
                        override_keys.join(",")
                    )
                )?;
            }

            // Validate control overrides against resolved config when auto-populate is enabled
            if workspace_settings.auto_populate_control {
                let control_variant_id = experiment
                    .variants
                    .iter()
                    .find(|v| v.variant_type == VariantType::CONTROL)
                    .map(|v| v.id.to_string())
                    .ok_or_else(|| {
                        log::error!(
                            "Control variant not found in existing experiment variants"
                        );
                        unexpected_error!(
                            "Control variant not found in existing experiment variants"
                        )
                    })?;

                let req_control_variant = variants
                    .iter()
                    .find(|v| v.id == control_variant_id)
                    .ok_or_else(|| {
                        log::error!("Control variant missing from request variants");
                        bad_argument!("Control variant missing from request variants")
                    })?;

                validate_control_overrides(
                    &req_control_variant.overrides,
                    &experiment.context,
                    &workspace_request,
                    &user,
                    &state,
                )
                .await?;
            }

            // validating experiment against other active experiments based on permission flags
            let flags = &state.experimentation_flags;
            let (valid, reason) = validate_experiment(
                &experiment_condition,
                &override_keys,
                Some(experiment_id),
                flags,
                &workspace_request.schema_name,
                &mut conn,
            )?;
            if !valid {
                return Err(bad_argument!(reason));
            }
        }
        ExperimentType::DeleteOverrides => {
            validate_delete_experiment_variants(
                &user,
                &state,
                &experiment_condition,
                &exp_context_id,
                &workspace_request,
                &new_variants,
            )
            .await?;
        }
    }

    /******************************* Updating contexts ************************************/
    let mut cac_operations: Vec<ContextAction> = vec![];

    // adding operations to create new updated variant contexts
    for variant in &new_variants {
        let overrides: Map<String, Value> = variant.overrides.clone().into_inner().into();
        let payload = UpdateRequest {
            context: Identifier::Id(variant.context_id.clone().ok_or_else(|| {
                unexpected_error!("context id not available for variant {}", variant.id)
            })?),
            override_: Cac::<Overrides>::try_from(overrides).map_err(|err| {
                log::error!("failed to convert variant overrides to cac override {err}");
                bad_argument!("failed to convert variant overrides to cac override")
            })?,
            description: description.clone(),
            change_reason: change_reason.clone(),
        };

        cac_operations.push(ContextAction::Replace(payload));
    }

    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/context/bulk-operations";
    let user_str = serde_json::to_string(&user).map_err(|err| {
        log::error!("Something went wrong, failed to stringify user data {err}");
        unexpected_error!(
            "Something went wrong, failed to stringify user data {}",
            err
        )
    })?;
    let extra_headers = vec![
        ("x-user", Some(user_str)),
        ("x-config-tags", custom_headers.config_tags),
    ]
    .into_iter()
    .filter_map(|(key, val)| val.map(|v| (key, v)))
    .collect::<Vec<_>>();

    let headers_map = construct_header_map(
        &workspace_request.workspace_id,
        &workspace_request.organisation_id,
        extra_headers,
    )?;

    let response = http_client
        .put(&url)
        .headers(headers_map.into())
        .header(
            header::AUTHORIZATION,
            format!("Internal {}", state.superposition_token),
        )
        .json(&cac_operations)
        .send()
        .await;

    // directly return an error response if not a 200 response
    let (resp_contexts, config_version_id) =
        process_cac_bulk_operation_http_response(response).await?;
    let created_contexts = resp_contexts
        .into_iter()
        .map(|item| match item {
            ContextBulkResponse::Replace(context) => Ok(context),
            _ => Err(format!("Unexpected response item: {item:?}")),
        })
        .collect::<Result<Vec<_>, _>>()
        .map_err(|err| {
            log::error!(
                "Something went wrong, failed to parse bulk operations response {err}"
            );
            unexpected_error!("Something went wrong")
        })?;

    for i in 0..created_contexts.len() {
        let created_context = &created_contexts[i];
        if new_variants[i]
            .context_id
            .clone()
            .map(|id| id != created_context.id)
            .unwrap_or_default()
        {
            log::error!(
                "Context id changed from {} to {}",
                new_variants[i].context_id.clone().unwrap_or_default(),
                created_context.id
            );
            Err(unexpected_error!("Something went wrong"))?;
        }

        new_variants[i].override_id = Some(created_context.override_id.clone());
    }

    /*************************** Updating experiment in DB **************************/
    let existing_metrics = &experiment.metrics.clone();
    let updated_metrics = payload.metrics.as_ref().unwrap_or(existing_metrics);

    let updated_experiment =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let experiment_group_id_result = handle_experiment_group_membership(
                &experiment,
                &experiment_group_id,
                &experiment.experiment_group_id,
                &state,
                transaction_conn,
                &workspace_request.schema_name,
                &user,
            )?;

            let updated_experiment =
                diesel::update(experiments::experiments.find(experiment_id))
                    .set((
                        experiments::variants.eq(Variants::new(new_variants)),
                        experiments::override_keys.eq(override_keys),
                        experiments::change_reason.eq(change_reason),
                        experiments::description
                            .eq(description.unwrap_or(experiment.description)),
                        experiments::metrics.eq(updated_metrics),
                        experiments::last_modified.eq(Utc::now()),
                        experiments::last_modified_by.eq(user.get_email()),
                        experiments::experiment_group_id.eq(experiment_group_id_result),
                    ))
                    .returning(Experiment::as_returning())
                    .schema_name(&workspace_request.schema_name)
                    .get_result::<Experiment>(transaction_conn)?;

            Ok(updated_experiment)
        })?;

    let experiment_response = ExperimentResponse::from(updated_experiment);

    let webhook_status = if let Ok(webhook) = fetch_webhook_by_event(
        &state,
        &user,
        &WebhookEvent::ExperimentUpdated,
        &workspace_request,
    )
    .await
    {
        execute_webhook_call(
            &webhook,
            &experiment_response,
            &config_version_id,
            &workspace_request,
            WebhookEvent::ExperimentUpdated,
            &state,
        )
        .await
    } else {
        true
    };

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            StatusCode::from_u16(512).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    add_config_version_to_header(&config_version_id, &mut http_resp);
    Ok(http_resp.json(experiment_response))
}

#[get("/audit")]
async fn get_audit_logs(
    filters: Query<AuditQueryFilters>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<EventLog>>> {
    let DbConnection(mut conn) = db_conn;

    let query_builder = |filters: &AuditQueryFilters| {
        let mut builder = event_log::event_log.schema_name(&schema_name).into_boxed();
        if let Some(tables) = filters.table.clone() {
            builder = builder.filter(event_log::table_name.eq_any(tables.0));
        }
        if let Some(actions) = filters.action.clone() {
            builder = builder.filter(event_log::action.eq_any(actions.0));
        }
        if let Some(username) = filters.username.clone() {
            builder = builder.filter(event_log::user_name.eq(username));
        }
        let now = Utc::now();
        builder
            .filter(
                event_log::timestamp
                    .ge(filters.from_date.unwrap_or(now - Duration::hours(24))),
            )
            .filter(event_log::timestamp.le(filters.to_date.unwrap_or(now)))
    };
    let filters = filters.into_inner();
    let base_query = query_builder(&filters);
    let count_query = query_builder(&filters);

    let limit = filters.count.unwrap_or(10);
    let offset = (filters.page.unwrap_or(1) - 1) * limit;
    let query = base_query
        .order(event_log::timestamp.desc())
        .limit(limit)
        .offset(offset);

    let log_count: i64 = count_query.count().get_result(&mut conn)?;

    let logs: Vec<EventLog> = query.load(&mut conn)?;

    let total_pages = (log_count as f64 / limit as f64).ceil() as i64;

    Ok(Json(PaginatedResponse {
        total_items: log_count,
        total_pages,
        data: logs,
    }))
}

#[patch("/{experiment_id}/pause")]
async fn pause_handler(
    state: Data<AppState>,
    path: Path<i64>,
    req: Json<ExperimentStateChangeRequest>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let response = pause(
        path.into_inner(),
        req.into_inner(),
        &mut conn,
        &workspace_request,
        &user,
    )
    .await?;

    let experiment_response = ExperimentResponse::from(response);

    let webhook_status = if let Ok(webhook) = fetch_webhook_by_event(
        &state,
        &user,
        &WebhookEvent::ExperimentPaused,
        &workspace_request,
    )
    .await
    {
        execute_webhook_call(
            &webhook,
            &experiment_response,
            &None,
            &workspace_request,
            WebhookEvent::ExperimentPaused,
            &state,
        )
        .await
    } else {
        true
    };

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            StatusCode::from_u16(512).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    Ok(http_resp.json(experiment_response))
}

pub async fn pause(
    experiment_id: i64,
    req: ExperimentStateChangeRequest,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    workspace_request: &WorkspaceContext,
    user: &User,
) -> superposition::Result<Experiment> {
    use superposition_types::database::schema::experiments::dsl;

    let experiment: Experiment = dsl::experiments
        .find(experiment_id)
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(conn)?;

    if !experiment.status.pausable() {
        return Err(bad_argument!(
            "experiment with id {} cannot be paused",
            experiment_id
        ));
    }

    // not removing buckets here, so that once resumed, the experiment can continue
    let updated_experiment = diesel::update(dsl::experiments)
        .filter(dsl::id.eq(experiment_id))
        .set((
            req,
            dsl::status.eq(ExperimentStatusType::PAUSED),
            dsl::last_modified.eq(Utc::now()),
            dsl::last_modified_by.eq(user.get_email()),
        ))
        .returning(Experiment::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(conn)?;

    Ok(updated_experiment)
}

#[patch("/{experiment_id}/resume")]
async fn resume_handler(
    state: Data<AppState>,
    path: Path<i64>,
    req: Json<ExperimentStateChangeRequest>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let response = resume(
        path.into_inner(),
        req.into_inner(),
        &mut conn,
        &workspace_request,
        &user,
    )
    .await?;

    let experiment_response = ExperimentResponse::from(response);

    let webhook_status = if let Ok(webhook) = fetch_webhook_by_event(
        &state,
        &user,
        &WebhookEvent::ExperimentInprogress,
        &workspace_request,
    )
    .await
    {
        execute_webhook_call(
            &webhook,
            &experiment_response,
            &None,
            &workspace_request,
            WebhookEvent::ExperimentInprogress,
            &state,
        )
        .await
    } else {
        true
    };

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            StatusCode::from_u16(512).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    Ok(http_resp.json(experiment_response))
}

pub async fn resume(
    experiment_id: i64,
    req: ExperimentStateChangeRequest,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    workspace_request: &WorkspaceContext,
    user: &User,
) -> superposition::Result<Experiment> {
    use superposition_types::database::schema::experiments::dsl;

    let experiment: Experiment = dsl::experiments
        .find(experiment_id)
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(conn)?;

    if !experiment.status.resumable() {
        return Err(bad_argument!(
            "experiment with id {} cannot be resumed",
            experiment_id
        ));
    }

    let updated_experiment = diesel::update(dsl::experiments)
        .filter(dsl::id.eq(experiment_id))
        .set((
            req,
            dsl::status.eq(ExperimentStatusType::INPROGRESS),
            dsl::last_modified.eq(Utc::now()),
            dsl::last_modified_by.eq(user.get_email()),
        ))
        .returning(Experiment::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(conn)?;

    Ok(updated_experiment)
}
