use std::collections::{HashMap, HashSet};

use actix_http::header::{self};
use actix_web::{
    get, patch, post, put,
    web::{self, Data, Json, Query},
    HttpRequest, HttpResponse, HttpResponseBuilder, Scope,
};
use chrono::{DateTime, Duration, Utc};
use diesel::{
    dsl::sql,
    r2d2::{ConnectionManager, PooledConnection},
    sql_types::{Bool, Text},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl, SelectableHelper,
    TextExpressionMethods,
};
use reqwest::{Method, Response, StatusCode};
use serde_json::{json, Map, Value};
use service_utils::{
    helpers::{
        construct_request_headers, execute_webhook_call, generate_snowflake_id, request,
    },
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, SchemaName, WorkspaceContext,
    },
};
use superposition_macros::{bad_argument, response_error, unexpected_error};
use superposition_types::{
    custom_query::PaginationParams,
    database::{
        models::experimentation::{
            EventLog, Experiment, ExperimentStatusType, Variant, Variants,
        },
        schema::{event_log::dsl as event_log, experiments::dsl as experiments},
    },
    result as superposition,
    webhook::{WebhookConfig, WebhookEvent},
    Condition, DBConnection, Exp, Overrides, PaginatedResponse, SortBy, TenantConfig,
    User,
};

use super::{
    helpers::{
        add_variant_dimension_to_ctx, check_variant_types,
        check_variants_override_coverage, decide_variant, extract_override_keys,
        fetch_cac_config, validate_experiment, validate_override_keys,
    },
    types::{
        ApplicableVariantsQuery, AuditQueryFilters, ConcludeExperimentRequest,
        ContextAction, ContextBulkResponse, ContextMoveReq, ContextPutReq,
        DiscardExperimentRequest, ExperimentCreateRequest, ExperimentCreateResponse,
        ExperimentListFilters, ExperimentResponse, OverrideKeysUpdateRequest,
        RampRequest,
    },
};
use crate::api::experiments::{helpers::construct_header_map, types::ExperimentSortOn};

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
}

fn add_config_version_to_header(
    config_version: &Option<String>,
    resp_builder: &mut HttpResponseBuilder,
) {
    if let Some(val) = config_version {
        resp_builder.insert_header((AppHeader::XConfigVersion.to_string(), val.clone()));
    }
}

async fn parse_error_response(
    response: reqwest::Response,
) -> superposition::Result<(StatusCode, superposition::ErrorResponse)> {
    let status_code = response.status();
    let error_response = response
        .json::<superposition::ErrorResponse>()
        .await
        .map_err(|err: reqwest::Error| {
            log::error!("failed to parse error response: {}", err);
            unexpected_error!("Something went wrong")
        })?;
    log::error!("http call to CAC failed with err {:?}", error_response);

    Ok((status_code, error_response))
}

async fn process_cac_http_response(
    response: Result<Response, reqwest::Error>,
) -> superposition::Result<(Vec<ContextBulkResponse>, Option<String>)> {
    let internal_server_error = unexpected_error!("Something went wrong.");
    match response {
        Ok(res) if res.status().is_success() => {
            let config_version = res
                .headers()
                .get("x-config-version")
                .and_then(|val| val.to_str().map_or(None, |v| Some(v.to_string())));
            let bulk_resp =
                res.json::<Vec<ContextBulkResponse>>()
                    .await
                    .map_err(|err| {
                        log::error!("failed to parse JSON response with error: {}", err);
                        internal_server_error
                    })?;
            Ok((bulk_resp, config_version))
        }
        Ok(res) => {
            log::error!("http call to CAC failed with status_code {}", res.status());

            if res.status().is_client_error() {
                let (status_code, error_response) = parse_error_response(res).await?;
                Err(response_error!(status_code, error_response.message))
            } else {
                Err(internal_server_error)
            }
        }
        Err(err) => {
            log::error!("reqwest failed to send request to CAC with error: {}", err);
            Err(internal_server_error)
        }
    }
}

#[allow(clippy::too_many_arguments)]
#[post("")]
async fn create(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: web::Json<ExperimentCreateRequest>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
    user: User,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    use superposition_types::database::schema::experiments::dsl::experiments;
    let mut variants = req.variants.to_vec();
    let DbConnection(mut conn) = db_conn;
    let description = req.description.clone();
    let change_reason = req.change_reason.clone();

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

    // Checking if all the variants are overriding the mentioned keys
    let variant_overrides = variants
        .iter()
        .map(|variant| variant.overrides.clone().into_inner())
        .collect::<Vec<Overrides>>();
    let are_valid_variants =
        check_variants_override_coverage(&variant_overrides, &unique_override_keys);
    if !are_valid_variants {
        return Err(bad_argument!(
            "all variants should contain the keys mentioned in override_keys. Check if any of the following keys [{}] are missing from keys in your variants",
                unique_override_keys.join(",")
            )
        );
    }

    // validating context
    let exp_context = req.context.clone().into_inner();

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

    // generating snowflake id for experiment
    let experiment_id = generate_snowflake_id(&state)?;

    //create overrides in CAC, if successfull then create experiment in DB
    let mut cac_operations: Vec<ContextAction> = vec![];
    for variant in &mut variants {
        let variant_id = experiment_id.to_string() + "-" + &variant.id;

        // updating variant.id to => experiment_id + variant.id
        variant.id = variant_id.to_string();

        let updated_cacccontext =
            add_variant_dimension_to_ctx(&exp_context, variant_id.to_string())?;

        let payload = ContextPutReq {
            context: updated_cacccontext
                .as_object()
                .ok_or_else(|| {
                    log::error!("Could not convert updated CAC context to serde Object");
                    unexpected_error!(
                        "Something went wrong, failed to create experiment contexts"
                    )
                })?
                .clone(),
            r#override: json!(variant.overrides),
            description: Some(description.clone()),
            change_reason: change_reason.clone(),
        };
        cac_operations.push(ContextAction::PUT(payload));
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
    let (resp_contexts, config_version_id) = process_cac_http_response(response).await?;
    let created_contexts = resp_contexts
        .into_iter()
        .map(|item| match item {
            ContextBulkResponse::PUT(context) => Ok(context),
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
        variants[i].context_id = Some(created_context.context_id.clone());
        variants[i].override_id = Some(created_context.override_id.clone());
    }

    // inserting experiment in db
    let new_experiment = Experiment {
        id: experiment_id,
        created_by: user.get_email(),
        created_at: Utc::now(),
        last_modified: Utc::now(),
        name: req.name.to_string(),
        override_keys: unique_override_keys.to_vec(),
        traffic_percentage: 0,
        status: ExperimentStatusType::CREATED,
        context: req.context.clone().into_inner(),
        variants: Variants::new(variants),
        last_modified_by: user.get_email(),
        chosen_variant: None,
        description,
        change_reason,
    };

    let mut inserted_experiments = diesel::insert_into(experiments)
        .values(&new_experiment)
        .returning(Experiment::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_results(&mut conn)?;

    let inserted_experiment: Experiment = inserted_experiments.remove(0);
    let response = ExperimentCreateResponse::from(inserted_experiment.clone());
    if let WebhookConfig::Enabled(experiments_webhook_config) =
        &tenant_config.experiments_webhook_config
    {
        execute_webhook_call(
            experiments_webhook_config,
            &ExperimentResponse::from(inserted_experiment),
            &config_version_id,
            &workspace_request,
            WebhookEvent::ExperimentCreated,
            &state.app_env,
            &state.http_client,
            &state.kms_client,
        )
        .await?;
    }
    let mut http_resp = HttpResponse::Ok();
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
    tenant_config: TenantConfig,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(conn) = db_conn;
    let (response, config_version_id) = conclude(
        &state,
        path.into_inner(),
        custom_headers.config_tags,
        req.into_inner(),
        conn,
        &workspace_request,
        &user,
    )
    .await?;

    let experiment_response = ExperimentResponse::from(response);

    if let WebhookConfig::Enabled(experiments_webhook_config) =
        &tenant_config.experiments_webhook_config
    {
        execute_webhook_call(
            experiments_webhook_config,
            &experiment_response,
            &config_version_id,
            &workspace_request,
            WebhookEvent::ExperimentConcluded,
            &state.app_env,
            &state.http_client,
            &state.kms_client,
        )
        .await?;
    }

    let mut http_resp = HttpResponse::Ok();
    add_config_version_to_header(&config_version_id, &mut http_resp);
    Ok(http_resp.json(experiment_response))
}

#[allow(clippy::too_many_arguments)]
pub async fn conclude(
    state: &Data<AppState>,
    experiment_id: i64,
    config_tags: Option<String>,
    req: ConcludeExperimentRequest,
    mut conn: PooledConnection<ConnectionManager<PgConnection>>,
    workspace_request: &WorkspaceContext,
    user: &User,
) -> superposition::Result<(Experiment, Option<String>)> {
    use superposition_types::database::schema::experiments::dsl;

    let change_reason = req.change_reason.clone();
    let winner_variant_id: String = req.chosen_variant.to_owned();

    let experiment: Experiment = dsl::experiments
        .find(experiment_id)
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(&mut conn)?;

    let description = match req.description.clone() {
        Some(desc) => desc,
        None => experiment.description.clone(),
    };
    if !experiment.status.active() {
        return Err(bad_argument!(
            "experiment with id {} is already {}",
            experiment_id,
            experiment.status
        ));
    }

    let experiment_context: Map<String, Value> = experiment.context.into();

    let mut operations: Vec<ContextAction> = vec![];

    let mut is_valid_winner_variant = false;
    for variant in experiment.variants.into_inner() {
        let context_id = variant.context_id.ok_or_else(|| {
            log::error!("context id not available for variant {:?}", variant.id);
            unexpected_error!("Something went wrong, failed to conclude experiment")
        })?;

        if variant.id == winner_variant_id {
            if !experiment_context.is_empty() {
                let context_move_req = ContextMoveReq {
                    context: experiment_context.clone(),
                    description: description.clone(),
                    change_reason: change_reason.clone(),
                };
                operations.push(ContextAction::MOVE((context_id, context_move_req)));
            } else {
                let user_str = serde_json::to_string(&user).map_err(|err| {
                    log::error!(
                        "Something went wrong, failed to stringify user data {err}"
                    );
                    unexpected_error!(
                        "Something went wrong, failed to stringify user data {}",
                        err
                    )
                })?;

                for (key, val) in variant.overrides.into_inner() {
                    let create_req = HashMap::from([("value", val)]);

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
                        request::<_, Value>(url, Method::PUT, Some(create_req), headers)
                            .await
                            .map_err(|err| unexpected_error!(err))?;
                }
                operations.push(ContextAction::DELETE(context_id));
            }

            is_valid_winner_variant = true;
        } else {
            // delete this context
            operations.push(ContextAction::DELETE(context_id));
        }
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

    let (_, config_version_id) = process_cac_http_response(response).await?;

    // updating experiment status in db
    let updated_experiment = diesel::update(dsl::experiments)
        .filter(dsl::id.eq(experiment_id))
        .set((
            dsl::status.eq(ExperimentStatusType::CONCLUDED),
            dsl::last_modified.eq(Utc::now()),
            dsl::last_modified_by.eq(user.get_email()),
            dsl::chosen_variant.eq(Some(winner_variant_id)),
            dsl::change_reason.eq(change_reason),
        ))
        .returning(Experiment::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(&mut conn)?;

    Ok((updated_experiment, config_version_id))
}

#[allow(clippy::too_many_arguments)]
#[patch("/{experiment_id}/discard")]
async fn discard_handler(
    state: Data<AppState>,
    path: web::Path<i64>,
    custom_headers: CustomHeaders,
    req: web::Json<DiscardExperimentRequest>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
    tenant_config: TenantConfig,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(conn) = db_conn;
    let (response, config_version_id) = discard(
        &state,
        path.into_inner(),
        custom_headers.config_tags,
        req.into_inner(),
        conn,
        &workspace_request,
        &user,
    )
    .await?;

    let experiment_response = ExperimentResponse::from(response);

    if let WebhookConfig::Enabled(experiments_webhook_config) =
        &tenant_config.experiments_webhook_config
    {
        execute_webhook_call(
            experiments_webhook_config,
            &experiment_response,
            &config_version_id,
            &workspace_request,
            WebhookEvent::ExperimentDiscarded,
            &state.app_env,
            &state.http_client,
            &state.kms_client,
        )
        .await?;
    }

    let mut http_resp = HttpResponse::Ok();
    add_config_version_to_header(&config_version_id, &mut http_resp);
    Ok(http_resp.json(experiment_response))
}

pub async fn discard(
    state: &Data<AppState>,
    experiment_id: i64,
    config_tags: Option<String>,
    req: DiscardExperimentRequest,
    mut conn: PooledConnection<ConnectionManager<PgConnection>>,
    workspace_request: &WorkspaceContext,
    user: &User,
) -> superposition::Result<(Experiment, Option<String>)> {
    use superposition_types::database::schema::experiments::dsl;

    let experiment: Experiment = dsl::experiments
        .find(experiment_id)
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(&mut conn)?;

    if !experiment.status.discardable() {
        return Err(bad_argument!(
            "experiment with id {} cannot be discarded",
            experiment_id
        ));
    }

    let operations: Vec<ContextAction> = experiment
        .variants
        .into_inner()
        .into_iter()
        .map(|variant| {
            variant
                .context_id
                .map(ContextAction::DELETE)
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

    let (_, config_version_id) = process_cac_http_response(response).await?;

    // updating experiment status in db
    let updated_experiment = diesel::update(dsl::experiments)
        .filter(dsl::id.eq(experiment_id))
        .set((
            dsl::status.eq(ExperimentStatusType::DISCARDED),
            dsl::last_modified.eq(Utc::now()),
            dsl::last_modified_by.eq(user.get_email()),
            dsl::chosen_variant.eq(None as Option<String>),
            dsl::change_reason.eq(req.change_reason),
        ))
        .returning(Experiment::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(&mut conn)?;

    Ok((updated_experiment, config_version_id))
}

#[get("/applicable-variants")]
async fn get_applicable_variants(
    db_conn: DbConnection,
    query_data: Query<ApplicableVariantsQuery>,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let query_data = query_data.into_inner();

    let experiments = experiments::experiments
        .filter(experiments::status.ne_all(vec![
            ExperimentStatusType::CONCLUDED,
            ExperimentStatusType::DISCARDED,
        ]))
        .schema_name(&schema_name)
        .load::<Experiment>(&mut conn)?;

    let experiments = experiments.into_iter().filter(|exp| {
        let context: Map<String, Value> = exp.context.clone().into();
        context.is_empty()
            || jsonlogic::apply(
                &Value::Object(context),
                &Value::Object(query_data.context.clone()),
            ) == Ok(Value::Bool(true))
    });

    let mut variants = Vec::new();
    for exp in experiments {
        if let Some(v) = decide_variant(
            exp.traffic_percentage as u8,
            exp.variants.into_inner(),
            query_data.toss,
        )
        .map_err(|e| {
            log::error!("Unable to decide variant {e}");
            unexpected_error!("Something went wrong.")
        })? {
            variants.push(v)
        }
    }

    Ok(HttpResponse::Ok().json(variants))
}

#[get("")]
async fn list_experiments(
    req: HttpRequest,
    pagination_params: Query<PaginationParams>,
    filters: Query<ExperimentListFilters>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    if let Some(true) = pagination_params.all {
        let result = experiments::experiments
            .schema_name(&schema_name)
            .get_results::<Experiment>(&mut conn)?;
        return Ok(
            HttpResponse::Ok().json(PaginatedResponse::<ExperimentResponse> {
                total_pages: 1,
                total_items: result.len() as i64,
                data: result.into_iter().map(ExperimentResponse::from).collect(),
            }),
        );
    }

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
        if let Some(ref context_search) = filters.context {
            builder = builder.filter(
                sql::<Bool>("context::text LIKE ")
                    .bind::<Text, _>(format!("%{}%", context_search)),
            );
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
        let now = Utc::now();
        builder
            .filter(
                experiments::last_modified
                    .ge(filters.from_date.unwrap_or(now - Duration::hours(24))),
            )
            .filter(experiments::last_modified.le(filters.to_date.unwrap_or(now)))
    };
    let filters = filters.into_inner();
    let base_query = query_builder(&filters);

    let count_query = query_builder(&filters);
    let number_of_experiments = count_query.count().get_result(&mut conn)?;
    let limit = pagination_params.count.unwrap_or(10);
    let offset = (pagination_params.page.unwrap_or(1) - 1) * limit;

    let sort_by = filters.sort_by.unwrap_or_default();
    let sort_on = filters.sort_on.unwrap_or_default();
    #[rustfmt::skip]
    let base_query = match (sort_on, sort_by) {
        (ExperimentSortOn::LastModifiedAt, SortBy::Desc) => base_query.order(experiments::last_modified.desc()),
        (ExperimentSortOn::LastModifiedAt, SortBy::Asc)  => base_query.order(experiments::last_modified.asc()),
        (ExperimentSortOn::CreatedAt, SortBy::Desc)      => base_query.order(experiments::created_at.desc()),
        (ExperimentSortOn::CreatedAt, SortBy::Asc)       => base_query.order(experiments::created_at.asc()),
    };
    let query = base_query.limit(limit).offset(offset);
    let experiment_list = query.load::<Experiment>(&mut conn)?;
    let total_pages = (number_of_experiments as f64 / limit as f64).ceil() as i64;
    Ok(
        HttpResponse::Ok().json(PaginatedResponse::<ExperimentResponse> {
            total_pages,
            total_items: number_of_experiments,
            data: experiment_list
                .into_iter()
                .map(ExperimentResponse::from)
                .collect(),
        }),
    )
}

#[get("/{id}")]
async fn get_experiment_handler(
    params: web::Path<i64>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<ExperimentResponse>> {
    let DbConnection(mut conn) = db_conn;
    let response = get_experiment(params.into_inner(), &mut conn, &schema_name)?;
    Ok(Json(ExperimentResponse::from(response)))
}

pub fn get_experiment(
    experiment_id: i64,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Experiment> {
    use superposition_types::database::schema::experiments::dsl::*;
    let result: Experiment = experiments
        .find(experiment_id)
        .schema_name(schema_name)
        .get_result::<Experiment>(conn)?;

    Ok(result)
}

#[allow(clippy::too_many_arguments)]
#[patch("/{id}/ramp")]
async fn ramp(
    data: Data<AppState>,
    params: web::Path<i64>,
    req: web::Json<RampRequest>,
    db_conn: DbConnection,
    user: User,
    workspace_request: WorkspaceContext,
    tenant_config: TenantConfig,
) -> superposition::Result<Json<ExperimentResponse>> {
    let DbConnection(mut conn) = db_conn;
    let exp_id = params.into_inner();
    let change_reason = req.change_reason.clone();

    let experiment: Experiment = experiments::experiments
        .find(exp_id)
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(&mut conn)?;

    let old_traffic_percentage = experiment.traffic_percentage as u8;
    let new_traffic_percentage = req.traffic_percentage as u8;
    let variants_count = experiment.variants.into_inner().len() as u8;
    let max = 100 / variants_count;

    if !experiment.status.active() {
        return Err(bad_argument!(
            "experiment already concluded, cannot ramp a concluded experiment"
        ));
    } else if new_traffic_percentage > max {
        return Err(bad_argument!(
            "The traffic_percentage cannot exceed {max}. Provide a traffic percentage less than {max}"
        ))?;
    } else if new_traffic_percentage != 0
        && new_traffic_percentage == old_traffic_percentage
    {
        return Err(bad_argument!("The traffic_percentage is same as provided"))?;
    }
    let updated_experiment: Experiment = diesel::update(experiments::experiments)
        .filter(experiments::id.eq(exp_id))
        .set((
            experiments::traffic_percentage.eq(req.traffic_percentage as i32),
            experiments::last_modified.eq(Utc::now()),
            experiments::last_modified_by.eq(user.get_email()),
            experiments::status.eq(ExperimentStatusType::INPROGRESS),
            experiments::change_reason.eq(change_reason),
        ))
        .returning(Experiment::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_result(&mut conn)?;

    let (_, config_version_id) = fetch_cac_config(&data, &workspace_request).await?;
    let experiment_response = ExperimentResponse::from(updated_experiment);

    let webhook_event = if new_traffic_percentage == 0
        && matches!(experiment.status, ExperimentStatusType::CREATED)
    {
        WebhookEvent::ExperimentStarted
    } else {
        WebhookEvent::ExperimentInprogress
    };
    if let WebhookConfig::Enabled(experiments_webhook_config) =
        &tenant_config.experiments_webhook_config
    {
        execute_webhook_call(
            experiments_webhook_config,
            &experiment_response,
            &config_version_id,
            &workspace_request,
            webhook_event,
            &data.app_env,
            &data.http_client,
            &data.kms_client,
        )
        .await?;
    }

    Ok(Json(experiment_response))
}

#[allow(clippy::too_many_arguments)]
#[put("/{id}/overrides")]
async fn update_overrides(
    params: web::Path<i64>,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    req: web::Json<OverrideKeysUpdateRequest>,
    workspace_request: WorkspaceContext,
    tenant_config: TenantConfig,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let experiment_id = params.into_inner();
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

    let experiment_variants: Vec<Variant> = experiment.variants.into_inner();

    let id_to_existing_variant: HashMap<String, &Variant> = HashMap::from_iter(
        experiment_variants
            .iter()
            .map(|variant| (variant.id.to_string(), variant))
            .collect::<Vec<(String, &Variant)>>(),
    );

    /****************** Validating override_keys and variant overrides *********************/

    validate_override_keys(&override_keys)?;

    // checking if variants passed with correct existing variant ids
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
        .into_iter()
        .map(|variant| {
            let existing_variant: &Variant =
                id_to_existing_variant.get(&variant.id).unwrap();
            Variant {
                id: variant.id,
                variant_type: existing_variant.variant_type.clone(),
                overrides: variant.overrides,
                override_id: None,
                context_id: None,
            }
        })
        .collect();

    let variant_overrides = new_variants
        .iter()
        .map(|variant| variant.overrides.clone().into_inner())
        .collect::<Vec<Overrides>>();
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
    let experiment_condition =
        Exp::<Condition>::validate_db_data(experiment.context.into())
            .map_err(|err| {
                log::error!(
                    "update_overrides : failed to decode condition from db with error {}",
                    err
                );
                unexpected_error!(err)
            })?
            .into_inner();

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

    /******************************* Updating contexts ************************************/
    let mut cac_operations: Vec<ContextAction> = vec![];

    // adding operations to create new updated variant contexts
    for variant in &mut new_variants {
        let updated_cac_context =
            add_variant_dimension_to_ctx(&experiment_condition, variant.id.to_string())
                .map_err(|e| {
                log::error!("failed to add `variantIds` dimension to context: {e}");
                unexpected_error!("Something went wrong, failed to update experiment")
            })?;

        let payload = ContextPutReq {
            context: updated_cac_context
                .as_object()
                .ok_or_else(|| {
                    log::error!("failed to parse updated context with variant dimension");
                    unexpected_error!("Something went wrong, failed to update experiment")
                })?
                .clone(),
            r#override: json!(variant.overrides),
            description: description.clone(),
            change_reason: change_reason.clone(),
        };
        cac_operations.push(ContextAction::REPLACE(payload));
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
    let (resp_contexts, config_version_id) = process_cac_http_response(response).await?;
    let created_contexts = resp_contexts
        .into_iter()
        .map(|item| match item {
            ContextBulkResponse::REPLACE(context) => Ok(context),
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

        new_variants[i].context_id = Some(created_context.context_id.clone());
        new_variants[i].override_id = Some(created_context.override_id.clone());
    }

    /*************************** Updating experiment in DB **************************/
    let updated_experiment = diesel::update(experiments::experiments.find(experiment_id))
        .set((
            experiments::variants.eq(Variants::new(new_variants)),
            experiments::override_keys.eq(override_keys),
            experiments::change_reason.eq(change_reason),
            experiments::last_modified.eq(Utc::now()),
            experiments::last_modified_by.eq(user.get_email()),
        ))
        .returning(Experiment::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_result::<Experiment>(&mut conn)?;

    let experiment_response = ExperimentResponse::from(updated_experiment);

    if let WebhookConfig::Enabled(experiments_webhook_config) =
        &tenant_config.experiments_webhook_config
    {
        execute_webhook_call(
            experiments_webhook_config,
            &experiment_response,
            &config_version_id,
            &workspace_request,
            WebhookEvent::ExperimentUpdated,
            &state.app_env,
            &state.http_client,
            &state.kms_client,
        )
        .await?;
    }

    let mut http_resp = HttpResponse::Ok();
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
