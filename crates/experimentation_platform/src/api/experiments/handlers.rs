use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use actix_http::header::{self, HeaderMap, HeaderName, HeaderValue};
use actix_web::{
    get, patch, post, put,
    web::{self, Data, Json, Query},
    HttpRequest, HttpResponse, HttpResponseBuilder, Scope,
};
use anyhow::anyhow;
use chrono::{DateTime, Duration, NaiveDateTime, Utc};
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use reqwest::{Method, Response, StatusCode};
use serde_json::{json, Map, Value};
use service_utils::helpers::{construct_request_headers, generate_snowflake_id, request};
use service_utils::service::types::{
    AppHeader, AppState, CustomHeaders, DbConnection, Tenant,
};
use superposition_macros::{bad_argument, response_error, unexpected_error};
use superposition_types::{result as superposition, Condition, Exp, Overrides, User};

use super::{
    helpers::{
        add_variant_dimension_to_ctx, check_variant_types,
        check_variants_override_coverage, decide_variant, extract_override_keys,
        validate_experiment, validate_override_keys,
    },
    types::{
        ApplicableVariantsQuery, AuditQueryFilters, ConcludeExperimentRequest,
        ContextAction, ContextBulkResponse, ContextMoveReq, ContextPutReq,
        ExperimentCreateRequest, ExperimentCreateResponse, ExperimentResponse,
        ExperimentsResponse, ListFilters, OverrideKeysUpdateRequest, RampRequest,
    },
};

use crate::db::{
    models::{EventLog, Experiment, ExperimentStatusType, Variant, Variants},
    schema::{event_log::dsl as event_log, experiments::dsl as experiments},
};

pub fn endpoints(scope: Scope) -> Scope {
    scope
        .service(get_audit_logs)
        .service(create)
        .service(conclude_handler)
        .service(list_experiments)
        .service(get_applicable_variants)
        .service(get_experiment_handler)
        .service(ramp)
        .service(update_overrides)
}

fn construct_header_map(
    tenant: &str,
    other_headers: Vec<(&str, String)>,
) -> superposition::Result<HeaderMap> {
    let mut headers = HeaderMap::new();
    let tenant_val = HeaderValue::from_str(tenant).map_err(|err| {
        log::error!("failed to set header: {}", err);
        unexpected_error!("Something went wrong")
    })?;
    headers.insert(HeaderName::from_static("x-tenant"), tenant_val);
    for (header, value) in other_headers {
        let header_name = HeaderName::from_str(header).map_err(|err| {
            log::error!("failed to set header: {}", err);
            unexpected_error!("Something went wrong")
        })?;

        HeaderValue::from_str(value.as_str())
            .map(|header_val| headers.insert(header_name, header_val))
            .map_err(|err| {
                log::error!("failed to set header: {}", err);
                unexpected_error!("Something went wrong")
            })?;
    }

    Ok(headers)
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

#[post("")]
async fn create(
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    req: web::Json<ExperimentCreateRequest>,
    db_conn: DbConnection,
    tenant: Tenant,
    user: User,
) -> superposition::Result<HttpResponse> {
    use crate::db::schema::experiments::dsl::experiments;
    let mut variants = req.variants.to_vec();
    let DbConnection(mut conn) = db_conn;

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
    let exp_context = Exp::<Condition>::try_from(req.context.clone())
        .map_err(|err| {
            log::error!("failed to decode condition with error {}", err);
            bad_argument!(err)
        })?
        .into_inner();

    // validating experiment against other active experiments based on permission flags
    let flags = &state.experimentation_flags;
    let (valid, reason) =
        validate_experiment(&exp_context, &unique_override_keys, None, flags, &mut conn)?;
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

    let headers_map = construct_header_map(tenant.as_str(), extra_headers)?;

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
        .filter_map(|item| match item {
            ContextBulkResponse::PUT(context) => Some(context),
            ContextBulkResponse::DELETE(_) => None,
            _ => {
                log::error!("Unexpected response item: {:?}", item);
                None
            }
        })
        .collect::<Vec<_>>();
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
        context: Value::Object(req.context.clone().into_inner().into()),
        variants: Variants::new(variants),
        last_modified_by: user.get_email(),
        chosen_variant: None,
    };

    let mut inserted_experiments = diesel::insert_into(experiments)
        .values(&new_experiment)
        .get_results(&mut conn)?;

    let inserted_experiment: Experiment = inserted_experiments.remove(0);
    let response = ExperimentCreateResponse::from(inserted_experiment);

    let mut http_resp = HttpResponse::Ok();
    add_config_version_to_header(&config_version_id, &mut http_resp);
    Ok(http_resp.json(response))
}

#[patch("/{experiment_id}/conclude")]
async fn conclude_handler(
    state: Data<AppState>,
    path: web::Path<i64>,
    custom_headers: CustomHeaders,
    req: web::Json<ConcludeExperimentRequest>,
    db_conn: DbConnection,
    tenant: Tenant,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(conn) = db_conn;
    let (response, config_version_id) = conclude(
        state,
        path.into_inner(),
        custom_headers.config_tags,
        req.into_inner(),
        conn,
        tenant,
        user,
    )
    .await?;
    let mut http_resp = HttpResponse::Ok();
    add_config_version_to_header(&config_version_id, &mut http_resp);
    Ok(http_resp.json(ExperimentResponse::from(response)))
}

pub async fn conclude(
    state: Data<AppState>,
    experiment_id: i64,
    config_tags: Option<String>,
    req: ConcludeExperimentRequest,
    mut conn: PooledConnection<ConnectionManager<PgConnection>>,
    tenant: Tenant,
    user: User,
) -> superposition::Result<(Experiment, Option<String>)> {
    use crate::db::schema::experiments::dsl;

    let winner_variant_id: String = req.chosen_variant.to_owned();

    let experiment: Experiment = dsl::experiments
        .find(experiment_id)
        .get_result::<Experiment>(&mut conn)?;

    if matches!(experiment.status, ExperimentStatusType::CONCLUDED) {
        return Err(bad_argument!(
            "experiment with id {} is already concluded",
            experiment_id
        ));
    }

    let experiment_context = experiment.context.as_object().ok_or_else(|| {
        log::error!("could not convert the context read from DB to JSON object");
        unexpected_error!("Something went wrong, failed to conclude experiment")
    })?;

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
                        ("x-tenant", tenant.as_str()),
                        (
                            "Authorization",
                            &format!("Internal {}", state.superposition_token),
                        ),
                        ("x-user", user_str.as_str()),
                    ])
                    .map_err(|err| {
                        superposition::AppError::UnexpectedError(anyhow!(err))
                    })?;

                    let _ =
                        request::<_, Value>(url, Method::PUT, Some(create_req), headers)
                            .await
                            .map_err(|err| {
                                superposition::AppError::UnexpectedError(anyhow!(err))
                            })?;
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

    let headers_map = construct_header_map(tenant.as_str(), extra_headers)?;

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
        ))
        .get_result::<Experiment>(&mut conn)?;

    Ok((updated_experiment, config_version_id))
}

#[get("/applicable-variants")]
async fn get_applicable_variants(
    db_conn: DbConnection,
    query_data: Query<ApplicableVariantsQuery>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let query_data = query_data.into_inner();

    let experiments = experiments::experiments
        .filter(experiments::status.ne(ExperimentStatusType::CONCLUDED))
        .load::<Experiment>(&mut conn)?;

    let experiments = experiments.into_iter().filter(|exp| {
        let is_empty = exp
            .context
            .as_object()
            .map_or(false, |context| context.is_empty());
        is_empty
            || jsonlogic::apply(&exp.context, &Value::Object(query_data.context.clone()))
                == Ok(Value::Bool(true))
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
    filters: Query<ListFilters>,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let max_event_timestamp: Option<NaiveDateTime> = event_log::event_log
        .filter(event_log::table_name.eq("experiments"))
        .select(diesel::dsl::max(event_log::timestamp))
        .first(&mut conn)?;

    let last_modified = req
        .headers()
        .get("If-Modified-Since")
        .and_then(|header_val| header_val.to_str().ok())
        .and_then(|header_str| {
            DateTime::parse_from_rfc2822(header_str)
                .map(|datetime| datetime.with_timezone(&Utc).naive_utc())
                .ok()
        });

    if max_event_timestamp.is_some() && max_event_timestamp < last_modified {
        return Ok(HttpResponse::NotModified().finish());
    };

    let query_builder = |filters: &ListFilters| {
        let mut builder = experiments::experiments.into_boxed();
        if let Some(states) = filters.status.clone() {
            builder = builder.filter(experiments::status.eq_any(states.0.clone()));
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

    let limit = filters.count.unwrap_or(10);
    let offset = (filters.page.unwrap_or(1) - 1) * limit;
    let query = base_query
        .order(experiments::last_modified.desc())
        .limit(limit)
        .offset(offset);

    let number_of_experiments = count_query.count().get_result(&mut conn)?;

    let experiment_list = query.load::<Experiment>(&mut conn)?;

    let total_pages = (number_of_experiments as f64 / limit as f64).ceil() as i64;

    Ok(HttpResponse::Ok().json(ExperimentsResponse {
        total_pages,
        total_items: number_of_experiments,
        data: experiment_list
            .into_iter()
            .map(ExperimentResponse::from)
            .collect(),
    }))
}

#[get("/{id}")]
async fn get_experiment_handler(
    params: web::Path<i64>,
    db_conn: DbConnection,
) -> superposition::Result<Json<ExperimentResponse>> {
    let DbConnection(mut conn) = db_conn;
    let response = get_experiment(params.into_inner(), &mut conn)?;
    Ok(Json(ExperimentResponse::from(response)))
}

pub fn get_experiment(
    experiment_id: i64,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Experiment> {
    use crate::db::schema::experiments::dsl::*;
    let result: Experiment = experiments
        .find(experiment_id)
        .get_result::<Experiment>(conn)?;

    Ok(result)
}

#[patch("/{id}/ramp")]
async fn ramp(
    params: web::Path<i64>,
    req: web::Json<RampRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<ExperimentResponse>> {
    let DbConnection(mut conn) = db_conn;
    let exp_id = params.into_inner();

    let experiment: Experiment = experiments::experiments
        .find(exp_id)
        .get_result::<Experiment>(&mut conn)?;

    let old_traffic_percentage = experiment.traffic_percentage as u8;
    let new_traffic_percentage = req.traffic_percentage as u8;
    let variants_count = experiment.variants.into_inner().len() as u8;
    let max = 100 / variants_count;

    if matches!(experiment.status, ExperimentStatusType::CONCLUDED) {
        return Err(bad_argument!(
            "experiment already concluded, cannot ramp a concluded experiment"
        ));
    } else if new_traffic_percentage > max {
        return Err(bad_argument!(
            "The traffic_percentage cannot exceed {}. Provide a traffic percentage less than {}", max, max
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
        ))
        .get_result(&mut conn)?;

    Ok(Json(ExperimentResponse::from(updated_experiment)))
}

#[put("/{id}/overrides")]
async fn update_overrides(
    params: web::Path<i64>,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    req: web::Json<OverrideKeysUpdateRequest>,
    tenant: Tenant,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let experiment_id = params.into_inner();

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
    let experiment_condition = Exp::<Condition>::try_from_db(
        experiment
            .context
            .as_object()
            .map_or_else(|| Map::new(), |ctx| ctx.clone()),
    )
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
        &mut conn,
    )?;
    if !valid {
        return Err(bad_argument!(reason));
    }

    /******************************* Updating contexts ************************************/
    let mut cac_operations: Vec<ContextAction> = vec![];

    // adding operations to remove exisiting variant contexts
    for existing_variant in experiment_variants {
        let context_id = existing_variant.context_id.ok_or_else(|| {
            log::error!(
                "context id not available for variant {:?}",
                existing_variant.id
            );
            unexpected_error!("Something went wrong, failed to update experiment")
        })?;
        cac_operations.push(ContextAction::DELETE(context_id.to_string()));
    }

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
        };
        cac_operations.push(ContextAction::PUT(payload));
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

    let headers_map = construct_header_map(tenant.as_str(), extra_headers)?;

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
        .filter_map(|item| match item {
            ContextBulkResponse::PUT(context) => Some(context),
            ContextBulkResponse::DELETE(_) => None,
            _ => {
                log::error!("Unexpected response item: {:?}", item);
                None
            }
        })
        .collect::<Vec<_>>();
    for i in 0..created_contexts.len() {
        let created_context = &created_contexts[i];

        new_variants[i].context_id = Some(created_context.context_id.clone());
        new_variants[i].override_id = Some(created_context.override_id.clone());
    }

    /*************************** Updating experiment in DB **************************/
    let new_variants_json = serde_json::to_value(new_variants).map_err(|e| {
        log::error!("failed to serialize new variants to json with error: {e}");
        bad_argument!("failed to update experiment, bad variant data")
    })?;
    let updated_experiment = diesel::update(experiments::experiments.find(experiment_id))
        .set((
            experiments::variants.eq(new_variants_json),
            experiments::override_keys.eq(override_keys),
            experiments::last_modified.eq(Utc::now()),
            experiments::last_modified_by.eq(user.get_email()),
        ))
        .get_result::<Experiment>(&mut conn)?;

    let mut http_resp = HttpResponse::Ok();
    add_config_version_to_header(&config_version_id, &mut http_resp);
    Ok(http_resp.json(ExperimentResponse::from(updated_experiment)))
}

#[get("/audit")]
async fn get_audit_logs(
    filters: Query<AuditQueryFilters>,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let query_builder = |filters: &AuditQueryFilters| {
        let mut builder = event_log::event_log.into_boxed();
        if let Some(tables) = filters.table.clone() {
            builder = builder.filter(event_log::table_name.eq_any(tables.0));
        }
        if let Some(actions) = filters.action.clone() {
            builder = builder.filter(event_log::action.eq_any(actions.0));
        }
        if let Some(username) = filters.username.clone() {
            builder = builder.filter(event_log::user_name.eq(username));
        }
        let now = Utc::now().naive_utc();
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

    Ok(HttpResponse::Ok().json(json!({
        "total_items": log_count,
        "total_pages": total_pages,
        "data": logs
    })))
}
