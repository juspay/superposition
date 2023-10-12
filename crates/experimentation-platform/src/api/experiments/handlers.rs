use std::collections::{HashMap, HashSet};

use actix_web::{
    get, patch, post, put,
    web::{self, Data, Json, Query},
    HttpRequest, HttpResponse, Scope,
};
use chrono::{DateTime, Duration, NaiveDateTime, Utc};
use dashboard_auth::types::User;
use diesel::{
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};

use service_utils::{
    errors::types::{Error as err, ErrorResponse},
    service::types::{AppState, DbConnection, Tenant},
    types as app,
};

use super::{
    helpers::{
        add_variant_dimension_to_ctx, check_variant_types,
        check_variants_override_coverage, extract_override_keys, validate_experiment,
        validate_override_keys,
    },
    types::{
        AuditQueryFilters, ConcludeExperimentRequest, ContextAction, ContextBulkResponse,
        ContextMoveReq, ContextPutReq, ContextPutResp, ExperimentCreateRequest,
        ExperimentCreateResponse, ExperimentResponse, ExperimentsResponse, ListFilters,
        OverrideKeysUpdateRequest, RampRequest, Variant,
    },
};

use crate::{
    db::models::{EventLog, Experiment, ExperimentStatusType},
    db::schema::{event_log::dsl as event_log, experiments::dsl as experiments},
};

use serde_json::{json, Map, Value};

pub fn endpoints(scope: Scope) -> Scope {
    scope
        .service(get_audit_logs)
        .service(create)
        .service(conclude_handler)
        .service(list_experiments)
        .service(get_experiment_handler)
        .service(ramp)
        .service(update_overrides)
}

#[post("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<ExperimentCreateRequest>,
    user: User,
    db_conn: DbConnection,
    tenant: Tenant,
) -> app::Result<Json<ExperimentCreateResponse>> {
    use crate::db::schema::experiments::dsl::experiments;
    let mut variants = req.variants.to_vec();

    let DbConnection(mut conn) = db_conn;

    // Checking if experiment has exactly 1 control variant, and
    // atleast 1 experimental variant
    check_variant_types(&variants)?;
    let unique_override_keys: Vec<String> = extract_override_keys(&variants[0].overrides)
        .into_iter()
        .collect();

    let unique_ids_of_variants_from_req: HashSet<&str> =
        HashSet::from_iter(variants.iter().map(|v| v.id.as_str()));

    if unique_ids_of_variants_from_req.len() != variants.len() {
        return Err(err::BadRequest(ErrorResponse {
            message: "variant ids are expected to be unique".to_string(),
            possible_fix: "provide unqiue variant IDs".to_string(),
        }));
    }

    validate_override_keys(&unique_override_keys)?;

    // Checking if all the variants are overriding the mentioned keys
    let variant_overrides = variants
        .iter()
        .map(|variant| &variant.overrides)
        .collect::<Vec<&Map<String, Value>>>();
    let are_valid_variants =
        check_variants_override_coverage(&variant_overrides, &unique_override_keys);
    if !are_valid_variants {
        return Err(err::BadRequest(ErrorResponse {
            message: "all variants should contain the keys mentioned in override_keys"
                .to_string(),
            possible_fix: format!("Check if any of the following keys [{}] are missing from keys in your variants",  unique_override_keys.join(","))
        }));
    }

    // Checking if context is a key-value pair map
    if !req.context.is_object() {
        return Err(err::BadRequest(ErrorResponse {
            message: "context should be map of key value pairs".to_string(),
            possible_fix: "Please refer documentation or contact an admin".to_string(),
        }));
    }

    // validating experiment against other active experiments based on permission flags
    let flags = &state.experimentation_flags;
    let (valid, reason) = validate_experiment(
        &req.context,
        &unique_override_keys,
        None,
        &flags,
        &mut conn,
    )?;
    if !valid {
        return Err(err::BadRequest(ErrorResponse {
            message: reason,
            possible_fix: "Please refer documentation or contact an admin".to_string(),
        }));
    }

    // generating snowflake id for experiment
    let mut snowflake_generator = state.snowflake_generator.lock().unwrap();
    let experiment_id = snowflake_generator.real_time_generate();

    //create overrides in CAC, if successfull then create experiment in DB
    let mut cac_operations: Vec<ContextAction> = vec![];
    for variant in &mut variants {
        let variant_id = experiment_id.to_string() + "-" + &variant.id;

        // updating variant.id to => experiment_id + variant.id
        variant.id = variant_id.to_string();

        let updated_cacccontext =
            add_variant_dimension_to_ctx(&req.context, variant_id.to_string())?;

        let payload = ContextPutReq {
            context: updated_cacccontext
                .as_object()
                .ok_or(err::InternalServerErr(
                    "Could not convert updated CAC context to serde Object".to_string(),
                ))?
                .clone(),
            r#override: json!(variant.overrides),
        };
        cac_operations.push(ContextAction::PUT(payload));
    }

    // creating variants' context in CAC
    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/context/bulk-operations";

    let created_contexts: Vec<ContextPutResp> = http_client
        .put(&url)
        .header("Authorization", format!("Bearer {}", user.token))
        .header("x-tenant", tenant.as_str())
        .json(&cac_operations)
        .send()
        .await
        .map_err(|e| err::InternalServerErr(e.to_string()))?
        .json::<Vec<ContextBulkResponse>>()
        .await
        .map_err(|e| err::InternalServerErr(e.to_string()))?
        .into_iter()
        .fold(
            Vec::<ContextPutResp>::new(),
            |mut put_responses, response| {
                if let ContextBulkResponse::PUT(created_context) = response {
                    put_responses.push(created_context);
                } else {
                    log::error!(
                        "unexpected response from cac for create only request: {:?}",
                        response
                    );
                }
                put_responses
            },
        );

    // updating variants with context and override ids
    for i in 0..created_contexts.len() {
        let created_context = &created_contexts[i];

        variants[i].context_id = Some(created_context.context_id.clone());
        variants[i].override_id = Some(created_context.override_id.clone());
    }

    // inserting experiment in db
    let new_experiment = Experiment {
        id: experiment_id,
        created_by: user.email.to_string(),
        created_at: Utc::now(),
        last_modified: Utc::now(),
        name: req.name.to_string(),
        override_keys: unique_override_keys.to_vec(),
        traffic_percentage: 0,
        status: ExperimentStatusType::CREATED,
        context: req.context.clone(),
        variants: serde_json::to_value(variants).unwrap(),
        last_modified_by: user.email,
        chosen_variant: None,
    };

    let mut inserted_experiments = diesel::insert_into(experiments)
        .values(&new_experiment)
        .get_results(&mut conn)?;

    let inserted_experiment: Experiment = inserted_experiments.remove(0);
    let response = ExperimentCreateResponse::from(inserted_experiment);

    return Ok(Json(response));
}

#[patch("/{experiment_id}/conclude")]
async fn conclude_handler(
    state: Data<AppState>,
    path: web::Path<i64>,
    req: web::Json<ConcludeExperimentRequest>,
    db_conn: DbConnection,
    user: User,
    tenant: Tenant,
) -> app::Result<Json<ExperimentResponse>> {
    let DbConnection(conn) = db_conn;
    let response = conclude(
        state,
        path.into_inner(),
        req.into_inner(),
        conn,
        user,
        tenant,
    )
    .await?;
    return Ok(Json(ExperimentResponse::from(response)));
}

pub async fn conclude(
    state: Data<AppState>,
    experiment_id: i64,
    req: ConcludeExperimentRequest,
    mut conn: PooledConnection<ConnectionManager<PgConnection>>,
    user: User,
    tenant: Tenant,
) -> app::Result<Experiment> {
    use crate::db::schema::experiments::dsl;

    let winner_variant_id: String = req.chosen_variant.to_owned();

    let experiment: Experiment = dsl::experiments
        .find(experiment_id)
        .get_result::<Experiment>(&mut conn)?;

    if matches!(experiment.status, ExperimentStatusType::CONCLUDED) {
        return Err(err::BadRequest(ErrorResponse {
            message: format!("experiment with id {} is already concluded", experiment_id),
            possible_fix: "Try to conclude a different experiment".to_string(),
        }));
    }

    let experiment_context =
        experiment
            .context
            .as_object()
            .ok_or(err::InternalServerErr(
                "Could not convert the context read from DB to JSON object".to_string(),
            ))?;

    let mut operations: Vec<ContextAction> = vec![];
    let experiment_variants: Vec<Variant> =
        serde_json::from_value(experiment.variants)
            .map_err(|e| err::InternalServerErr(e.to_string()))?;

    let mut is_valid_winner_variant = false;
    for variant in experiment_variants {
        let context_id = variant.context_id.ok_or(err::InternalServerErr(
            "Could not read context ID from experiment".to_string(),
        ))?;

        if variant.id == winner_variant_id {
            let context_move_req = ContextMoveReq {
                context: experiment_context.clone(),
            };

            is_valid_winner_variant = true;

            operations.push(ContextAction::MOVE((context_id, context_move_req)));
        } else {
            // delete this context
            operations.push(ContextAction::DELETE(context_id));
        }
    }

    if !is_valid_winner_variant {
        return Err(err::NotFound(ErrorResponse {
            message: "winner variant not found".to_string(),
            possible_fix:
                "A wrong variant ID may have been sent, please check and try again"
                    .to_string(),
        }));
    }

    // calling CAC bulk api with operations as payload
    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/context/bulk-operations";
    let response = http_client
        .put(&url)
        .header("Authorization", format!("Bearer {}", user.token))
        .header("x-tenant", tenant.as_str())
        .json(&operations)
        .send()
        .await
        .map_err(|e| err::InternalServerErr(e.to_string()))?;

    if !response.status().is_success() {
        return Err(err::InternalServerErr(format!(
            "Request to {} failed with response: {:?}",
            url, response
        )));
    }

    // updating experiment status in db
    let updated_experiment = diesel::update(dsl::experiments)
        .filter(dsl::id.eq(experiment_id))
        .set((
            dsl::status.eq(ExperimentStatusType::CONCLUDED),
            dsl::last_modified.eq(Utc::now()),
            dsl::last_modified_by.eq(user.email),
            dsl::chosen_variant.eq(Some(winner_variant_id)),
        ))
        .get_result::<Experiment>(&mut conn)?;

    return Ok(updated_experiment);
}

#[get("")]
async fn list_experiments(
    req: HttpRequest,
    filters: Query<ListFilters>,
    db_conn: DbConnection,
) -> app::Result<HttpResponse> {
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
        total_items: number_of_experiments,
        total_pages: total_pages,
        data: experiment_list
            .into_iter()
            .map(|entry| ExperimentResponse::from(entry))
            .collect(),
    }))
}

#[get("/{id}")]
async fn get_experiment_handler(
    params: web::Path<i64>,
    db_conn: DbConnection,
) -> app::Result<Json<ExperimentResponse>> {
    let DbConnection(mut conn) = db_conn;
    let response = get_experiment(params.into_inner(), &mut conn)?;
    return Ok(Json(ExperimentResponse::from(response)));
}

pub fn get_experiment(
    experiment_id: i64,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> app::Result<Experiment> {
    use crate::db::schema::experiments::dsl::*;
    let result: Experiment = experiments
        .find(experiment_id)
        .get_result::<Experiment>(conn)?;

    return Ok(result);
}

#[patch("/{id}/ramp")]
async fn ramp(
    params: web::Path<i64>,
    req: web::Json<RampRequest>,
    db_conn: DbConnection,
    user: User,
) -> app::Result<Json<ExperimentResponse>> {
    let DbConnection(mut conn) = db_conn;
    let exp_id = params.into_inner();

    let experiment: Experiment = experiments::experiments
        .find(exp_id)
        .get_result::<Experiment>(&mut conn)?;

    let old_traffic_percentage = experiment.traffic_percentage as u8;
    let new_traffic_percentage = req.traffic_percentage as u8;
    let experiment_variants: Vec<Variant> =
        serde_json::from_value(experiment.variants)
            .map_err(|e| err::InternalServerErr(e.to_string()))?;
    let variants_count = experiment_variants.len() as u8;
    let max = 100 / variants_count;

    if matches!(experiment.status, ExperimentStatusType::CONCLUDED) {
        return Err(err::BadRequest(ErrorResponse {
            message: "Experiment is already concluded".to_string(),
            possible_fix: "".to_string(),
        }));
    } else if new_traffic_percentage > max {
        return Err(err::BadRequest(ErrorResponse {
            message: format!("The traffic_percentage cannot exceed {}", max),
            possible_fix: format!("Provide a traffic percentage less than {}", max),
        }));
    } else if new_traffic_percentage == old_traffic_percentage {
        return Err(err::BadRequest(ErrorResponse {
            message: "The traffic_percentage is same as provided".to_string(),
            possible_fix: "".to_string(),
        }));
    }
    let updated_experiment: Experiment = diesel::update(experiments::experiments)
        .filter(experiments::id.eq(exp_id))
        .set((
            experiments::traffic_percentage.eq(req.traffic_percentage as i32),
            experiments::last_modified.eq(Utc::now()),
            experiments::last_modified_by.eq(user.email),
            experiments::status.eq(ExperimentStatusType::INPROGRESS),
        ))
        .get_result(&mut conn)?;

    return Ok(Json(ExperimentResponse::from(updated_experiment)));
}

#[put("/{id}/overrides")]
async fn update_overrides(
    params: web::Path<i64>,
    state: Data<AppState>,
    db_conn: DbConnection,
    user: User,
    req: web::Json<OverrideKeysUpdateRequest>,
    tenant: Tenant,
) -> app::Result<Json<ExperimentResponse>> {
    let DbConnection(mut conn) = db_conn;
    let experiment_id = params.into_inner();

    let payload = req.into_inner();
    let variants = payload.variants;

    let first_variant = variants.get(0).ok_or(err::BadRequest(ErrorResponse {
        message: "Variant not found in request".to_string(),
        possible_fix: "Provide at least one entry in variant's list".to_string(),
    }))?;
    let override_keys = extract_override_keys(&first_variant.overrides)
        .into_iter()
        .collect();

    // fetch the current variants of the experiment
    let experiment = experiments::experiments
        .find(experiment_id)
        .first::<Experiment>(&mut conn)?;

    if experiment.status != ExperimentStatusType::CREATED {
        return Err(err::BadRequest(ErrorResponse {
            message: "Only experiments in CREATED state can be updated".to_string(),
            possible_fix: "Please refer the documentation or contact an admin"
                .to_string(),
        }));
    }

    let experiment_variants: Vec<Variant> = serde_json::from_value(experiment.variants)
        .map_err(|e| {
        err::InternalServerErr(
            format!("Failed to parse exisitng variants: {e}").to_string(),
        )
    })?;

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
            .map(|variant| (*variant).id.to_string())
            .collect::<Vec<String>>(),
    );
    for existing_id in id_to_existing_variant.keys() {
        if !variant_ids.contains(existing_id) {
            return Err(err::BadRequest(ErrorResponse {
                message:
                    "some variant ids do not match with exisiting experiment variants"
                        .to_string(),
                possible_fix: "provide all existing variants of the experiment"
                    .to_string(),
            }));
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
        .map(|variant| &variant.overrides)
        .collect::<Vec<&Map<String, Value>>>();
    let are_valid_variants =
        check_variants_override_coverage(&variant_overrides, &override_keys);
    if !are_valid_variants {
        return Err(err::BadRequest(ErrorResponse {
            message: "all variants should contain the keys mentioned in override_keys"
                .to_string(),
            possible_fix: format!("Check if any of the following keys [{}] are missing from keys in your variants",  override_keys.join(","))
        }));
    }

    // validating experiment against other active experiments based on permission flags
    let flags = &state.experimentation_flags;
    let (valid, reason) = validate_experiment(
        &experiment.context,
        &override_keys,
        Some(experiment_id),
        &flags,
        &mut conn,
    )?;
    if !valid {
        return Err(err::BadRequest(ErrorResponse {
            message: reason,
            possible_fix: "Please refer documentation or contact an admin".to_string(),
        }));
    }

    /******************************* Updating contexts ************************************/
    let mut cac_operations: Vec<ContextAction> = vec![];

    // adding operations to remove exisiting variant contexts
    for existing_variant in experiment_variants {
        let context_id = existing_variant
            .context_id
            .ok_or(format!(
                "Context Id not available for variant {:?}",
                existing_variant.id
            ))
            .map_err(|e| err::InternalServerErr(e))?;
        cac_operations.push(ContextAction::DELETE(context_id.to_string()));
    }

    // adding operations to create new updated variant contexts
    for variant in &mut new_variants {
        let updated_cacccontext =
            add_variant_dimension_to_ctx(&experiment.context, variant.id.to_string())
                .map_err(|e| {
                    err::InternalServerErr(
                        format!("failed to add variant dimension to context: {e}")
                            .to_string(),
                    )
                })?;

        let payload = ContextPutReq {
            context: updated_cacccontext
                .as_object()
                .ok_or(err::InternalServerErr(
                    "failed to parse updated context with variant dimension".to_string(),
                ))?
                .clone(),
            r#override: json!(variant.overrides),
        };
        cac_operations.push(ContextAction::PUT(payload));
    }

    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/context/bulk-operations";

    let created_contexts: Vec<ContextPutResp> = http_client
        .put(&url)
        .header("Authorization", format!("Bearer {}", user.token))
        .header("x-tenant", tenant.as_str())
        .json(&cac_operations)
        .send()
        .await
        .map_err(|e| err::InternalServerErr(e.to_string()))?
        .json::<Vec<ContextBulkResponse>>()
        .await
        .map_err(|e| err::InternalServerErr(e.to_string()))?
        .into_iter()
        .fold(
            Vec::<ContextPutResp>::new(),
            |mut put_responses, response| {
                if let ContextBulkResponse::PUT(created_context) = response {
                    put_responses.push(created_context);
                }
                put_responses
            },
        );

    /*************************** Updating experiment in DB **************************/

    for i in 0..created_contexts.len() {
        let created_context = &created_contexts[i];

        new_variants[i].context_id = Some(created_context.context_id.clone());
        new_variants[i].override_id = Some(created_context.override_id.clone());
    }

    let new_variants_json = serde_json::to_value(new_variants).map_err(|e| {
        err::InternalServerErr(format!("failed to convert new variants to json: {e}"))
    })?;
    let updated_experiment = diesel::update(experiments::experiments.find(experiment_id))
        .set((
            experiments::variants.eq(new_variants_json),
            experiments::override_keys.eq(override_keys),
            experiments::last_modified.eq(Utc::now()),
            experiments::last_modified_by.eq(user.email),
        ))
        .get_result::<Experiment>(&mut conn)?;

    return Ok(Json(ExperimentResponse::from(updated_experiment)));
}

#[get("/audit")]
async fn get_audit_logs(
    filters: Query<AuditQueryFilters>,
    db_conn: DbConnection,
) -> app::Result<HttpResponse> {
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
