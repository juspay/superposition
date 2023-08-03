use actix_web::{
    get,
    http::StatusCode,
    patch, post,
    web::{self, Data, Json, Query},
    Scope,
};
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};

use service_utils::service::types::{AppState, AuthenticationInfo, DbConnection};

use super::{
    helpers::{
        add_variant_dimension_to_ctx, check_variant_types,
        check_variants_override_coverage, validate_experiment,
    },
    types::{
        ConcludeExperimentRequest, ContextAction, ContextPutReq, ContextPutResp,
        ExperimentCreateRequest, ExperimentCreateResponse, ExperimentResponse,
        ExperimentsResponse, RampRequest, Variant,
    },
};
use crate::{
    api::{errors::AppError, experiments::types::ListFilters},
    db::models::{Experiment, ExperimentStatusType},
};

pub fn endpoints() -> Scope {
    Scope::new("/experiments")
        .service(create)
        .service(conclude)
        .service(list_experiments)
        .service(get_experiment)
        .service(ramp)
}

#[post("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<ExperimentCreateRequest>,
    auth_info: AuthenticationInfo,
    db_conn: DbConnection,
) -> actix_web::Result<Json<ExperimentCreateResponse>> {
    use crate::db::schema::cac_v1::experiments::dsl::experiments;

    let DbConnection(mut conn) = db_conn;
    let override_keys = &req.override_keys;
    let mut variants = req.variants.to_vec();

    // Checking if experiment has exactly 1 control variant, and
    // atleast 1 experimental variant
    check_variant_types(&variants)
        .map_err(|e| actix_web::error::ErrorBadRequest(e.to_string()))?;

    // Checking if all the variants are overriding the mentioned keys
    let are_valid_variants = check_variants_override_coverage(&variants, override_keys);
    if !are_valid_variants {
        return Err(actix_web::error::ErrorBadRequest(
            "all variants should contain the keys mentioned override_keys".to_string(),
        ));
    }

    // Checking if context is a key-value pair map
    if !req.context.is_object() {
        return Err(actix_web::error::ErrorBadRequest(
            "context should be map of key value pairs".to_string(),
        ));
    }

    //traffic_percentage should be max 100/length of variants
    let allowed_traffic = (100 / variants.len()) as i32;
    if req.traffic_percentage > allowed_traffic {
        return Err(actix_web::error::ErrorBadRequest(format!(
            "the traffic percentage specified ({}) is greater than what can be evenly distributed among variants. Reduce the traffic percentage to a value <= {}",
            req.traffic_percentage, allowed_traffic
        )));
    }

    // validating experiment against other active experiments based on permission flags
    let flags = &state.experimentation_flags;
    match validate_experiment(&req, &flags, &mut conn) {
        Ok(valid) => {
            if !valid {
                return Err(actix_web::error::ErrorBadRequest(
                    "invalid experiment config".to_string(),
                ));
            }
        }
        Err(_) => {
            return Err(actix_web::error::ErrorInternalServerError(""));
        }
    }

    // generating snowflake id for experiment
    let mut snowflake_generator = state.snowflake_generator.lock().unwrap();
    let experiment_id = snowflake_generator.real_time_generate();

    //create overrides in CAC, if successfull then create experiment in DB
    let mut cac_operations: Vec<ContextAction> = vec![];
    for mut variant in &mut variants {
        let variant_id = experiment_id.to_string() + "-" + &variant.id;

        // updating variant.id to => experiment_id + variant.id
        variant.id = variant_id.to_string();

        let updated_cacccontext =
            add_variant_dimension_to_ctx(&req.context, variant_id.to_string())
                .map_err(|_| actix_web::error::ErrorInternalServerError(""))?;

        let payload = ContextPutReq {
            context: updated_cacccontext
                .as_object()
                .ok_or(actix_web::error::ErrorInternalServerError(""))?
                .clone(),
            r#override: variant.overrides.clone(),
        };
        cac_operations.push(ContextAction::PUT(payload));
    }

    // creating variants' context in CAC
    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/context/bulk-operations";

    let created_contexts: Vec<ContextPutResp> = http_client
        .put(&url)
        .bearer_auth(&state.admin_token)
        .json(&cac_operations)
        .send()
        .map_err(|e| {
            log::info!("failed to create contexts in cac: {e}");
            actix_web::error::ErrorInternalServerError("")
        })?
        .json::<Vec<ContextPutResp>>()
        .map_err(|e| {
            log::info!("failed to parse response: {e}");
            actix_web::error::ErrorInternalServerError("")
        })?;

    // updating variants with context and override ids
    for i in 0..created_contexts.len() {
        let created_context = &created_contexts[i];

        variants[i].context_id = Some(created_context.context_id.clone());
        variants[i].override_id = Some(created_context.override_id.clone());
    }

    // inserting experiment in db
    let AuthenticationInfo(email) = auth_info;
    let new_experiment = Experiment {
        id: experiment_id,
        created_by: email,
        created_at: Utc::now(),
        last_modified: Utc::now(),
        name: req.name.to_string(),
        override_keys: req.override_keys.to_vec(),
        traffic_percentage: req.traffic_percentage,
        status: ExperimentStatusType::CREATED,
        context: req.context.clone(),
        variants: serde_json::to_value(variants).unwrap(),
    };

    let insert = diesel::insert_into(experiments)
        .values(&new_experiment)
        .get_results(&mut conn);

    match insert {
        Ok(mut inserted_experiments) => {
            let inserted_experiment: Experiment = inserted_experiments.remove(0);
            let response = ExperimentCreateResponse::from(inserted_experiment);

            return Ok(Json(response));
        }
        Err(e) => {
            log::info!("Experiment creation failed with error: {e}");
            return Err(actix_web::error::ErrorInternalServerError(
                "Failed to create experiment".to_string(),
            ));
        }
    }
}

#[patch("/{experiment_id}/conclude")]
async fn conclude(
    state: Data<AppState>,
    path: web::Path<i64>,
    req: web::Json<ConcludeExperimentRequest>,
    db_conn: DbConnection,
    _auth_info: AuthenticationInfo,
) -> actix_web::Result<Json<ExperimentResponse>> {
    use crate::db::schema::cac_v1::experiments::dsl;

    let experiment_id: i64 = path.into_inner();
    let winner_variant_id: String = req.into_inner().winner_variant.to_owned();

    let DbConnection(mut conn) = db_conn;
    let db_result = dsl::experiments
        .find(experiment_id)
        .get_result::<Experiment>(&mut conn);

    let experiment = match db_result {
        Ok(response) => response,
        Err(diesel::result::Error::NotFound) => {
            return Err(actix_web::error::ErrorNotFound("experiment not found"));
        }
        Err(e) => {
            log::info!("failed to fetch experiment from db: {e}");
            return Err(actix_web::error::ErrorInternalServerError(
                "something went wrong.",
            ));
        }
    };

    if matches!(experiment.status, ExperimentStatusType::CONCLUDED) {
        return Err(actix_web::error::ErrorBadRequest(
            "experiment is already concluded",
        ));
    }

    let experiment_context = experiment
        .context
        .as_object()
        .ok_or(actix_web::error::ErrorInternalServerError(""))?;

    let mut operations: Vec<ContextAction> = vec![];
    let experiment_variants: Vec<Variant> = serde_json::from_value(experiment.variants)
        .map_err(|e| {
            log::error!("parsing to variant type failed with err: {e}");
            actix_web::error::ErrorInternalServerError("")
        })?;

    let mut is_valid_winner_variant = false;
    for variant in experiment_variants {
        let context_id = variant
            .context_id
            .ok_or(actix_web::error::ErrorInternalServerError(""))?;

        if variant.id == winner_variant_id {
            let context_put_req = ContextPutReq {
                context: experiment_context.clone(),
                r#override: variant.overrides,
            };

            is_valid_winner_variant = true;
            operations.push(ContextAction::MOVE((context_id, context_put_req)));
        } else {
            // delete this context
            operations.push(ContextAction::DELETE(context_id));
        }
    }

    if !is_valid_winner_variant {
        return Err(actix_web::error::ErrorNotFound("winner varaint not found"));
    }

    // calling CAC bulk api with operations as payload
    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/context/bulk-operations";
    let response = http_client
        .put(&url)
        .bearer_auth(&state.admin_token)
        .json(&operations)
        .send()
        .map_err(|e| {
            log::info!("Failed to update contexts in CAC: {e}");
            actix_web::error::ErrorInternalServerError("")
        })?;

    if !response.status().is_success() {
        return Err(actix_web::error::ErrorInternalServerError(""));
    }

    // updating experiment status in db
    let experiment_update_result = diesel::update(dsl::experiments)
        .filter(dsl::id.eq(experiment_id))
        .set((
            dsl::status.eq(ExperimentStatusType::CONCLUDED),
            dsl::last_modified.eq(Utc::now()),
        ))
        .get_result::<Experiment>(&mut conn);

    match experiment_update_result {
        Ok(updated_experiment) => {
            return Ok(Json(ExperimentResponse::from(updated_experiment)));
        }
        Err(e) => {
            log::info!("Failed to updated experiment status: {e}");
            return Err(actix_web::error::ErrorInternalServerError(""));
        }
    }
}

#[get("")]
async fn list_experiments(
    state: Data<AppState>,
    filters: Query<ListFilters>,
) -> actix_web::Result<Json<ExperimentsResponse>, AppError> {
    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            log::info!("Unable to get db connection from pool, error: {e}");
            return Err(AppError {
                message: "Something went wrong".to_string(),
                possible_fix: "Try again after some time".to_string(),
                status_code: StatusCode::INTERNAL_SERVER_ERROR,
            });
            // return an error
        }
    };

    use crate::db::schema::cac_v1::experiments::dsl::*;
    let query = experiments
        .filter(status.eq_any(filters.status.clone()))
        .filter(last_modified.ge(filters.from_date))
        .filter(last_modified.le(filters.to_date))
        .order(last_modified.desc())
        .limit(filters.count)
        .offset((filters.page - 1) * filters.count);

    let db_result = query.load::<Experiment>(&mut conn);

    match db_result {
        Ok(response) => {
            return Ok(Json(
                response
                    .into_iter()
                    .map(|entry| ExperimentResponse::from(entry))
                    .collect(),
            ))
        }
        Err(e) => {
            return Err(match e {
                diesel::result::Error::NotFound => AppError {
                    message: String::from("No results found"),
                    possible_fix: String::from("Update your filter parameters"),
                    status_code: StatusCode::NOT_FOUND,
                },
                _ => AppError {
                    message: String::from("Something went wrong"),
                    possible_fix: String::from("Please try again later"),
                    status_code: StatusCode::INTERNAL_SERVER_ERROR,
                },
            })
        }
    };
}

#[get("/{id}")]
async fn get_experiment(
    params: web::Path<i64>,
    db_conn: DbConnection,
) -> actix_web::Result<Json<ExperimentResponse>> {
    use crate::db::schema::cac_v1::experiments::dsl::*;

    let experiment_id = params.into_inner();
    let DbConnection(mut conn) = db_conn;

    let db_result = experiments
        .find(experiment_id)
        .get_result::<Experiment>(&mut conn);

    let response = match db_result {
        Ok(result) => ExperimentResponse::from(result),
        Err(diesel::result::Error::NotFound) => {
            return Err(actix_web::error::ErrorNotFound(
                "Experiment not found".to_string(),
            ));
        }
        Err(e) => {
            log::error!("{}", format!("get experiments failed due to : {e:?}"));
            return Err(actix_web::error::ErrorInternalServerError(""));
        }
    };

    return Ok(Json(response));
}

#[patch("/{id}/ramp")]
async fn ramp(
    params: web::Path<i64>,
    req: web::Json<RampRequest>,
    db_conn: DbConnection,
) -> actix_web::Result<Json<String>> {
    let DbConnection(mut conn) = db_conn;
    let exp_id = params.into_inner();

    use crate::db::schema::cac_v1::experiments::dsl::*;
    let db_result: Result<Experiment, _> =
        experiments.find(exp_id).get_result::<Experiment>(&mut conn);

    let experiment = match db_result {
        Ok(result) => result,
        Err(diesel::result::Error::NotFound) =>
            return Err(actix_web::error::ErrorNotFound("No results found")),
        Err(e) => {
            log::info!("{e}");
            return Err(actix_web::error::ErrorInternalServerError("Something went wrong"));
        }
    };

    let old_traffic_percentage = experiment.traffic_percentage as u8;
    let new_traffic_percentage = req.traffic_percentage as u8;
    let experiment_variants: Vec<Variant> = serde_json::from_value(experiment.variants)
        .map_err(|e| {
            log::error!("parsing to variant type failed with err: {e}");
            actix_web::error::ErrorInternalServerError("")
        })?;
    let variants_count = experiment_variants.len() as u8;
    let max = 100 / variants_count;

    if matches!(experiment.status, ExperimentStatusType::CONCLUDED) {
        return Err(actix_web::error::ErrorBadRequest(
            "Experiment is already concluded",
        ));
    } else if new_traffic_percentage > max {
        log::info!("The Traffic percentage provided exceeds the range");
        return Err(actix_web::error::ErrorBadRequest(
            format!("The traffic_percentage cannot exceed {}", max),
        ));
    } else if new_traffic_percentage == old_traffic_percentage {
        return Err(actix_web::error::ErrorBadRequest(
            "The traffic_percentage is same as provided",
        ));
    }

    let update = diesel::update(experiments)
        .filter(id.eq(exp_id))
        .set((
                traffic_percentage.eq(req.traffic_percentage as i32),
                last_modified.eq(Utc::now()),
            ))
        .execute(&mut conn);

    match update {
        Ok(0) => return Err(actix_web::error::ErrorInternalServerError("Failed to update the traffic_percentage")),
        Ok(_) => return Ok(Json(format!("Traffic percentage has been updated for the experiment id : {}", exp_id))),
        Err(e)   => {
            log::info!("Failed to update the traffic_percentage: {e}");
            return Err(actix_web::error::ErrorInternalServerError("Failed to update the traffic_percentage"));
        }
    }
}

