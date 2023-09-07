use std::collections::HashSet;

use actix_web::{
    get, patch, post,
    web::{self, Data, Json, Query},
    HttpRequest, HttpResponse, Scope,
};
use chrono::{DateTime, Duration, NaiveDateTime, Utc};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};

use service_utils::{
    errors::types::{Error as err, ErrorResponse},
    service::types::{AppState, AuthenticationInfo, DbConnection},
    types as app,
};

use super::{
    helpers::{
        add_variant_dimension_to_ctx, check_variant_types,
        check_variants_override_coverage, validate_experiment,
    },
    types::{
        ConcludeExperimentRequest, ContextAction, ContextPutReq, ContextPutResp,
        ExperimentCreateRequest, ExperimentCreateResponse, ExperimentResponse,
        ExperimentsResponse, ListFilters, RampRequest, Variant,
    },
};

use crate::{
    db::models::{Experiment, ExperimentStatusType},
    db::schema::cac_v1::{event_log::dsl as event_log, experiments::dsl as experiments},
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
) -> app::Result<Json<ExperimentCreateResponse>> {
    use crate::db::schema::cac_v1::experiments::dsl::experiments;
    let mut variants = req.variants.to_vec();

    let DbConnection(mut conn) = db_conn;
    let override_keys = &req.override_keys;

    let unique_ids_of_variants_from_req: HashSet<&str> =
        HashSet::from_iter(variants.iter().map(|v| v.id.as_str()));

    if unique_ids_of_variants_from_req.len() != variants.len() {
        return Err(err::BadRequest(ErrorResponse {
            message: "variant ids are expected to be unique".to_string(),
            possible_fix: "provide unqiue variant IDs".to_string(),
        }));
    }

    // Checking if experiment has exactly 1 control variant, and
    // atleast 1 experimental variant
    check_variant_types(&variants)?;

    // Checking if all the variants are overriding the mentioned keys
    let are_valid_variants = check_variants_override_coverage(&variants, override_keys);
    if !are_valid_variants {
        return Err(err::BadRequest(ErrorResponse {
            message: "all variants should contain the keys mentioned in override_keys"
                .to_string(),
            possible_fix: format!("Check if any of the following keys [{}] are missing from keys in your variants",  override_keys.join(","))
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
    let (valid, reason) = validate_experiment(&req, &flags, &mut conn)?;
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
    for mut variant in &mut variants {
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
        .await
        .map_err(|e| err::InternalServerErr(e.to_string()))?
        .json::<Vec<ContextPutResp>>()
        .await
        .map_err(|e| err::InternalServerErr(e.to_string()))?;

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
        created_by: email.to_string(),
        created_at: Utc::now(),
        last_modified: Utc::now(),
        name: req.name.to_string(),
        override_keys: req.override_keys.to_vec(),
        traffic_percentage: 0,
        status: ExperimentStatusType::CREATED,
        context: req.context.clone(),
        variants: serde_json::to_value(variants).unwrap(),
        last_modified_by: email,
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
async fn conclude(
    state: Data<AppState>,
    path: web::Path<i64>,
    req: web::Json<ConcludeExperimentRequest>,
    db_conn: DbConnection,
    auth_info: AuthenticationInfo,
) -> app::Result<Json<ExperimentResponse>> {
    use crate::db::schema::cac_v1::experiments::dsl;

    let experiment_id: i64 = path.into_inner();
    let winner_variant_id: String = req.into_inner().chosen_variant.to_owned();

    let DbConnection(mut conn) = db_conn;
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
        .bearer_auth(&state.admin_token)
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

    let AuthenticationInfo(email) = auth_info;

    // updating experiment status in db
    let updated_experiment = diesel::update(dsl::experiments)
        .filter(dsl::id.eq(experiment_id))
        .set((
            dsl::status.eq(ExperimentStatusType::CONCLUDED),
            dsl::last_modified.eq(Utc::now()),
            dsl::last_modified_by.eq(email),
            dsl::chosen_variant.eq(Some(winner_variant_id))
        ))
        .get_result::<Experiment>(&mut conn)?;

    return Ok(Json(ExperimentResponse::from(updated_experiment)));
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
                experiments::last_modified.ge(filters.from_date.unwrap_or(now - Duration::hours(24))),
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
async fn get_experiment(
    params: web::Path<i64>,
    db_conn: DbConnection,
) -> app::Result<Json<ExperimentResponse>> {
    use crate::db::schema::cac_v1::experiments::dsl::*;

    let experiment_id = params.into_inner();
    let DbConnection(mut conn) = db_conn;

    let result: Experiment = experiments
        .find(experiment_id)
        .get_result::<Experiment>(&mut conn)?;

    return Ok(Json(ExperimentResponse::from(result)));
}

#[patch("/{id}/ramp")]
async fn ramp(
    params: web::Path<i64>,
    req: web::Json<RampRequest>,
    db_conn: DbConnection,
    auth_info: AuthenticationInfo,
) -> app::Result<Json<String>> {
    let DbConnection(mut conn) = db_conn;
    let exp_id = params.into_inner();

    use crate::db::schema::cac_v1::experiments::dsl::*;
    let experiment: Experiment = experiments
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
    let AuthenticationInfo(email) = auth_info;

    let new_traffic_percentage = diesel::update(experiments)
        .filter(id.eq(exp_id))
        .set((
            traffic_percentage.eq(req.traffic_percentage as i32),
            last_modified.eq(Utc::now()),
            last_modified_by.eq(email),
            status.eq(ExperimentStatusType::INPROGRESS),
        ))
        .execute(&mut conn)?;

    if new_traffic_percentage == 0 {
        return Err(err::InternalServerErr(
            "Failed to update the traffic_percentage".to_string(),
        ));
    }
    return Ok(Json(format!(
        "Traffic percentage has been updated for the experiment id : {}",
        exp_id
    )));
}