use actix_web::{
    get,
    http::StatusCode,
    post,
    web::{self, Data, Json, Query},
    Scope,
};
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use std::collections::HashMap;

use service_utils::service::types::{AppState, AuthenticationInfo};

use super::{
    helpers::{
        add_fields_to_json, check_variant_types, check_variants_override_coverage,
        validate_experiment,
    },
    types::{ExperimentCreateRequest, ExperimentCreateResponse},
};
use crate::{
    api::{errors::AppError, experiments::types::ListFilters},
    db::models::{Experiment, ExperimentStatusType, Experiments},
};

pub fn endpoints() -> Scope {
    Scope::new("/experiments")
        .service(create)
        .service(list_experiments)
}

#[post("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<ExperimentCreateRequest>,
    auth_info: AuthenticationInfo
) -> actix_web::Result<Json<ExperimentCreateResponse>, AppError> {
    use crate::db::schema::experiments::dsl::experiments;

    let override_keys = &req.override_keys;
    let mut variants = req.variants.to_vec();

    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            println!("Unable to get db connection from pool, error: {e}");
            return Err(AppError {
                message: "Could not connect to the database".to_string(),
                possible_fix: "Try after sometime".to_string(),
                status_code: StatusCode::INTERNAL_SERVER_ERROR,
            });
        }
    };

    // Checking if experiment has exactly 1 control variant, and
    // atleast 1 experimental variant
    match check_variant_types(&variants) {
        Err(e) => {
            return Err(AppError {
                message: e.to_string(),
                possible_fix: "".to_string(),
                status_code: StatusCode::BAD_REQUEST,
            })
        }
        _ => (),
    }

    // Checking if all the variants are overriding the mentioned keys
    let are_valid_variants = check_variants_override_coverage(&variants, override_keys);
    if !are_valid_variants {
        return Err(AppError {
            message: "all variants should contain the keys mentioned override_keys"
                .to_string(),
            possible_fix:
                "Try including all keys mentioned in override keys in variant overrides"
                    .to_string(),
            status_code: StatusCode::BAD_REQUEST,
        });
    }

    // Checking if context is a key-value pair map
    if !req.context.is_object() {
        return Err(AppError {
            message: "context should be map of key value pairs".to_string(),
            possible_fix: "".to_string(),
            status_code: StatusCode::BAD_REQUEST,
        });
    }

    //traffic_percentage should be max 100/length of variants
    // TODO: Add traffic_percentage validation

    // validating experiment against other active experiments based on permission flags
    let flags = &state.experimentation_flags;
    match validate_experiment(&req, &flags, &mut conn) {
        Ok(valid) => {
            if !valid {
                return Err(AppError {
                    message: "invalid experiment config".to_string(),
                    possible_fix: "".to_string(),
                    status_code: StatusCode::BAD_REQUEST,
                });
            }
        }
        Err(e) => {
            return Err(AppError {
                message: e.to_string(),
                possible_fix: "".to_string(),
                status_code: StatusCode::INTERNAL_SERVER_ERROR,
            });
        }
    }

    // generating snowflake id for experiment
    let mut snowflake_generator = state.snowflake_generator.lock().unwrap();
    let experiment_id = snowflake_generator.real_time_generate();

    //create overrides in CAC, if successfull then create experiment in DB
    for mut variant in &mut variants {
        let variant_id = experiment_id.to_string() + "-" + &variant.id;
        let fields_to_add =
            HashMap::from([("variant".to_string(), variant_id.to_string())]);

        let _updated_cacccontext = add_fields_to_json(&req.context, &fields_to_add);
        // call cac to send updated_context and req.overrides

        // updating variant.id to => experiment_id + variant.id
        variant.id = variant_id;
    }

    let AuthenticationInfo(email) = auth_info;
    let new_experiment = Experiment {
        id: experiment_id,
        created_by: email,
        created_at: Utc::now(),
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
            let inserted_experiment = inserted_experiments.remove(0);
            let response = ExperimentCreateResponse {
                message: "Created".to_string(),
                data: inserted_experiment,
            };

            return Ok(Json(response));
        }
        Err(e) => {
            println!("Experiment creation failed with error: {e}");
            return Err(AppError {
                message: "Failed to create experiment".to_string(),
                possible_fix: "".to_string(),
                status_code: StatusCode::INTERNAL_SERVER_ERROR,
            });
        }
    }
}

#[get("/list")]
async fn list_experiments(
    state: Data<AppState>,
    filters: Query<ListFilters>,
) -> actix_web::Result<Json<Experiments>, AppError> {
    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            println!("Unable to get db connection from pool, error: {e}");
            return Err(AppError {
                message: "Could not connect to the database".to_string(),
                possible_fix: "Try after sometime".to_string(),
                status_code: StatusCode::INTERNAL_SERVER_ERROR,
            });
            // return an error
        }
    };

    use crate::db::schema::experiments::dsl::*;
    let query = experiments
        .filter(status.eq_any(filters.status.clone()))
        .filter(created_at.ge(filters.from_date))
        .filter(created_at.le(filters.to_date))
        .limit(filters.count)
        .offset((filters.page - 1) * filters.count);

    // println!(
    //     "List filter query: {:?}",
    //     diesel::debug_query::<diesel::pg::Pg, _>(&query)
    // );
    let db_result = query.load::<Experiment>(&mut conn);

    match db_result {
        Ok(response) => return Ok(Json(response)),
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
