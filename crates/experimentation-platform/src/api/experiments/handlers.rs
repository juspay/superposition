use actix_web::{
    get,
    http::StatusCode,
    post,
    web::{self, Data, Json, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::pg::PgConnection;
use diesel::{sql_query, QueryResult, RunQueryDsl};
use serde_json::Value;
use std::collections::{HashMap, HashSet};

use service_utils::service::types::AppState;

use super::types::ExperimentCreateReq;
use crate::{
    api::{errors::AppError, experiments::types::ListFilters},
    db::models::{Experiment, ExperimentStatusType, Experiments},
};

pub fn endpoints() -> Scope {
    Scope::new("/experiments").service(create)
}

fn get_active_experiments(conn: &mut PgConnection) -> QueryResult<Vec<Experiment>> {
    let active_experiments = sql_query(
        "SELECT * FROM experiments WHERE status = 'CREATED' OR status = 'INPROGRESS'",
    );
    return active_experiments.load(conn);
}

fn are_overlapping_contexts(context_a_json: &Value, context_b_json: &Value) -> bool {
    // TODO: pattern match conversion to object instead of throwing err
    let context_a = context_a_json.as_object().expect("contextA not an object");
    let context_b = context_b_json.as_object().expect("contextB not an object");

    let context_a_keys = context_a.keys();
    let context_b_keys = context_b.keys();

    let ref_keys = if context_a_keys.len() > context_b_keys.len() {
        context_b_keys
    } else {
        context_a_keys
    };

    let mut is_overlapping = true;
    for key in ref_keys {
        let test = (context_a.contains_key(key) && context_b.contains_key(key))
            && (context_a[key] == context_b[key]);
        is_overlapping = is_overlapping && test;

        if !test {
            break;
        }
    }

    is_overlapping
}

pub fn add_fields_to_json(json: &Value, fields: &HashMap<String, String>) -> Value {
    match json {
        Value::Object(m) => {
            let mut m = m.clone();
            for (k, v) in fields {
                m.insert(k.clone(), Value::String(v.clone()));
            }
            Value::Object(m)
        }

        v => v.clone(),
    }
}

#[post("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<ExperimentCreateReq>,
) -> HttpResponse {
    use crate::db::schema::experiments::dsl::experiments;

    let override_keys = &req.override_keys;
    let variants = req.variants.to_vec();

    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            println!("unable to get db connection from pool, error: {e}");
            return HttpResponse::InternalServerError().finish();
        }
    };

    // Checking if all the variants are overriding the mentioned keys
    for variant in &variants {
        let overrides = &variant.overrides;
        let mut is_valid_variant = false;

        for override_key in override_keys {
            let has_override_key = match overrides[override_key] {
                Value::Null => false,
                _ => true,
            };
            is_valid_variant = is_valid_variant && has_override_key;
        }

        if !is_valid_variant {
            return HttpResponse::BadRequest()
                    .json(
                        "{\"message\" : \"all variants should contain the keys mentioned override_keys\"}"
                    );
        }
    }

    //traffic_percentage should be max 100/length of variants
    //read the envs related to falgs to check the overlapping
    let flags = &state.experimentation_flags;

    //check for overlapping context experiments
    if !req.context.is_object() {
        return HttpResponse::BadRequest()
            .json("{\"message\": \"context should be a map of key value pairs\" }");
    }

    // let context_obj = req.context.as_object().expect("context is not a map");
    let active_experiments: Vec<Experiment> =
        get_active_experiments(&mut conn).expect("Failed to get active experiments!");

    let mut valid_experiment = true;
    if !flags.allow_same_keys_overlapping_ctx
        || !flags.allow_diff_keys_overlapping_ctx
        || !flags.allow_same_keys_non_overlapping_ctx
    {
        let override_keys_set: HashSet<_> = req.override_keys.iter().collect();
        for active_experiment in active_experiments.iter() {
            let are_overlapping =
                are_overlapping_contexts(&req.context, &active_experiment.context);

            let have_intersecting_key_set = active_experiment
                .override_keys
                .iter()
                .any(|key| override_keys_set.contains(key));

            if !flags.allow_diff_keys_overlapping_ctx {
                valid_experiment = valid_experiment && !are_overlapping;
            }
            if !flags.allow_same_keys_overlapping_ctx {
                valid_experiment =
                    valid_experiment && (are_overlapping && !have_intersecting_key_set);
            }
            if !flags.allow_same_keys_non_overlapping_ctx {
                valid_experiment =
                    valid_experiment && (!are_overlapping && !have_intersecting_key_set);
            }
        }
    }

    if !valid_experiment {
        return HttpResponse::BadRequest().json("invalid experiment config");
    }

    // create id for experiment
    let mut snowflake_generator = state.snowflake_generator.lock().unwrap();
    let experiment_id = snowflake_generator.real_time_generate();

    //create overrides in CAC, if successfull then create experiment in DB

    for mut variant in variants {
        let variant_id = experiment_id.to_string() + &variant.id;
        let fields_to_add =
            HashMap::from([("variant".to_string(), variant_id.to_string())]);

        let updated_context = add_fields_to_json(&req.context, &fields_to_add);
        // call cac to send updated_context and req.overrides

        // update variant.id to => experiment_id + variant.id
        variant.id = variant_id;
    }

    let new_experiment = Experiment {
        id: experiment_id,
        created_by: "NA".to_string(),
        created_at: Utc::now(),

        name: req.name.to_string(),
        override_keys: req.override_keys.to_vec(),
        traffic_percentage: req.traffic_percentage,

        status: ExperimentStatusType::CREATED,

        context: req.context.clone(),
        variants: serde_json::to_value(&req.variants).unwrap(),
    };

    let insert = diesel::insert_into(experiments)
        .values(&new_experiment)
        .execute(&mut conn);

    match insert {
        Ok(_) => return HttpResponse::Created().json(new_experiment),
        Err(e) => {
            println!("Experiment creation failed with error: {e}");
            return HttpResponse::InternalServerError()
                .body("Failed to create experiment\n");
        }
    }
}

#[get("/list")]
async fn list_experiments(
    state: Data<AppState>,
    filters: Query<ListFilters>,
) -> actix_web::Result<Json<Experiments>, AppError> {
    let conn = match state.db_pool.get() {
        Ok(conn) => &mut conn,
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

    let db_result = exp
        .filter(status.eq_any(filters.status))
        .filter(created_at.ge(filters.from_date))
        .filter(created_at.le(filters.to_date))
        .limit(filters.count)
        .offset((filters.page - 1) * filters.count)
        .load(conn);

    let response = match db_result {
        Ok(result) => Json(result),
        Err(e) => {
            return Err(match e {
                NotFound => AppError {
                    message: String::from("No results found"),
                    possible_fix: String::from("Update your filter parameters"),
                    status_code: StatusCode::NOT_FOUND,
                },
                DatabaseError => AppError {
                    message: String::from("Something went wrong"),
                    possible_fix: String::from("Please try again later"),
                    status_code: StatusCode::INTERNAL_SERVER_ERROR,
                },
            })
        }
    };

    return Ok(response);
}
