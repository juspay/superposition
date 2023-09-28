use actix_web::{
    patch,
    web::{self, Data, Json},
    Scope, get,
};

use experimentation_platform::{
    api::experiments::{
        handlers::{get_experiment, conclude}, 
        types::{ExperimentResponse, ConcludeExperimentRequest, VariantType},
        helpers::extract_dimensions
    }, 
    db::models::Experiment
};
use serde_json::Value;

use crate::api::external_api::{
    helpers::{fetch_variant_id, get_resolved_config},
    types::DiffResponse
};
use service_utils::{
    service::types::{AppState, AuthenticationInfo, DbConnection},
    types as app,
};

pub fn endpoints(scope: Scope) -> Scope {
    scope
        .service(stabilize)
        .service(revert)
        .service(diff_handler)
}

#[patch("/{id}/stabilize")]
async fn stabilize(
    params: web::Path<i64>,
    state: Data<AppState>,
    db_conn: DbConnection,
    auth_info: AuthenticationInfo,
) -> app::Result<Json<ExperimentResponse>> {
    let response = conclude_experiment(params.into_inner(), state, db_conn, auth_info, VariantType::EXPERIMENTAL).await?;
    return Ok(Json(ExperimentResponse::from(response)));
}

#[patch("/{id}/revert")]
async fn revert(
    params: web::Path<i64>,
    state: Data<AppState>,
    db_conn: DbConnection,
    auth_info: AuthenticationInfo,
) -> app::Result<Json<ExperimentResponse>> {    
    let response = conclude_experiment(params.into_inner(), state, db_conn, auth_info, VariantType::CONTROL).await?;
    return Ok(Json(ExperimentResponse::from(response)));
}

pub async fn conclude_experiment(
    exp_id: i64,
    state: Data<AppState>,
    db_conn: DbConnection,
    auth_info: AuthenticationInfo,
    variant: VariantType,
) -> app::Result<Experiment> {
    let DbConnection(mut conn) = db_conn;
    
    let experiment = get_experiment(exp_id, &mut conn)?;
    let id = fetch_variant_id(&experiment, variant)?;
    let req_body = ConcludeExperimentRequest {
        chosen_variant : id.to_string()
    };
    let response = conclude (state, exp_id, req_body, conn, auth_info).await?;
    return Ok(response);
}

#[get("/{id}/diff")]
pub async fn diff_handler(
    params: web::Path<i64>,
    state: Data<AppState>,
    db_conn: DbConnection,
) -> app::Result<Json<DiffResponse>> {
    let DbConnection(mut conn) = db_conn;
    let exp_id = params.into_inner();
    let experiment = get_experiment(exp_id, &mut conn)?;
    let mut req = extract_dimensions(&experiment.context)?;
    let control_id = fetch_variant_id(&experiment, VariantType::CONTROL)?;
    let experimental_id = fetch_variant_id(&experiment, VariantType::EXPERIMENTAL)?;

    req.insert("variantIds".to_string(), Value::String(format!("[{}]",control_id)));
    let before = get_resolved_config(&state, &req)?;
    req.insert("variantIds".to_string(), Value::String(format!("[{}]",experimental_id)));
    let after = get_resolved_config(&state, &req)?;

    let res = DiffResponse {
        before,
        after
    };
    return Ok(Json(res));
}
