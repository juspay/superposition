use actix_web::{
    patch,
    web::{self, Data, Json},
    Scope,
};

use experimentation_platform::{
    api::experiments::{
        handlers::{get_experiment, conclude}, 
        types::{ExperimentResponse, ConcludeExperimentRequest, VariantType}
    }, 
    db::models::Experiment
};
use crate::api::external_api::helpers::fetch_variant_id;

use service_utils::{
    service::types::{AppState, AuthenticationInfo, DbConnection},
    types as app,
};

pub fn endpoints(scope: Scope) -> Scope {
    scope
        .service(stabilize)
        .service(revert)
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
    let id = fetch_variant_id(experiment, variant)?;
    let req_body = ConcludeExperimentRequest {
        chosen_variant : id.to_string()
    };
    let response = conclude (state, exp_id, req_body, conn, auth_info).await?;
    return Ok(response);
}
