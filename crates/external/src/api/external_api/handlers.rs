use actix_web::{
    get, patch,
    web::{self, Data, Json},
    Scope,
};

use crate::api::external_api::{
    helpers::{fetch_variant_id, get_resolved_config},
    types::DiffResponse,
};
use experimentation_platform::{
    api::experiments::{
        handlers::{conclude, get_experiment},
        types::{ConcludeExperimentRequest, ExperimentResponse, VariantType},
    },
    db::models::Experiment,
};
use serde_json::Value;
use service_utils::{
    helpers::extract_dimensions,
    result,
    service::types::{AppState, DbConnection, Tenant},
};

use superposition_types::User;

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
    tenant: Tenant,
    user: User,
) -> result::Result<Json<ExperimentResponse>> {
    let response = conclude_experiment(
        params.into_inner(),
        state,
        db_conn,
        tenant,
        VariantType::EXPERIMENTAL,
        user,
    )
    .await?;
    return Ok(Json(ExperimentResponse::from(response)));
}

#[patch("/{id}/revert")]
async fn revert(
    params: web::Path<i64>,
    state: Data<AppState>,
    db_conn: DbConnection,
    tenant: Tenant,
    user: User,
) -> result::Result<Json<ExperimentResponse>> {
    let response = conclude_experiment(
        params.into_inner(),
        state,
        db_conn,
        tenant,
        VariantType::CONTROL,
        user,
    )
    .await?;
    return Ok(Json(ExperimentResponse::from(response)));
}

pub async fn conclude_experiment(
    exp_id: i64,
    state: Data<AppState>,
    db_conn: DbConnection,
    tenant: Tenant,
    variant: VariantType,
    user: User,
) -> result::Result<Experiment> {
    let DbConnection(mut conn) = db_conn;

    let experiment = get_experiment(exp_id, &mut conn)?;
    let id = fetch_variant_id(&experiment, variant)?;
    let req_body = ConcludeExperimentRequest {
        chosen_variant: id.to_string(),
    };
    let response = conclude(state, exp_id, req_body, conn, tenant, user).await?;
    return Ok(response);
}

#[get("/{id}/diff")]
pub async fn diff_handler(
    params: web::Path<i64>,
    state: Data<AppState>,
    db_conn: DbConnection,
) -> result::Result<Json<DiffResponse>> {
    let DbConnection(mut conn) = db_conn;
    let exp_id = params.into_inner();
    let experiment = get_experiment(exp_id, &mut conn)?;
    let mut req = extract_dimensions(&experiment.context)?;
    let control_id = fetch_variant_id(&experiment, VariantType::CONTROL)?;
    let experimental_id = fetch_variant_id(&experiment, VariantType::EXPERIMENTAL)?;

    req.insert(
        "variantIds".to_string(),
        Value::String(format!("[{}]", control_id)),
    );
    let before = get_resolved_config(&state, &req).await?;
    req.insert(
        "variantIds".to_string(),
        Value::String(format!("[{}]", experimental_id)),
    );
    let after = get_resolved_config(&state, &req).await?;

    let res = DiffResponse { before, after };
    return Ok(Json(res));
}
