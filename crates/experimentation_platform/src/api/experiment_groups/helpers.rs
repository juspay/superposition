use std::collections::HashSet;

use actix_web::web::{Data, Json};
use diesel::SelectableHelper;
use diesel::{BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::Value;
use service_utils::helpers::generate_snowflake_id;
use service_utils::service::types::{AppState, WorkspaceContext};
use superposition_macros::bad_argument;
use superposition_types::{
    api::experiment_groups::{ExpGroupCreateRequest, ExpGroupUpdateRequest},
    database::{
        models::experimentation::{ExperimentGroup, ExperimentStatusType},
        schema::{
            experiment_groups::dsl as experiment_groups, experiments::dsl as experiments,
        },
    },
    result as superposition, Condition, DBConnection, User,
};

use crate::api::experiments::{cac_api::validate_context, helpers::hash};

pub fn validate_members(
    members: &[String],
    group_context: &Condition,
    conn: &mut DBConnection,
    schema_name: &String,
) -> superposition::Result<Vec<i64>> {
    if members.is_empty() {
        return Ok(Vec::new());
    }
    let members = members
        .iter()
        .map(|member| {
            member.parse::<i64>().map_err(|e| {
                bad_argument!("{} is not a valid experiment ID. Error: {}", member, e)
            })
        })
        .collect::<superposition::Result<Vec<_>>>()?;
    let mut accepted_members = Vec::new();
    let experiment_contexts: Vec<(i64, Condition)> = experiments::experiments
        .select((experiments::id, experiments::context))
        .filter(
            experiments::id
                .eq_any(&members[..])
                .and(experiments::status.eq(ExperimentStatusType::CREATED)),
        )
        .schema_name(schema_name)
        .get_results::<(i64, Condition)>(conn)?;
    for (experiment_id, context) in experiment_contexts.iter() {
        if context
            .contains(group_context)
            .map_err(|e| bad_argument!("The contexts do not match. Error: {}", e))?
        {
            accepted_members.push(*experiment_id);
        } else {
            return Err(bad_argument!(
                "Experiment with id {} does not fit in with the experiment group. The contexts do not match.", experiment_id
            ));
        }
    }
    if members.len() != accepted_members.len() {
        return Err(bad_argument!(
            "Some experiment Ids do not exist/are invalid"
        ));
    }
    Ok(accepted_members)
}

pub async fn create_experiment_group(
    state: Data<AppState>,
    req: ExpGroupCreateRequest,
    conn: &mut DBConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let exp_context = req.context.clone();
    validate_context(&state, &exp_context, &workspace_request, &user).await?;
    validate_members(
        &req.member_experiment_ids[..],
        &exp_context,
        conn,
        &workspace_request.schema_name,
    )?;
    let experiment_group_id = generate_snowflake_id(&state)?;
    let experiment_group_hash = hash(&Value::Object(exp_context.clone().into()));
    let new_experiment_group = ExperimentGroup {
        experiment_group_id,
        experiment_group_hash,
        name: req.name,
        description: req.description,
        change_reason: req.change_reason,
        created_by: user.email.clone(),
        last_modified_by: user.email.clone(),
        created_at: chrono::Utc::now(),
        last_modified_at: chrono::Utc::now(),
        context: req.context,
        traffic_percentage: req.traffic_percentage,
        member_experiment_ids: req.member_experiment_ids,
    };
    let new_experiment_group = diesel::insert_into(experiment_groups::experiment_groups)
        .values(&new_experiment_group)
        .returning(ExperimentGroup::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_result::<ExperimentGroup>(conn)?;
    Ok(Json(new_experiment_group))
}

pub fn update_experiment_group(
    id: i64,
    req: ExpGroupUpdateRequest,
    conn: &mut DBConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let mut req = req;
    let exp_context = experiment_groups::experiment_groups
        .select(experiment_groups::context)
        .filter(experiment_groups::experiment_group_id.eq(&id))
        .schema_name(&workspace_request.schema_name)
        .first::<Condition>(conn)?;
    validate_members(
        &req.member_experiment_ids[..],
        &exp_context,
        conn,
        &workspace_request.schema_name,
    )?;
    let new_members: HashSet<String> = HashSet::from_iter(req.member_experiment_ids);
    req.member_experiment_ids = Vec::from_iter(new_members);
    let updated_group = diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::experiment_group_id.eq(&id))
        .set((
            req,
            experiment_groups::last_modified_by.eq(user.email),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_result(conn)?;
    Ok(Json(updated_group))
}
