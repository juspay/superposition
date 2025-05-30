use std::collections::HashSet;

use actix_web::web::{Data, Json};
use diesel::{
    BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
};
use serde_json::Value;
use service_utils::helpers::generate_snowflake_id;
use service_utils::service::types::{AppState, WorkspaceContext};
use superposition_macros::bad_argument;
use superposition_types::database::models::experimentation::TrafficPercentage;
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

/// validates if the members in the members lit can be part of the experiment group
/// it checks the following
/// - if the members are in the database
/// - if they are in the created stage,
/// - if their contexts contain the group context
/// - if the sum of their traffic percentages does not exceed 100%
pub fn validate_members(
    members: &[i64],
    group_context: &Condition,
    conn: &mut DBConnection,
    schema_name: &String,
) -> superposition::Result<Vec<i64>> {
    if members.is_empty() {
        return Ok(Vec::new());
    }
    let mut accepted_members = HashSet::new();
    let experiment_contexts: Vec<(i64, Condition, TrafficPercentage)> =
        experiments::experiments
            .select((
                experiments::id,
                experiments::context,
                experiments::traffic_percentage,
            ))
            .filter(
                experiments::id
                    .eq_any(members)
                    .and(experiments::status.eq(ExperimentStatusType::CREATED)),
            )
            .schema_name(schema_name)
            .get_results::<(i64, Condition, TrafficPercentage)>(conn)?;
    let mut traffic_percentage_sum = 0;
    for (experiment_id, context, traffic) in experiment_contexts.iter() {
        if context
            .contains(group_context)
            .map_err(|e| bad_argument!("The contexts do not match. Error: {}", e))?
        {
            accepted_members.insert(*experiment_id);
            traffic_percentage_sum += **traffic;
        } else {
            return Err(bad_argument!(
                "Experiment with id {} does not fit in with the experiment group. The contexts do not match.", experiment_id
            ));
        }
    }
    if traffic_percentage_sum > 100 {
        return Err(bad_argument!(
            "The total traffic percentage of the experiments in the group exceeds 100%. Current total: {}",
            traffic_percentage_sum
        ));
    }
    let members = HashSet::from_iter(members.to_owned());
    let invalid_members: HashSet<_> = members.difference(&accepted_members).collect();
    if !invalid_members.is_empty() {
        return Err(bad_argument!(
            "the following experiment IDs are not present in the database/are not in the created stage: {}",
            invalid_members
                .iter()
                .map(|id| id.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    Ok(Vec::from_iter(accepted_members))
}

pub async fn create_experiment_group(
    state: Data<AppState>,
    req: ExpGroupCreateRequest,
    conn: &mut DBConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let exp_context = req.context.clone().into_inner();
    validate_context(&state, &exp_context, &workspace_request, &user).await?;
    let members = if let Some(members) = req.member_experiment_ids {
        validate_members(&members, &exp_context, conn, &workspace_request.schema_name)?
    } else {
        Vec::new()
    };
    let id = generate_snowflake_id(&state)?;
    let context_hash = hash(&Value::Object(exp_context.clone().into()));
    let now = chrono::Utc::now();
    let new_experiment_group = ExperimentGroup {
        id,
        context_hash,
        name: req.name,
        description: req.description,
        change_reason: req.change_reason,
        created_by: user.email.clone(),
        last_modified_by: user.email.clone(),
        created_at: now,
        last_modified_at: now,
        context: exp_context,
        traffic_percentage: req.traffic_percentage,
        member_experiment_ids: members,
    };
    let new_experiment_group = diesel::insert_into(experiment_groups::experiment_groups)
        .values(&new_experiment_group)
        .returning(ExperimentGroup::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_result::<ExperimentGroup>(conn)?;
    Ok(Json(new_experiment_group))
}

pub async fn update_experiment_group(
    id: i64,
    req: ExpGroupUpdateRequest,
    conn: &mut DBConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let mut req = req;
    let exp_context = experiment_groups::experiment_groups
        .filter(experiment_groups::id.eq(&id))
        .schema_name(&workspace_request.schema_name)
        .first::<ExperimentGroup>(conn)?;
    if let Some(members) = req.member_experiment_ids {
        validate_members(
            &members,
            &exp_context.context,
            conn,
            &workspace_request.schema_name,
        )?;
        req.member_experiment_ids = Some(members);
    }
    let updated_group = diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(&id))
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
