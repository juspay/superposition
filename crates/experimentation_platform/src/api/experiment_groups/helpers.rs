use std::collections::HashSet;

use actix_web::web::Json;
use diesel::{
    BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
};
use service_utils::service::types::SchemaName;
use superposition_macros::bad_argument;
use superposition_types::{
    api::experiment_groups::ExpGroupMemberRequest,
    database::{
        models::experimentation::{
            ExperimentGroup, ExperimentStatusType, TrafficPercentage,
        },
        schema::{
            experiment_groups::dsl as experiment_groups, experiments::dsl as experiments,
        },
    },
    result as superposition, Condition, DBConnection, User,
};

/// validates if the members in the members lit can be part of the experiment group
/// it checks the following
/// - if the members are in the database
/// - if they are in the created stage,
/// - if their contexts contain the group context
/// - if the sum of their traffic percentages does not exceed 100%
/// - if there are no repeating members in the new members list
pub fn validate_members(
    new_members: &[i64],
    existing_members: &[i64],
    group_context: &Condition,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<i64>> {
    if new_members.is_empty() {
        return Ok(Vec::new());
    }
    let new_members = HashSet::from_iter(new_members.to_owned());
    let existing_members = HashSet::from_iter(existing_members.to_owned());
    let repeating_members = new_members
        .intersection(&existing_members)
        .collect::<Vec<_>>();
    if !repeating_members.is_empty() {
        return Err(bad_argument!(
            "The new members list contains IDs that are already in the group: {}",
            repeating_members
                .iter()
                .map(|id| id.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ));
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
                    .eq_any(&new_members)
                    .and(experiments::status.eq(ExperimentStatusType::CREATED)),
            )
            .schema_name(schema_name)
            .get_results::<(i64, Condition, TrafficPercentage)>(conn)?;
    let existing_member_contexts: Vec<TrafficPercentage> = experiments::experiments
        .select(experiments::traffic_percentage)
        .filter(experiments::id.eq_any(&existing_members))
        .schema_name(schema_name)
        .get_results::<TrafficPercentage>(conn)?;
    let mut traffic_percentage_sum = existing_member_contexts
        .into_iter()
        .fold(0, |acc, x| acc + *x);
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

    if traffic_percentage_sum >= 100 {
        return Err(bad_argument!(
            "The total traffic percentage of the experiments in the group exceeds 100%. Current total: {}",
            traffic_percentage_sum
        ));
    }
    let invalid_members: HashSet<_> = new_members.difference(&accepted_members).collect();
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
    let all_members = accepted_members
        .union(&existing_members)
        .cloned()
        .collect::<Vec<_>>();
    Ok(all_members)
}

pub fn add_members(
    exp_group_id: &i64,
    mut req: ExpGroupMemberRequest,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let exp_context = experiment_groups::experiment_groups
        .filter(experiment_groups::id.eq(exp_group_id))
        .schema_name(schema_name)
        .first::<ExperimentGroup>(conn)?;
    req.member_experiment_ids = validate_members(
        &req.member_experiment_ids,
        &exp_context.member_experiment_ids,
        &exp_context.context,
        conn,
        schema_name,
    )?;
    let updated_group = diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(exp_group_id))
        .set((
            req,
            experiment_groups::last_modified_by.eq(user.email),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(schema_name)
        .get_result(conn)?;
    Ok(Json(updated_group))
}

pub fn remove_members(
    id: &i64,
    mut req: ExpGroupMemberRequest,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let exp_context = experiment_groups::experiment_groups
        .filter(experiment_groups::id.eq(&id))
        .schema_name(schema_name)
        .first::<ExperimentGroup>(conn)?;
    let current_members: HashSet<i64> =
        HashSet::from_iter(exp_context.member_experiment_ids);
    let members_to_remove = HashSet::from_iter(req.member_experiment_ids);
    req.member_experiment_ids = current_members
        .difference(&members_to_remove)
        .cloned()
        .collect::<Vec<_>>();
    let updated_group = diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(&id))
        .set((
            req,
            experiment_groups::last_modified_by.eq(user.email),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(schema_name)
        .get_result(conn)?;
    Ok(Json(updated_group))
}
