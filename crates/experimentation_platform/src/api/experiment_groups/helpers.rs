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
            Experiment, ExperimentGroup, ExperimentStatusType, TrafficPercentage,
        },
        schema::{
            experiment_groups::dsl as experiment_groups, experiments::dsl as experiments,
        },
    },
    result as superposition, Condition, DBConnection, User,
};

use crate::api::experiments::helpers::ensure_experiments_exist;

pub fn fetch_and_validate_members(
    new_members: &[i64],
    existing_members: &[i64],
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<Experiment>> {
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

    let members: Vec<Experiment> = experiments::experiments
        .filter(
            experiments::id
                .eq_any(&new_members)
                .and(experiments::status.eq(ExperimentStatusType::CREATED)),
        )
        .schema_name(schema_name)
        .get_results::<Experiment>(conn)?;

    ensure_experiments_exist(
        &new_members,
        &members,
        "The following experiment IDs are not present in the database/are not in the created stage",
    )?;
    Ok(members)
}

/// validates if the members in the members lit can be part of the experiment group
/// it checks the following
/// - if their contexts contain the group context
/// - if the sum of their traffic percentages does not exceed 100%
pub fn validate_experiment_group_constraints(
    member_experiments: &[Experiment],
    existing_members: &[i64],
    group_context: &Condition,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<i64>> {
    let mut accepted_members = HashSet::new();
    let existing_members = HashSet::from_iter(existing_members.to_owned());

    let existing_member_contexts: Vec<TrafficPercentage> = experiments::experiments
        .select(experiments::traffic_percentage)
        .filter(experiments::id.eq_any(&existing_members))
        .schema_name(schema_name)
        .get_results::<TrafficPercentage>(conn)?;
    let mut traffic_percentage_sum = existing_member_contexts
        .into_iter()
        .fold(0, |acc, x| acc + *x);
    for member_experiment in member_experiments.iter() {
        if member_experiment
            .context
            .contains(group_context)
            .map_err(|e| bad_argument!("The contexts do not match. Error: {}", e))?
        {
            accepted_members.insert(member_experiment.id);
            traffic_percentage_sum += *member_experiment.traffic_percentage;
        } else {
            return Err(bad_argument!(
                "Experiment with id {} does not fit in with the experiment group. The contexts do not match.", member_experiment.id
            ));
        }
    }

    if traffic_percentage_sum >= 100 {
        return Err(bad_argument!(
            "The total traffic percentage of the experiments in the group exceeds 100%. Current total: {}",
            traffic_percentage_sum
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
    member_experiments: &[Experiment],
    mut req: ExpGroupMemberRequest,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    user: &User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let exp_context = experiment_groups::experiment_groups
        .filter(experiment_groups::id.eq(exp_group_id))
        .schema_name(schema_name)
        .first::<ExperimentGroup>(conn)?;
    req.member_experiment_ids = validate_experiment_group_constraints(
        member_experiments,
        &exp_context.member_experiment_ids,
        &exp_context.context,
        conn,
        schema_name,
    )?;
    let updated_group = diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(exp_group_id))
        .set((
            req,
            experiment_groups::last_modified_by.eq(user.email.clone()),
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
    user: &User,
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
            experiment_groups::last_modified_by.eq(user.email.clone()),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(schema_name)
        .get_result(conn)?;
    Ok(Json(updated_group))
}
