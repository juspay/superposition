use std::collections::HashSet;

use actix_web::web::{Data, Json};
use diesel::{
    BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
};
use serde_json::Value;
use service_utils::{
    helpers::generate_snowflake_id,
    service::types::{AppState, SchemaName, WorkspaceContext},
};
use superposition_macros::{bad_argument, unexpected_error};
use superposition_types::{
    api::experiment_groups::ExpGroupMemberRequest,
    database::{
        models::{
            experimentation::{
                Buckets, Experiment, ExperimentGroup, ExperimentStatusType, GroupType,
                TrafficPercentage,
            },
            ChangeReason, Description,
        },
        schema::{
            experiment_groups::dsl as experiment_groups, experiments::dsl as experiments,
        },
    },
    result as superposition, Condition, DBConnection, User,
};

use crate::api::experiments::helpers::{ensure_experiments_exist, hash};

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
) -> superposition::Result<Vec<i64>> {
    let existing_members = HashSet::from_iter(existing_members.to_owned());

    for member_experiment in member_experiments.iter() {
        if !member_experiment
            .context
            .contains(group_context)
            .map_err(|e| bad_argument!("The contexts do not match. Error: {}", e))?
        {
            return Err(bad_argument!(
                "Experiment with id {} does not fit in with the experiment group. The contexts do not match.",
                member_experiment.id
            ));
        }
    }

    let all_members = member_experiments
        .iter()
        .map(|exp| exp.id)
        .collect::<HashSet<i64>>()
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
    if req.member_experiment_ids.is_empty() {
        return Err(bad_argument!(
            "Please provide at least one experiment ID to add to the group"
        ));
    }
    let exp_context: ExperimentGroup = experiment_groups::experiment_groups
        .filter(experiment_groups::id.eq(exp_group_id))
        .schema_name(schema_name)
        .first::<ExperimentGroup>(conn)?;

    if exp_context.group_type == GroupType::SystemGenerated {
        return Err(bad_argument!(
            "Cannot add members to a system-generated experiment groups."
        ));
    }

    req.member_experiment_ids = validate_experiment_group_constraints(
        member_experiments,
        &exp_context.member_experiment_ids,
        &exp_context.context,
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
    if req.member_experiment_ids.is_empty() {
        return Err(bad_argument!(
            "Please provide at least one experiment ID to remove from the group"
        ));
    }
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

    for member_id in &members_to_remove {
        let experiment = experiments::experiments
            .filter(experiments::id.eq(member_id))
            .schema_name(schema_name)
            .first::<Experiment>(conn)?;
        update_bucket_allocation(
            &experiment,
            id,
            &TrafficPercentage::try_from(0).map_err(|e| unexpected_error!(e))?,
            conn,
            schema_name,
            user,
        )?;
    }
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

pub fn update_bucket_allocation(
    experiment: &Experiment,
    experiment_group_id: &i64,
    traffic_percentage: &TrafficPercentage,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    user: &User,
) -> superposition::Result<()> {
    let experiment_group: ExperimentGroup = experiment_groups::experiment_groups
        .filter(experiment_groups::id.eq(&experiment_group_id))
        .schema_name(schema_name)
        .for_update()
        .get_result::<ExperimentGroup>(conn)?;

    let mut buckets = experiment_group.buckets.clone();
    let mut current_buckets = vec![];
    let mut unassigned_buckets = vec![];

    // Separate current and unassigned buckets
    for bucket in buckets.iter_mut() {
        if let Some(variant_id) = bucket {
            if experiment.variants.iter().any(|v| v.id == *variant_id) {
                current_buckets.push(bucket);
            }
        } else {
            unassigned_buckets.push(bucket);
        }
    }

    let required_bucket_count = **traffic_percentage as usize * experiment.variants.len();
    let current_bucket_count = current_buckets.len();

    match required_bucket_count.cmp(&current_bucket_count) {
        std::cmp::Ordering::Greater => {
            assign_additional_buckets(
                &mut unassigned_buckets.into_iter().rev().collect::<Vec<_>>(),
                experiment,
                required_bucket_count - current_bucket_count,
            )?;
        }
        std::cmp::Ordering::Less => {
            unassign_excess_buckets(
                &mut current_buckets,
                experiment,
                current_bucket_count - required_bucket_count,
            );
        }
        std::cmp::Ordering::Equal => {
            return Ok(());
        }
    }

    let new_traffic_percentage =
        if experiment_group.group_type == GroupType::SystemGenerated {
            TrafficPercentage::try_from(
                experiment.variants.clone().into_inner().len() as i32
                    * traffic_percentage.as_i32(),
            )
            .map_err(|e| unexpected_error!(e))?
        } else {
            experiment_group.traffic_percentage.clone()
        };

    // Update the experiment group with the modified buckets
    diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(&experiment_group_id))
        .set((
            experiment_groups::buckets.eq(&buckets),
            experiment_groups::traffic_percentage.eq(new_traffic_percentage),
            experiment_groups::last_modified_by.eq(&user.email),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(schema_name)
        .execute(conn)?;

    Ok(())
}

fn assign_additional_buckets(
    unassigned_buckets: &mut Vec<&mut Option<String>>,
    experiment: &Experiment,
    additional_needed: usize,
) -> superposition::Result<()> {
    if additional_needed > unassigned_buckets.len() {
        return Err(bad_argument!(
            "Not enough empty buckets to accommodate the updated traffic percentage. Required additional: {}, Available: {}",
            additional_needed,
            unassigned_buckets.len()
        ));
    }
    let variants_len = experiment.variants.clone().into_inner().len();

    for variant in experiment.variants.clone().into_inner() {
        for _ in 0..additional_needed / variants_len {
            if let Some(bucket) = unassigned_buckets.pop() {
                *bucket = Some(variant.id.clone());
            }
        }
    }

    Ok(())
}

fn unassign_excess_buckets(
    current_buckets: &mut [&mut Option<String>],
    experiment: &Experiment,
    excess_count: usize,
) {
    let variants_len = experiment.variants.clone().into_inner().len();
    for variant in experiment.variants.clone().into_inner() {
        for _ in 0..excess_count / variants_len {
            if let Some(bucket) = current_buckets
                .iter_mut()
                .rev()
                .find(|b| b.is_some() && **b == &mut Some(variant.id.clone()))
            {
                **bucket = None;
            }
        }
    }
}

pub fn detach_experiment_from_group(
    experiment: &Experiment,
    experiment_group_id: i64,
    conn: &mut DBConnection,
    workspace_request: &WorkspaceContext,
    user: &User,
) -> superposition::Result<()> {
    let experiment_group = experiment_groups::experiment_groups
        .filter(experiment_groups::id.eq(&experiment_group_id))
        .schema_name(&workspace_request.schema_name)
        .first::<ExperimentGroup>(conn)?;

    if experiment_group.group_type == GroupType::SystemGenerated {
        // delete the system-generated experiment group
        diesel::delete(experiment_groups::experiment_groups)
            .filter(experiment_groups::id.eq(&experiment_group_id))
            .schema_name(&workspace_request.schema_name)
            .execute(conn)?;
        return Ok(());
    }

    let mut buckets = experiment_group.buckets.clone();

    // Remove experiments from buckets if they are in the removed_experiments list
    for bucket in buckets.iter_mut() {
        if let Some(variant_id) = bucket {
            // Extract experiment_id from variant_id
            let experiment_id = variant_id.split('-')
            .next()
            .and_then(|id| id.parse::<i64>().ok())
            .ok_or_else(|| {
                bad_argument!(
                    "Invalid variant ID format. Ensure the variant ID is in the correct format."
                )
            })?;
            if experiment.id == experiment_id {
                *bucket = None;
            }
        }
    }

    // Remove the experiment from the member_experiment_ids
    let mut member_experiment_ids = experiment_group.member_experiment_ids.clone();
    member_experiment_ids.retain(|&id| id != experiment.id);

    diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(&experiment_group_id))
        .set((
            experiment_groups::member_experiment_ids.eq(member_experiment_ids),
            experiment_groups::buckets.eq(buckets),
            experiment_groups::last_modified_by.eq(user.email.clone()),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(&workspace_request.schema_name)
        .execute(conn)?;
    Ok(())
}

pub fn create_system_generated_experiment_group(
    experiment: &Experiment,
    traffic_percentage: &TrafficPercentage,
    state: &Data<AppState>,
    conn: &mut DBConnection,
    workspace_request: &WorkspaceContext,
    user: &User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let exp_context = experiment.context.clone();
    let id = generate_snowflake_id(state)?;
    let context_hash = hash(&Value::Object(exp_context.clone().into()));
    let now = chrono::Utc::now();
    let description = Description::try_from(format!(
        "Experiment group for experiment {}",
        experiment.name
    ))
    .map_err(|e| unexpected_error!(e))?;

    let group_traffic_percentage = TrafficPercentage::try_from(
        experiment.variants.clone().into_inner().len() as i32
            * traffic_percentage.as_i32(),
    )
    .map_err(|e| unexpected_error!(e))?;

    let mut buckets = Buckets::new();

    for variant in experiment.variants.iter() {
        for _i in 0..**traffic_percentage {
            for bucket in buckets.iter_mut() {
                if bucket.is_none() {
                    *bucket = Some(variant.id.clone());
                    break;
                }
            }
        }
    }

    let new_experiment_group = ExperimentGroup {
        id,
        context_hash,
        name: experiment.name.clone(),
        description,
        change_reason: ChangeReason::default(),
        created_by: user.email.clone(),
        last_modified_by: user.email.clone(),
        created_at: now,
        last_modified_at: now,
        context: exp_context,
        traffic_percentage: group_traffic_percentage,
        member_experiment_ids: vec![experiment.id],
        buckets,
        group_type: GroupType::SystemGenerated,
    };
    let new_experiment_group = diesel::insert_into(experiment_groups::experiment_groups)
        .values(&new_experiment_group)
        .returning(ExperimentGroup::as_returning())
        .schema_name(&workspace_request.schema_name)
        .get_result::<ExperimentGroup>(conn)?;
    Ok(Json(new_experiment_group))
}
