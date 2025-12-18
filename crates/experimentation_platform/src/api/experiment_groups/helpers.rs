use std::collections::HashSet;

use actix_web::web::{Data, Json};
use diesel::{
    BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
};
use fred::{prelude::{KeysInterface, RedisPool}, types::Expiration};
use serde_json::Value;
use service_utils::{
    helpers::{generate_snowflake_id, get_from_env_or_default},
    redis::EXPERIMENT_GROUPS_LIST_KEY_SUFFIX,
    service::types::{AppState, SchemaName, WorkspaceContext},
};
use superposition_macros::{bad_argument, unexpected_error};
use superposition_types::{
    Condition, DBConnection, User,
    api::experiment_groups::ExpGroupMemberRequest,
    database::{
        models::{
            ChangeReason, Description,
            experimentation::{
                Bucket, Buckets, Experiment, ExperimentGroup, ExperimentStatusType,
                GroupType, TrafficPercentage,
            },
        },
        schema::{
            experiment_groups::dsl as experiment_groups, experiments::dsl as experiments,
        },
    },
    result as superposition, Condition, DBConnection, PaginatedResponse, User,
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
    let experiment_group = fetch_experiment_group(exp_group_id, conn, schema_name)?;

    if experiment_group.group_type == GroupType::SystemGenerated {
        return Err(bad_argument!(
            "Cannot add members to a system-generated experiment groups."
        ));
    }

    req.member_experiment_ids = validate_experiment_group_constraints(
        member_experiments,
        &experiment_group.member_experiment_ids,
        &experiment_group.context,
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

    let experiment_group = fetch_experiment_group(id, conn, schema_name)?;

    if experiment_group.group_type == GroupType::SystemGenerated {
        return Err(bad_argument!(
            "Cannot remove members from a system-generated experiment group."
        ));
    }

    let current_members: HashSet<i64> =
        HashSet::from_iter(experiment_group.member_experiment_ids.clone());
    let members_to_remove = HashSet::from_iter(req.member_experiment_ids);
    req.member_experiment_ids = current_members
        .difference(&members_to_remove)
        .cloned()
        .collect::<Vec<_>>();

    let experiments_to_remove: Vec<Experiment> = experiments::experiments
        .filter(experiments::id.eq_any(members_to_remove.clone()))
        .schema_name(schema_name)
        .for_update()
        .get_results::<Experiment>(conn)?;

    ensure_experiments_exist(
        &HashSet::from_iter(members_to_remove),
        &experiments_to_remove,
        "The following experiment IDs are not present in the database",
    )?;

    let mut buckets = experiment_group.buckets;
    for member_experiment in &experiments_to_remove {
        update_bucket_allocation(
            member_experiment,
            &mut buckets,
            &TrafficPercentage::default(),
        )?;
    }

    let updated_group = diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(&id))
        .set((
            req,
            experiment_groups::buckets.eq(buckets),
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
    exp_group_buckets: &mut Buckets,
    exp_traffic_percentage: &TrafficPercentage,
) -> superposition::Result<()> {
    let mut current_exp_buckets = vec![];
    let mut unassigned_buckets = vec![];

    // Separate current exp buckets and unassigned buckets
    for bucket in exp_group_buckets.iter_mut() {
        if let Some(buck) = bucket {
            if experiment.variants.iter().any(|v| v.id == *buck.variant_id) {
                current_exp_buckets.push(bucket);
            }
        } else {
            unassigned_buckets.push(bucket);
        }
    }

    let required_bucket_count =
        **exp_traffic_percentage as usize * experiment.variants.len();
    let current_bucket_count = current_exp_buckets.len();
    let bucket_diff = required_bucket_count.abs_diff(current_bucket_count);

    match required_bucket_count.cmp(&current_bucket_count) {
        std::cmp::Ordering::Greater => {
            assign_additional_buckets(&mut unassigned_buckets, experiment, bucket_diff)?;
        }
        std::cmp::Ordering::Less => {
            unassign_excess_buckets(&mut current_exp_buckets, experiment, bucket_diff);
        }
        std::cmp::Ordering::Equal => (),
    }

    Ok(())
}

fn assign_additional_buckets(
    unassigned_buckets: &mut Vec<&mut Option<Bucket>>,
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
    let variants = experiment.variants.clone().into_inner();
    let variants_len = variants.len();

    // Reverse the unassigned_buckets to fill from the front
    unassigned_buckets.reverse();

    for variant in variants {
        for _ in 0..additional_needed / variants_len {
            if let Some(bucket) = unassigned_buckets.pop() {
                *bucket = Some(Bucket {
                    experiment_id: experiment.id.to_string(),
                    variant_id: variant.id.clone(),
                });
            }
        }
    }

    Ok(())
}

fn unassign_excess_buckets(
    current_buckets: &mut [&mut Option<Bucket>],
    experiment: &Experiment,
    excess_count: usize,
) {
    let variants = experiment.variants.clone().into_inner();
    let variants_len = variants.len();
    for variant in variants {
        for _ in 0..excess_count / variants_len {
            if let Some(bucket) = current_buckets
                .iter_mut()
                .rev()
                .find(|b| b.as_ref().is_some_and(|b| *b.variant_id == *variant.id))
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
    workspace_context: &WorkspaceContext,
    user: &User,
) -> superposition::Result<()> {
    let experiment_group = fetch_experiment_group(
        &experiment_group_id,
        conn,
        &workspace_context.schema_name,
    )?;

    let mut buckets = experiment_group.buckets;
    update_bucket_allocation(experiment, &mut buckets, &TrafficPercentage::default())?;

    let mut member_experiment_ids = experiment_group.member_experiment_ids;
    member_experiment_ids.retain(|&id| id != experiment.id);

    diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(&experiment_group_id))
        .set((
            experiment_groups::change_reason.eq(ChangeReason::try_from(format!(
                "Removed experiment {} from group {}",
                experiment.id, experiment_group_id
            ))
            .map_err(|e| unexpected_error!(e))?),
            experiment_groups::member_experiment_ids.eq(member_experiment_ids),
            experiment_groups::buckets.eq(buckets),
            experiment_groups::last_modified_by.eq(user.email.clone()),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(&workspace_context.schema_name)
        .execute(conn)?;

    if experiment_group.group_type == GroupType::SystemGenerated {
        diesel::delete(experiment_groups::experiment_groups)
            .filter(experiment_groups::id.eq(&experiment_group_id))
            .schema_name(&workspace_context.schema_name)
            .execute(conn)?;
    }

    Ok(())
}

pub fn create_system_generated_experiment_group(
    experiment: &Experiment,
    exp_traffic_percentage: &TrafficPercentage,
    state: &Data<AppState>,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    user: &User,
) -> superposition::Result<ExperimentGroup> {
    let context = experiment.context.clone();
    let id = generate_snowflake_id(state)?;
    let context_hash = hash(&Value::Object(context.clone().into()));
    let now = chrono::Utc::now();

    let variants = experiment.variants.clone().into_inner();
    let group_traffic_percentage =
        TrafficPercentage::try_from(variants.len() as u8 * **exp_traffic_percentage)
            .map_err(|e| unexpected_error!(e))?;

    let mut buckets = Buckets::default();
    update_bucket_allocation(experiment, &mut buckets, exp_traffic_percentage)?;

    let new_experiment_group = ExperimentGroup {
        id,
        context_hash,
        name: experiment.name.clone(),
        description: Description::try_from(format!(
            "Experiment group for experiment {}",
            experiment.name
        ))
        .map_err(|e| unexpected_error!(e))?,
        change_reason: ChangeReason::try_from(format!(
            "System generated experiment group for experiment {}",
            experiment.id
        ))
        .map_err(|e| unexpected_error!(e))?,
        created_by: user.get_email(),
        last_modified_by: user.get_email(),
        created_at: now,
        last_modified_at: now,
        context,
        traffic_percentage: group_traffic_percentage,
        member_experiment_ids: vec![experiment.id],
        buckets,
        group_type: GroupType::SystemGenerated,
    };
    let new_experiment_group = diesel::insert_into(experiment_groups::experiment_groups)
        .values(&new_experiment_group)
        .returning(ExperimentGroup::as_returning())
        .schema_name(schema_name)
        .get_result::<ExperimentGroup>(conn)?;
    Ok(new_experiment_group)
}

pub fn update_experiment_group_buckets(
    experiment: &Experiment,
    experiment_group_id: &i64,
    exp_traffic_percentage: &TrafficPercentage,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    user: &User,
) -> superposition::Result<()> {
    let experiment_group =
        fetch_experiment_group(experiment_group_id, conn, schema_name)?;

    let new_traffic_percentage = match experiment_group.group_type {
        GroupType::SystemGenerated => TrafficPercentage::try_from(
            experiment.variants.clone().into_inner().len() as i32
                * (**exp_traffic_percentage as i32),
        )
        .map_err(|e| unexpected_error!(e))?,
        GroupType::UserCreated => experiment_group.traffic_percentage,
    };

    let mut buckets = experiment_group.buckets;
    update_bucket_allocation(experiment, &mut buckets, exp_traffic_percentage)?;

    diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(experiment_group.id))
        .set((
            experiment_groups::buckets.eq(buckets),
            experiment_groups::traffic_percentage.eq(new_traffic_percentage),
            experiment_groups::change_reason.eq(ChangeReason::try_from(format!(
                "Updated traffic percentage for experiment group {}",
                experiment_group.id
            ))
            .map_err(|e| unexpected_error!(e))?),
            experiment_groups::last_modified_by.eq(user.get_email()),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(schema_name)
        .execute(conn)?;
    Ok(())
}

pub fn fetch_experiment_group(
    id: &i64,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<ExperimentGroup> {
    let experiment_group = experiment_groups::experiment_groups
        .filter(experiment_groups::id.eq(id))
        .schema_name(schema_name)
        .for_update()
        .get_result::<ExperimentGroup>(conn)?;
    Ok(experiment_group)
}

pub async fn put_experiment_groups_in_redis(
    redis_pool: Option<RedisPool>,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let pool = match redis_pool {
        Some(pool) => pool,
        None => {
            log::debug!("Redis not configured, skipping experiment groups cache update");
            return Ok(());
        }
    };

    let experiment_group_list: Vec<ExperimentGroup> =
        experiment_groups::experiment_groups
            .order(experiment_groups::last_modified_at.desc())
            .schema_name(schema_name)
            .load::<ExperimentGroup>(conn)?;

    let paginated_response = PaginatedResponse::all(experiment_group_list);

    let serialized = serde_json::to_string(&paginated_response).map_err(|e| {
        log::error!("Failed to serialize experiment groups for redis: {}", e);
        unexpected_error!("Failed to serialize experiment groups for redis: {}", e)
    })?;

    let key = format!("{}{EXPERIMENT_GROUPS_LIST_KEY_SUFFIX}", **schema_name);
    let key_ttl: i64 = get_from_env_or_default("REDIS_KEY_TTL", 604800);
    let expiration = Some(Expiration::EX(key_ttl));
    pool.next_connected()
        .set::<(), String, String>(key, serialized, expiration, None, false)
        .await
        .map_err(|e| {
            log::error!("Failed to write experiment groups to redis: {}", e);
            unexpected_error!("Failed to write experiment groups to redis: {}", e)
        })?;

    log::debug!("Successfully updated experiment groups cache in Redis");
    Ok(())
}
