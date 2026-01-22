use actix_web::{
    Scope, delete, get, patch, post,
    web::{self, Data, Json},
};
use chrono::Utc;
use diesel::{
    Connection, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
    TextExpressionMethods,
};
use serde_json::Value;
use service_utils::{
    helpers::{generate_snowflake_id, get_from_env_or_default},
    service::types::{AppState, DbConnection, WorkspaceContext},
};
use superposition_derives::authorized;
use superposition_macros::{bad_argument, unexpected_error};
use superposition_types::{
    PaginatedResponse, SortBy, User,
    api::experiment_groups::{
        ExpGroupCreateRequest, ExpGroupFilters, ExpGroupMemberRequest,
        ExpGroupUpdateRequest, SortOn,
    },
    custom_query::{self as superposition_query, CustomQuery, PaginationParams},
    database::{
        models::{
            ChangeReason,
            experimentation::{
                Buckets, Experiment, ExperimentGroup, ExperimentGroups,
                ExperimentStatusType, GroupType,
            },
        },
        schema::{
            experiment_groups::dsl as experiment_groups, experiments::dsl as experiments,
        },
    },
    result as superposition,
};

use crate::api::{
    experiment_groups::helpers::{
        add_members, create_system_generated_experiment_group,
        fetch_and_validate_members, fetch_experiment_group, remove_members,
        validate_experiment_group_constraints,
    },
    experiments::{
        cac_api::validate_context,
        helpers::{
            fetch_and_validate_change_reason_with_function, hash,
            validate_and_add_experiment_group_id,
            validate_and_remove_experiment_group_id,
        },
    },
};

pub fn endpoints(scope: Scope) -> Scope {
    scope
        .service(create_handler)
        .service(update_handler)
        .service(list_handler)
        .service(get_handler)
        .service(delete_handler)
        .service(add_members_handler)
        .service(remove_members_handler)
        .service(backfill_handler)
}

#[authorized]
#[post("")]
async fn create_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    req: Json<ExpGroupCreateRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let DbConnection(mut conn) = db_conn;
    let req = req.into_inner();
    log::trace!("Creating experiment group with request: {:?}", req);

    let exp_context = req.context.into_inner();
    let member_experiments = if let Some(members) = req.member_experiment_ids {
        fetch_and_validate_members(
            &members,
            &[],
            &mut conn,
            &workspace_context.schema_name,
        )?
    } else {
        Vec::new()
    };

    fetch_and_validate_change_reason_with_function(
        &workspace_context,
        &req.change_reason,
        &state,
    )
    .await?;

    validate_context(&state, &exp_context, &workspace_context, &user).await?;
    validate_experiment_group_constraints(&member_experiments, &[], &exp_context)?;

    let members = member_experiments
        .iter()
        .map(|exp| exp.id)
        .collect::<Vec<_>>();
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
        member_experiment_ids: members.clone(),
        buckets: Buckets::default(),
        group_type: GroupType::UserCreated,
    };

    let new_experiment_group =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            validate_and_add_experiment_group_id(
                &member_experiments,
                &id,
                &workspace_context.schema_name,
                transaction_conn,
                &user,
            )?;
            let new_experiment_group =
                diesel::insert_into(experiment_groups::experiment_groups)
                    .values(&new_experiment_group)
                    .returning(ExperimentGroup::as_returning())
                    .schema_name(&workspace_context.schema_name)
                    .get_result::<ExperimentGroup>(transaction_conn)?;
            Ok(new_experiment_group)
        })?;
    Ok(Json(new_experiment_group))
}

#[authorized]
#[patch("/{exp_group_id}")]
async fn update_handler(
    workspace_context: WorkspaceContext,
    exp_group_id: web::Path<i64>,
    req: Json<ExpGroupUpdateRequest>,
    db_conn: DbConnection,
    user: User,
    state: Data<AppState>,
) -> superposition::Result<Json<ExperimentGroup>> {
    let DbConnection(mut conn) = db_conn;
    let id = exp_group_id.into_inner();

    let experiment_group =
        fetch_experiment_group(&id, &mut conn, &workspace_context.schema_name)?;
    if experiment_group.group_type == GroupType::SystemGenerated {
        return Err(bad_argument!(
            "Cannot update system generated experiment group with id {}",
            id
        ));
    }

    let req = req.into_inner();

    fetch_and_validate_change_reason_with_function(
        &workspace_context,
        &req.change_reason,
        &state,
    )
    .await?;

    let updated_group = diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(&id))
        .set((
            req,
            experiment_groups::last_modified_by.eq(user.email),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(&workspace_context.schema_name)
        .get_result(&mut conn)?;
    Ok(Json(updated_group))
}

#[authorized]
#[patch("/{exp_group_id}/add-members")]
async fn add_members_handler(
    workspace_context: WorkspaceContext,
    exp_group_id: web::Path<i64>,
    req: Json<ExpGroupMemberRequest>,
    db_conn: DbConnection,
    user: User,
    state: Data<AppState>,
) -> superposition::Result<Json<ExperimentGroup>> {
    let req = req.into_inner();
    let DbConnection(mut conn) = db_conn;

    fetch_and_validate_change_reason_with_function(
        &workspace_context,
        &req.change_reason,
        &state,
    )
    .await?;

    let id = exp_group_id.into_inner();
    let member_experiments = fetch_and_validate_members(
        &req.member_experiment_ids,
        &[],
        &mut conn,
        &workspace_context.schema_name,
    )?;

    let experiment_group =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            validate_and_add_experiment_group_id(
                &member_experiments,
                &id,
                &workspace_context.schema_name,
                transaction_conn,
                &user,
            )?;
            add_members(
                &id,
                &member_experiments,
                req,
                transaction_conn,
                &workspace_context.schema_name,
                &user,
            )
        })?;
    Ok(experiment_group)
}

#[authorized]
#[patch("/{exp_group_id}/remove-members")]
async fn remove_members_handler(
    workspace_context: WorkspaceContext,
    exp_group_id: web::Path<i64>,
    req: Json<ExpGroupMemberRequest>,
    state: Data<AppState>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let req = req.into_inner();
    let DbConnection(mut conn) = db_conn;
    let id = exp_group_id.into_inner();

    fetch_and_validate_change_reason_with_function(
        &workspace_context,
        &req.change_reason,
        &state,
    )
    .await?;

    let experiment_group =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            validate_and_remove_experiment_group_id(
                &req.member_experiment_ids,
                &id,
                &workspace_context.schema_name,
                &state,
                transaction_conn,
                &user,
            )?;
            remove_members(
                &id,
                req,
                transaction_conn,
                &workspace_context.schema_name,
                &user,
            )
        })?;
    Ok(experiment_group)
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    pagination_params: superposition_query::Query<PaginationParams>,
    filters: superposition_query::Query<ExpGroupFilters>,
    db_conn: DbConnection,
) -> superposition::Result<Json<PaginatedResponse<ExperimentGroup>>> {
    let DbConnection(mut conn) = db_conn;
    let query_builder = |filters: &ExpGroupFilters| {
        let mut builder = experiment_groups::experiment_groups
            .schema_name(&workspace_context.schema_name)
            .into_boxed();
        if let Some(name) = &filters.name {
            builder = builder.filter(experiment_groups::name.like(format!("%{}%", name)));
        }
        if let Some(created_by) = &filters.created_by {
            builder =
                builder.filter(experiment_groups::created_by.eq(created_by.clone()));
        }
        if let Some(last_modified_by) = &filters.last_modified_by {
            builder = builder
                .filter(experiment_groups::last_modified_by.eq(last_modified_by.clone()));
        }
        if let Some(group_type) = &filters.group_type {
            builder = builder
                .filter(experiment_groups::group_type.eq_any(group_type.0.clone()));
        }
        builder
    };
    let filters = filters.into_inner();
    let base_query = query_builder(&filters);
    let count_query = query_builder(&filters);
    let sort_by = filters.sort_by.unwrap_or(SortBy::Desc);
    let sort_on = filters.sort_on.unwrap_or_default();
    #[rustfmt::skip]
    let base_query = match (sort_on, sort_by) {
        (SortOn::LastModifiedAt, SortBy::Desc) => base_query.order(experiment_groups::last_modified_at.desc()),
        (SortOn::LastModifiedAt, SortBy::Asc)  => base_query.order(experiment_groups::last_modified_at.asc()),
        (SortOn::CreatedAt, SortBy::Desc)      => base_query.order(experiment_groups::created_at.desc()),
        (SortOn::CreatedAt, SortBy::Asc)       => base_query.order(experiment_groups::created_at.asc()),
        (SortOn::Name, SortBy::Desc)           => base_query.order(experiment_groups::name.desc()),
        (SortOn::Name, SortBy::Asc)            => base_query.order(experiment_groups::name.asc()),
    };
    if let Some(true) = pagination_params.all {
        let result: ExperimentGroups =
            base_query.get_results::<ExperimentGroup>(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(result)));
    }
    let total_items = count_query.count().get_result(&mut conn)?;
    let limit = pagination_params.count.unwrap_or(10);
    let offset = (pagination_params.page.unwrap_or(1) - 1) * limit;

    let query = base_query.limit(limit).offset(offset);
    let data = query.load::<ExperimentGroup>(&mut conn)?;
    let total_pages = (total_items as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items,
        data,
    }))
}

#[authorized]
#[get("/{exp_group_id}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    exp_group_id: web::Path<i64>,
    db_conn: DbConnection,
) -> superposition::Result<Json<ExperimentGroup>> {
    let id = exp_group_id.into_inner();
    let DbConnection(mut conn) = db_conn;
    let result = experiment_groups::experiment_groups
        .schema_name(&workspace_context.schema_name)
        .filter(experiment_groups::id.eq(id))
        .first::<ExperimentGroup>(&mut conn)?;
    Ok(Json(result))
}

#[authorized]
#[delete("/{exp_group_id}")]
async fn delete_handler(
    workspace_context: WorkspaceContext,
    exp_group_id: web::Path<i64>,
    mut db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let id = exp_group_id.into_inner();
    db_conn.transaction::<Json<ExperimentGroup>, superposition::AppError, _>(|conn| {
        let marked_group = diesel::update(experiment_groups::experiment_groups)
            .filter(experiment_groups::id.eq(&id))
            .set((
                experiment_groups::last_modified_by.eq(user.email),
                experiment_groups::last_modified_at.eq(chrono::Utc::now()),
            ))
            .returning(ExperimentGroup::as_returning())
            .schema_name(&workspace_context.schema_name)
            .get_result(conn)?;
        if !marked_group.member_experiment_ids.is_empty() {
            return Err(bad_argument!(
                "Cannot delete experiment group {} since it has members",
                marked_group.name
            ));
        }
        diesel::delete(experiment_groups::experiment_groups)
            .filter(experiment_groups::id.eq(&id))
            .schema_name(&workspace_context.schema_name)
            .execute(conn)?;
        Ok(Json(marked_group))
    })
}

// Remove this after backfilling experiment groups
#[authorized]
#[post("/backfill")]
async fn backfill_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    db_conn: DbConnection,
) -> superposition::Result<Json<Vec<ExperimentGroup>>> {
    log::info!("Backfilling experiment groups");
    let DbConnection(mut conn) = db_conn;
    let user = User {
        email: "system@superposition.io".into(),
        username: "superposition".into(),
    };
    let delay = get_from_env_or_default("BACKFILL_DELAY", 100);

    let experiment_groups =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut results = vec![];

            let experiments: Vec<Experiment> = experiments::experiments
                .filter(experiments::status.eq_any(&[
                    ExperimentStatusType::INPROGRESS,
                    ExperimentStatusType::PAUSED,
                ]))
                .filter(experiments::experiment_group_id.is_null())
                .schema_name(&workspace_context.schema_name)
                .load::<Experiment>(transaction_conn)?;

            for experiment in experiments {
                let experiment_group = create_system_generated_experiment_group(
                    &experiment,
                    &experiment.traffic_percentage,
                    &state,
                    transaction_conn,
                    &workspace_context.schema_name,
                    &user,
                )?;

                diesel::update(experiments::experiments.find(experiment.id))
                    .set((
                        experiments::change_reason.eq(ChangeReason::try_from(format!(
                            "Experiment {} backfilled to group {}",
                            experiment.name, experiment_group.id
                        ))
                        .map_err(|e| unexpected_error!(e))?),
                        experiments::last_modified.eq(Utc::now()),
                        experiments::last_modified_by.eq(user.get_email()),
                        experiments::experiment_group_id.eq(experiment_group.id),
                    ))
                    .returning(Experiment::as_returning())
                    .schema_name(&workspace_context.schema_name)
                    .execute(transaction_conn)?;

                results.push(experiment_group);

                std::thread::sleep(std::time::Duration::from_millis(delay));
            }
            Ok(results)
        })?;

    Ok(Json(experiment_groups))
}
