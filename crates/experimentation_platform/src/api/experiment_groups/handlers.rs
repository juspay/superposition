use actix_web::{
    delete, get, patch, post,
    web::{self, Data, Json, Query},
    Scope,
};
use diesel::{
    Connection, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
    TextExpressionMethods,
};
use serde_json::Value;
use service_utils::{
    helpers::generate_snowflake_id,
    service::types::{AppState, DbConnection, SchemaName, WorkspaceContext},
};
use superposition_macros::bad_argument;
use superposition_types::{
    api::experiment_groups::{
        ExpGroupCreateRequest, ExpGroupFilters, ExpGroupMemberRequest,
        ExpGroupUpdateRequest, SortOn,
    },
    custom_query::PaginationParams,
    database::{
        models::experimentation::{ExperimentGroup, ExperimentGroups},
        schema::experiment_groups::dsl as experiment_groups,
    },
    result as superposition, PaginatedResponse, SortBy, User,
};

use crate::api::{
    experiment_groups::helpers::{
        add_members, fetch_and_validate_members, remove_members,
        validate_experiment_group_constraints,
    },
    experiments::{
        cac_api::validate_context,
        helpers::{
            hash, validate_and_add_experiment_group_id,
            validate_and_remove_experiment_group_id,
        },
    },
};

pub fn endpoints(scope: Scope) -> Scope {
    scope
        .service(create_experiment_group)
        .service(update_experiment_group)
        .service(list_experiment_groups)
        .service(get_experiment_group)
        .service(delete_experiment_group)
        .service(add_members_to_group)
        .service(remove_members_to_group)
}

#[post("")]
async fn create_experiment_group(
    state: Data<AppState>,
    req: Json<ExpGroupCreateRequest>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
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
            &workspace_request.schema_name,
        )?
    } else {
        Vec::new()
    };

    validate_context(&state, &exp_context, &workspace_request, &user).await?;
    validate_experiment_group_constraints(
        &member_experiments,
        &[],
        &exp_context,
        &mut conn,
        &workspace_request.schema_name,
    )?;

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
    };

    let new_experiment_group =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            validate_and_add_experiment_group_id(
                &member_experiments,
                &id,
                &workspace_request.schema_name,
                transaction_conn,
                &user,
            )?;
            let new_experiment_group =
                diesel::insert_into(experiment_groups::experiment_groups)
                    .values(&new_experiment_group)
                    .returning(ExperimentGroup::as_returning())
                    .schema_name(&workspace_request.schema_name)
                    .get_result::<ExperimentGroup>(transaction_conn)?;
            Ok(new_experiment_group)
        })?;
    Ok(Json(new_experiment_group))
}

#[patch("/{exp_group_id}")]
async fn update_experiment_group(
    exp_group_id: web::Path<i64>,
    req: Json<ExpGroupUpdateRequest>,
    db_conn: DbConnection,
    schema_name: SchemaName,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let req = req.into_inner();
    let DbConnection(mut conn) = db_conn;
    let id = exp_group_id.into_inner();
    let updated_group = diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::id.eq(&id))
        .set((
            req,
            experiment_groups::last_modified_by.eq(user.email),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(&schema_name)
        .get_result(&mut conn)?;
    Ok(Json(updated_group))
}

#[patch("/{exp_group_id}/add-members")]
async fn add_members_to_group(
    exp_group_id: web::Path<i64>,
    req: Json<ExpGroupMemberRequest>,
    db_conn: DbConnection,
    schema_name: SchemaName,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let req = req.into_inner();
    let DbConnection(mut conn) = db_conn;
    let id = exp_group_id.into_inner();
    let member_experiments = fetch_and_validate_members(
        &req.member_experiment_ids,
        &[],
        &mut conn,
        &schema_name,
    )?;

    let experiment_group =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            validate_and_add_experiment_group_id(
                &member_experiments,
                &id,
                &schema_name,
                transaction_conn,
                &user,
            )?;
            add_members(
                &id,
                &member_experiments,
                req,
                transaction_conn,
                &schema_name,
                &user,
            )
        })?;
    Ok(experiment_group)
}

#[patch("/{exp_group_id}/remove-members")]
async fn remove_members_to_group(
    exp_group_id: web::Path<i64>,
    req: Json<ExpGroupMemberRequest>,
    db_conn: DbConnection,
    schema_name: SchemaName,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let req = req.into_inner();
    let DbConnection(mut conn) = db_conn;
    let id = exp_group_id.into_inner();

    let experiment_group =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            validate_and_remove_experiment_group_id(
                &req.member_experiment_ids,
                &id,
                &schema_name,
                transaction_conn,
                &user,
            )?;
            remove_members(&id, req, transaction_conn, &schema_name, &user)
        })?;
    Ok(experiment_group)
}

#[get("")]
async fn list_experiment_groups(
    pagination_params: Query<PaginationParams>,
    filters: Query<ExpGroupFilters>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<ExperimentGroup>>> {
    let DbConnection(mut conn) = db_conn;
    let pagination_params = pagination_params.into_inner();
    let query_builder = |filters: &ExpGroupFilters| {
        let mut builder = experiment_groups::experiment_groups
            .schema_name(&schema_name)
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

#[get("/{exp_group_id}")]
async fn get_experiment_group(
    exp_group_id: web::Path<i64>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<ExperimentGroup>> {
    let id = exp_group_id.into_inner();
    let DbConnection(mut conn) = db_conn;
    let result = experiment_groups::experiment_groups
        .schema_name(&schema_name)
        .filter(experiment_groups::id.eq(id))
        .first::<ExperimentGroup>(&mut conn)?;
    Ok(Json(result))
}

#[delete("/{exp_group_id}")]
async fn delete_experiment_group(
    exp_group_id: web::Path<i64>,
    mut db_conn: DbConnection,
    schema_name: SchemaName,
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
            .schema_name(&schema_name)
            .get_result(conn)?;
        if !marked_group.member_experiment_ids.is_empty() {
            return Err(bad_argument!(
                "Cannot delete experiment group {} since it has members",
                marked_group.name
            ));
        }
        diesel::delete(experiment_groups::experiment_groups)
            .filter(experiment_groups::id.eq(&id))
            .schema_name(&schema_name)
            .execute(conn)?;
        Ok(Json(marked_group))
    })
}
