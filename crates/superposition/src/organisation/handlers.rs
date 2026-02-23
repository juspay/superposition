use actix_web::{
    Scope, get, post, routes,
    web::{Data, Json, Path, Query},
};
use chrono::Utc;
use diesel::prelude::*;
use idgenerator::IdInstance;
use service_utils::{run_query, service::types::AppState};
use superposition_derives::authorized;
use superposition_types::{
    PaginatedResponse, User,
    api::organisation::{CreateRequest, UpdateRequest},
    custom_query::PaginationParams,
    database::{
        models::{OrgStatus, Organisation},
        superposition_schema::superposition::organisations::{
            self, updated_at, updated_by,
        },
    },
    result as superposition,
};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_handler)
        .service(list_handler)
        .service(get_handler)
        .service(update_handler)
}

#[authorized]
#[post("")]
pub async fn create_handler(
    request: Json<CreateRequest>,
    state: Data<AppState>,
    user: User,
) -> superposition::Result<Json<Organisation>> {
    // Generating a numeric ID from IdInstance and prefixing it with `orgid`
    let numeric_id = IdInstance::next_id();
    let org_id = format!("orgid{}", numeric_id);
    let now = Utc::now();
    let req = request.into_inner();

    let new_org = Organisation {
        id: org_id,
        name: req.name,
        country_code: req.country_code,
        contact_email: req.contact_email,
        contact_phone: req.contact_phone,
        created_by: user.get_username(),
        admin_email: req.admin_email,
        status: OrgStatus::PendingKyb,
        sector: req.sector,
        created_at: now,
        updated_at: now,
        updated_by: user.get_username(),
    };

    let new_org = run_query!(state.db_pool, conn, {
        diesel::insert_into(organisations::table)
            .values(&new_org)
            .get_result(conn)
    })?;

    Ok(Json(new_org))
}

#[authorized]
#[routes]
#[put("/{org_id}")]
#[patch("/{org_id}")]
pub async fn update_handler(
    org_id: Path<String>,
    request: Json<UpdateRequest>,
    state: Data<AppState>,
    user: User,
) -> superposition::Result<Json<Organisation>> {
    let org_id = org_id.into_inner();
    let now = Utc::now();
    let req = request.into_inner();

    let updated_org = run_query!(state.db_pool, conn, {
        diesel::update(organisations::table)
            .filter(organisations::id.eq(org_id))
            .set((req, updated_at.eq(now), updated_by.eq(user.get_email())))
            .get_result(conn)
    })?;

    Ok(Json(updated_org))
}

#[authorized]
#[get("/{org_id}")]
pub async fn get_handler(
    org_id: Path<String>,
    state: Data<AppState>,
) -> superposition::Result<Json<Organisation>> {
    let org = run_query!(state.db_pool, conn, {
        organisations::table
            .find(org_id.as_str())
            .first::<Organisation>(conn)
    })?;

    Ok(Json(org))
}

#[authorized]
#[get("")]
pub async fn list_handler(
    state: Data<AppState>,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<Organisation>>> {
    if let Some(true) = filters.all {
        let result = run_query!(state.db_pool, conn, {
            organisations::table
                .order(organisations::created_at.desc())
                .get_results(conn)
        })?;

        return Ok(Json(PaginatedResponse::all(result)));
    }

    // Get total count of organisations
    let total_items = run_query!(
        state.db_pool,
        conn,
        organisations::table.count().get_result(conn)
    )?;

    // Set up pagination
    let limit = filters.count.unwrap_or(10);
    let mut builder = organisations::table
        .into_boxed()
        .order(organisations::created_at.desc())
        .limit(limit);

    // Apply offset if page is specified
    if let Some(page) = filters.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }

    // Get paginated results
    let data = run_query!(state.db_pool, conn, builder.load(conn))?;

    let total_pages = (total_items as f64 / limit as f64).ceil() as i64;

    Ok(Json(PaginatedResponse {
        total_pages,
        total_items,
        data,
    }))
}
