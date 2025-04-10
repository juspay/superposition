use actix_web::{
    get, post, put,
    web::{self, Json, Query},
    Scope,
};
use chrono::Utc;
use diesel::prelude::*;
use idgenerator::IdInstance;
use service_utils::service::types::DbConnection;
use superposition_macros::{db_error, unexpected_error};
use superposition_types::database::{
    models::{OrgStatus, Organisation},
    superposition_schema::superposition::organisations,
};
use superposition_types::{
    custom_query::PaginationParams, result as superposition, PaginatedResponse, User,
};

use crate::organisation::types::UpdateOrganisationRequest;

use super::types::CreateOrganisationRequest;

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_organisation)
        .service(list_organisations)
        .service(get_organisation)
        .service(update_organisation)
}

#[post("")]
pub async fn create_organisation(
    request: web::Json<CreateOrganisationRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Organisation>> {
    let DbConnection(mut conn) = db_conn;

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

    let new_org = diesel::insert_into(organisations::table)
        .values(&new_org)
        .get_result(&mut conn)
        .map_err(|e| {
            log::error!("Failed to insert new organisation: {:?}", e);
            superposition::AppError::UnexpectedError(anyhow::anyhow!(
                "Failed to create organisation"
            ))
        })?;

    Ok(Json(new_org))
}

#[put("/{org_id}")]
pub async fn update_organisation(
    org_id: web::Path<String>,
    request: web::Json<UpdateOrganisationRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Organisation>> {
    let DbConnection(mut conn) = db_conn;
    let org_id = org_id.into_inner();
    let now = Utc::now();
    let req = request.into_inner();

    let org: Organisation = organisations::table
        .find(&org_id)
        .first::<Organisation>(&mut conn)
        .map_err(|e| {
            log::error!("Failed to fetch organisation {}: {:?}", org_id, e);
            db_error!(e)
        })?;

    let org = Organisation {
        admin_email: req.admin_email.unwrap_or(org.admin_email),
        country_code: if req.country_code.is_some() {
            req.country_code
        } else {
            org.country_code
        },
        contact_email: if req.contact_email.is_some() {
            req.contact_email
        } else {
            org.contact_email
        },
        contact_phone: if req.contact_phone.is_some() {
            req.contact_phone
        } else {
            org.contact_phone
        },
        sector: if req.sector.is_some() {
            req.sector
        } else {
            org.sector
        },
        updated_at: now,
        updated_by: user.email,
        status: req.status.unwrap_or(org.status),
        ..org
    };

    let updated_org = diesel::update(organisations::table)
        .filter(organisations::id.eq(org_id))
        .set(&org)
        .get_result(&mut conn)
        .map_err(|e| {
            log::error!("Failed to update organisation: {:?}", e);
            unexpected_error!("Failed to update organisation")
        })?;

    Ok(Json(updated_org))
}

#[get("/{org_id}")]
pub async fn get_organisation(
    org_id: web::Path<String>,
    db_conn: DbConnection,
) -> superposition::Result<Json<Organisation>> {
    let DbConnection(mut conn) = db_conn;

    let org = organisations::table
        .find(org_id.as_str())
        .first::<Organisation>(&mut conn)
        .map_err(|e| {
            log::error!("Failed to fetch organisation {}: {:?}", org_id, e);
            db_error!(e)
        })?;

    Ok(Json(org))
}

#[get("")]
pub async fn list_organisations(
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<Organisation>>> {
    let DbConnection(mut conn) = db_conn;
    log::info!("list_organisations");
    // If all parameter is true, return all organisations
    if let Some(true) = filters.all {
        let result: Vec<Organisation> = organisations::table
            .order(organisations::created_at.desc())
            .get_results(&mut conn)?;
        log::info!("organisations: {result:?}");
        return Ok(Json(PaginatedResponse::all(result)));
    }

    // Get total count of organisations
    let total_items: i64 = organisations::table.count().get_result(&mut conn)?;

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
    let data: Vec<Organisation> = builder.load(&mut conn)?;

    let total_pages = (total_items as f64 / limit as f64).ceil() as i64;

    Ok(Json(PaginatedResponse {
        total_pages,
        total_items,
        data,
    }))
}
