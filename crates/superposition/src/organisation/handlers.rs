use actix_web::{
    get, post,
    web::{self, Json, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::prelude::*;
use idgenerator::IdInstance;
use service_utils::service::types::DbConnection;
use superposition_types::database::{
    models::organisation::{OrgStatus, Organisation},
    schema::organisations::dsl::organisations,
};
use superposition_types::{
    custom_query::PaginationParams, result as superposition, PaginatedResponse, User,
};

use super::types::{CreateOrganisationRequest, CreateOrganisationResponse};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_organisation)
        .service(list_organisations)
        .service(get_organisation)
}

#[post("")]
pub async fn create_organisation(
    req: web::Json<CreateOrganisationRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    // Generating a numeric ID from IdInstance and prefixing it with `orgid`
    let numeric_id = IdInstance::next_id();
    let org_id = format!("orgid{}", numeric_id);
    let now = Utc::now().naive_utc();

    let new_org = Organisation {
        id: org_id.clone(),
        name: req.name.clone(),
        country_code: req.country_code.clone(),
        contact_email: req.contact_email.clone(),
        contact_phone: req.contact_phone.clone(),
        created_by: user.get_username(),
        admin_email: req.admin_email.clone(),
        status: OrgStatus::PendingKyb,
        sector: req.sector.clone(),
        created_at: now,
        updated_at: now,
        updated_by: user.get_username(),
    };

    diesel::insert_into(organisations)
        .values(&new_org)
        .execute(&mut conn)
        .map_err(|e| {
            log::error!("Failed to insert new organisation: {:?}", e);
            superposition::AppError::UnexpectedError(anyhow::anyhow!(
                "Failed to create organisation"
            ))
        })?;

    let mut http_resp = HttpResponse::Created();
    Ok(http_resp.json(CreateOrganisationResponse { org_id }))
}

#[get("/{org_id}")]
pub async fn get_organisation(
    org_id: web::Path<String>,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let org = organisations
        .find(org_id.as_str())
        .first::<Organisation>(&mut conn)
        .map_err(|e| {
            log::error!("Failed to fetch organisation {}: {:?}", org_id, e);
            match e {
                diesel::result::Error::NotFound => superposition::AppError::NotFound(
                    format!("Organisation {} not found", org_id),
                ),
                _ => superposition::AppError::UnexpectedError(anyhow::anyhow!(
                    "Failed to fetch organisation"
                )),
            }
        })?;

    Ok(HttpResponse::Ok().json(org))
}

#[get("/list")]
pub async fn list_organisations(
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<Organisation>>> {
    use superposition_types::database::schema::organisations::dsl::*;
    let DbConnection(mut conn) = db_conn;
    log::info!("list_organisations");
    let result =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            // If all parameter is true, return all organisations
            if let Some(true) = filters.all {
                let result: Vec<Organisation> = organisations
                    .order(created_at.desc())
                    .get_results(transaction_conn)?;
                log::info!("organisations: {organisations:?}");
                return Ok(PaginatedResponse {
                    total_pages: 1,
                    total_items: result.len() as i64,
                    data: result,
                });
            }

            // Get total count of organisations
            let total_items: i64 = organisations.count().get_result(transaction_conn)?;

            // Set up pagination
            let limit = filters.count.unwrap_or(10);
            let mut builder = organisations
                .into_boxed()
                .order(created_at.desc())
                .limit(limit);

            // Apply offset if page is specified
            if let Some(page) = filters.page {
                let offset = (page - 1) * limit;
                builder = builder.offset(offset);
            }

            // Get paginated results
            let data: Vec<Organisation> = builder.load(transaction_conn)?;

            let total_pages = (total_items as f64 / limit as f64).ceil() as i64;

            Ok(PaginatedResponse {
                total_pages,
                total_items,
                data: data,
            })
        })?;

    Ok(Json(result))
}
