use crate::workspace::handlers::rotate_workspace_encryption_key_internal;
use actix_web::{
    Scope, get, post, routes,
    web::{Data, Json, Path, Query},
};
use chrono::Utc;
use diesel::prelude::*;
use idgenerator::IdInstance;
use service_utils::{
    encryption::generate_encryption_key,
    service::types::{AppState, DbConnection, WorkspaceContext},
};
use superposition_derives::authorized;
use superposition_macros::bad_argument;
use superposition_types::{
    PaginatedResponse, User, custom_query::PaginationParams, result as superposition,
};
use superposition_types::{
    api::organisation::{CreateRequest, UpdateRequest},
    api::secrets::{GenerateMasterKeyResponse, MasterKeyRotationStatus},
    database::{
        models::{OrgStatus, Organisation, Workspace},
        superposition_schema::superposition::{
            organisations::{self, updated_at, updated_by},
            workspaces::dsl as ws,
        },
    },
};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_handler)
        .service(list_handler)
        .service(get_handler)
        .service(update_handler)
        .service(generate_master_key_handler)
        .service(rotate_master_key_handler)
}

#[authorized]
#[post("")]
pub async fn create_handler(
    request: Json<CreateRequest>,
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
        .get_result(&mut conn)?;

    Ok(Json(new_org))
}

#[authorized]
#[routes]
#[put("/{org_id}")]
#[patch("/{org_id}")]
pub async fn update_handler(
    org_id: Path<String>,
    request: Json<UpdateRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Organisation>> {
    let DbConnection(mut conn) = db_conn;
    let org_id = org_id.into_inner();
    let now = Utc::now();
    let req = request.into_inner();

    let updated_org = diesel::update(organisations::table)
        .filter(organisations::id.eq(org_id))
        .set((req, updated_at.eq(now), updated_by.eq(user.get_email())))
        .get_result(&mut conn)?;

    Ok(Json(updated_org))
}

#[authorized]
#[get("/{org_id}")]
pub async fn get_handler(
    org_id: Path<String>,
    db_conn: DbConnection,
) -> superposition::Result<Json<Organisation>> {
    let DbConnection(mut conn) = db_conn;

    let org = organisations::table
        .find(org_id.as_str())
        .first::<Organisation>(&mut conn)?;

    Ok(Json(org))
}

#[authorized]
#[get("")]
pub async fn list_handler(
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
// Note: Not to be used during mid migration - to avoid old replicas from failing
#[authorized]
#[post("/rotate-master-key")]
pub async fn rotate_master_key_handler(
    workspace_context: WorkspaceContext,
    user: User,
    db_conn: DbConnection,
    app_state: Data<AppState>,
) -> superposition::Result<Json<MasterKeyRotationStatus>> {
    let DbConnection(mut conn) = db_conn;

    let new_master_key = &app_state.master_key;
    let previous_master_key =
        app_state.previous_master_key.as_ref().ok_or_else(|| {
            bad_argument!(
                "PREVIOUS_MASTER_ENCRYPTION_KEY must be set to rotate master key"
            )
        })?;

    let all_workspaces: Vec<Workspace> = ws::workspaces.load(&mut conn)?;

    let total_workspaces = all_workspaces.len() as i64;
    let user_email = user.get_email();

    let rotation_result =
        conn.transaction::<(i64, i64), superposition::AppError, _>(|conn| {
            let mut workspaces_rotated = 0i64;
            let mut total_secrets_re_encrypted = 0i64;

            for workspace in &all_workspaces {
                match rotate_workspace_encryption_key_internal(
                    &workspace_context,
                    conn,
                    new_master_key,
                    previous_master_key,
                    &user_email,
                ) {
                    Ok((secrets_count, _)) => {
                        workspaces_rotated += 1;
                        total_secrets_re_encrypted += secrets_count;
                    }
                    Err(e) => {
                        let workspace_identifier = format!(
                            "{}/{}",
                            workspace.organisation_id, workspace.workspace_name
                        );
                        log::error!(
                            "Failed to rotate keys for workspace {}: {}",
                            workspace_identifier,
                            e
                        );
                        return Err(e);
                    }
                }
            }

            Ok((workspaces_rotated, total_secrets_re_encrypted))
        })?;

    let rotation_time = chrono::Utc::now();

    log::info!(
        "Successfully rotated master key. Rotated {} workspaces, re-encrypted {} secrets.",
        rotation_result.0,
        rotation_result.1
    );

    let result = MasterKeyRotationStatus {
        workspaces_rotated: rotation_result.0,
        total_workspaces,
        total_secrets_re_encrypted: rotation_result.1,
        rotation_timestamp: rotation_time,
    };

    Ok(Json(result))
}

#[post("/master-key/generate")]
async fn generate_master_key_handler()
-> superposition::Result<Json<GenerateMasterKeyResponse>> {
    let new_key = generate_encryption_key();

    log::info!("Generated new master encryption key");

    let response = GenerateMasterKeyResponse {
        master_key: new_key,
        instructions: "1. Copy this key immediately - it will NOT be shown again.\n\
                       2. Store it securely in your secrets manager.\n\
                       3. Set it as environment variable: MASTER_ENCRYPTION_KEY=<key>\n\
                       4. Restart the service for the key to take effect."
            .to_string(),
        warning:
            "CRITICAL: Losing this key means PERMANENT loss of ALL encrypted secrets."
                .to_string(),
    };

    Ok(Json(response))
}
