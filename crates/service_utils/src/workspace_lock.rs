use std::{fmt, time::Duration};

use actix_web::{
    HttpResponse,
    error::ResponseError,
    http::{StatusCode, header::ContentType},
};
use chrono::{DateTime, Utc};
use diesel::{
    BoolExpressionMethods, ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl,
    SelectableHelper,
};
use serde::Serialize;
use superposition_types::{
    DBConnection,
    api::workspace::WorkspaceLock,
    database::{models::Workspace, superposition_schema::superposition::workspaces::dsl},
    result::{self as superposition},
};
use uuid::Uuid;

#[derive(Clone, Debug)]
pub(crate) struct WorkspaceLockRequest {
    pub(crate) operation: String,
    pub(crate) locked_by: String,
    pub(crate) ttl: Duration,
}

impl WorkspaceLockRequest {
    pub(crate) fn expires_at(&self, now: DateTime<Utc>) -> DateTime<Utc> {
        chrono::Duration::from_std(self.ttl)
            .map(|ttl| now + ttl)
            .unwrap_or_else(|_| now + chrono::Duration::minutes(1))
    }
}

#[derive(Clone, Debug)]
pub(crate) struct WorkspaceLockLease {
    pub(crate) organisation_id: String,
    pub(crate) workspace_name: String,
    pub(crate) lock_id: Uuid,
    pub(crate) owns_lock: bool,
}

#[derive(Debug)]
pub(crate) enum AcquireLockError {
    Diesel(diesel::result::Error),
    LockContended(WorkspaceLock),
}

impl fmt::Display for AcquireLockError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Diesel(e) => write!(f, "database error: {}", e),
            Self::LockContended(lock) => {
                write!(
                    f,
                    "workspace is locked by {} for {} until {}",
                    lock.locked_by, lock.operation, lock.expires_at
                )
            }
        }
    }
}

impl From<diesel::result::Error> for AcquireLockError {
    fn from(e: diesel::result::Error) -> Self {
        Self::Diesel(e)
    }
}

#[derive(Debug, Serialize)]
struct WorkspaceLockConflictBody {
    message: String,
    lock: WorkspaceLock,
}

#[derive(Debug)]
pub(crate) struct WorkspaceLockConflict {
    body: WorkspaceLockConflictBody,
}

impl WorkspaceLockConflict {
    pub(crate) fn new(lock: WorkspaceLock) -> Self {
        Self {
            body: WorkspaceLockConflictBody {
                message: "Workspace is busy with another write request; retry later"
                    .to_string(),
                lock,
            },
        }
    }
}

impl fmt::Display for WorkspaceLockConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.body.message)
    }
}

impl ResponseError for WorkspaceLockConflict {
    fn status_code(&self) -> StatusCode {
        StatusCode::CONFLICT
    }

    fn error_response(&self) -> HttpResponse {
        HttpResponse::Conflict()
            .insert_header(ContentType::json())
            .json(&self.body)
    }
}

impl ResponseError for AcquireLockError {
    fn status_code(&self) -> StatusCode {
        match self {
            Self::LockContended(_) => StatusCode::CONFLICT,
            Self::Diesel(diesel::result::Error::NotFound) => StatusCode::NOT_FOUND,
            Self::Diesel(_) => StatusCode::INTERNAL_SERVER_ERROR,
        }
    }

    fn error_response(&self) -> HttpResponse {
        match self {
            Self::LockContended(lock) => {
                WorkspaceLockConflict::new(lock.clone()).error_response()
            }
            Self::Diesel(diesel::result::Error::NotFound) => {
                HttpResponse::build(StatusCode::NOT_FOUND)
                    .insert_header(ContentType::json())
                    .json("Workspace not found")
            }
            Self::Diesel(e) => {
                log::error!("failed to acquire workspace lock: {}", e);
                HttpResponse::build(StatusCode::INTERNAL_SERVER_ERROR)
                    .insert_header(ContentType::json())
                    .json("Failed to acquire workspace lock")
            }
        }
    }
}

pub(crate) fn acquire_workspace_lease(
    conn: &mut DBConnection,
    organisation_id: &str,
    workspace_id: &str,
    request: WorkspaceLockRequest,
) -> Result<WorkspaceLockLease, AcquireLockError> {
    let lock_id = Uuid::new_v4();
    let now = Utc::now();
    let expires_at = request.expires_at(now);

    let updated_rows = diesel::update(dsl::workspaces)
        .filter(dsl::organisation_id.eq(organisation_id))
        .filter(dsl::workspace_name.eq(workspace_id))
        .filter(
            dsl::workspace_lock_expires_at
                .is_null()
                .or(dsl::workspace_lock_expires_at.le(now)),
        )
        .set((
            dsl::workspace_lock_id.eq(lock_id),
            dsl::workspace_lock_operation.eq(Some(request.operation)),
            dsl::workspace_locked_by.eq(Some(request.locked_by)),
            dsl::workspace_lock_acquired_at.eq(Some(now)),
            dsl::workspace_lock_expires_at.eq(Some(expires_at)),
        ))
        .execute(conn)?;

    if updated_rows == 1 {
        return Ok(WorkspaceLockLease {
            organisation_id: organisation_id.to_string(),
            workspace_name: workspace_id.to_string(),
            lock_id,
            owns_lock: true,
        });
    }

    let active_lock = get_active_workspace_lock(conn, organisation_id, workspace_id)?;
    match active_lock {
        Some(lock) => Err(AcquireLockError::LockContended(lock)),
        None => Err(AcquireLockError::Diesel(diesel::result::Error::NotFound)),
    }
}

pub(crate) fn release_workspace_lease(
    conn: &mut DBConnection,
    lease: WorkspaceLockLease,
) -> superposition::Result<()> {
    if !lease.owns_lock {
        return Ok(());
    }

    diesel::update(dsl::workspaces)
        .filter(dsl::organisation_id.eq(&lease.organisation_id))
        .filter(dsl::workspace_name.eq(&lease.workspace_name))
        .filter(dsl::workspace_lock_id.eq(lease.lock_id))
        .set((
            dsl::workspace_lock_id.eq::<Option<Uuid>>(None),
            dsl::workspace_lock_operation.eq::<Option<String>>(None),
            dsl::workspace_locked_by.eq::<Option<String>>(None),
            dsl::workspace_lock_acquired_at.eq::<Option<DateTime<Utc>>>(None),
            dsl::workspace_lock_expires_at.eq::<Option<DateTime<Utc>>>(None),
        ))
        .execute(conn)?;
    Ok(())
}

fn get_active_workspace_lock(
    conn: &mut superposition_types::DBConnection,
    organisation_id: &str,
    workspace_id: &str,
) -> Result<Option<WorkspaceLock>, diesel::result::Error> {
    let workspace = dsl::workspaces
        .filter(dsl::organisation_id.eq(organisation_id))
        .filter(dsl::workspace_name.eq(workspace_id))
        .select(Workspace::as_select())
        .first::<Workspace>(conn)
        .optional()?;

    Ok(workspace.and_then(|w| WorkspaceLock::from_workspace(&w)))
}
