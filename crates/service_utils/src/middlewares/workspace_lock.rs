use std::future::{ready, Ready};
use std::rc::Rc;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use actix_web::{
    body::EitherBody,
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    error, http::Method,
    web::Data,
    Error,
};
use diesel::prelude::*;
use futures_util::future::LocalBoxFuture;

use crate::{
    extensions::HttpRequestExt,
    service::types::AppState,
};

/// Middleware factory for workspace locking using PostgreSQL advisory locks.
/// This ensures all write operations (POST, PUT, DELETE, PATCH) are serialized per workspace.
pub struct WorkspaceLockMiddlewareFactory;

impl WorkspaceLockMiddlewareFactory {
    pub fn new() -> Self {
        Self
    }
}

impl Default for WorkspaceLockMiddlewareFactory {
    fn default() -> Self {
        Self::new()
    }
}

impl<S, B> Transform<S, ServiceRequest> for WorkspaceLockMiddlewareFactory
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<EitherBody<B>>;
    type Error = Error;
    type InitError = ();
    type Transform = WorkspaceLockMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(WorkspaceLockMiddleware {
            service: Rc::new(service),
        }))
    }
}

pub struct WorkspaceLockMiddleware<S> {
    service: Rc<S>,
}

impl<S, B> Service<ServiceRequest> for WorkspaceLockMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<EitherBody<B>>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        let srv = self.service.clone();

        Box::pin(async move {
            // Only lock for write operations
            let is_write_operation = matches!(
                req.method(),
                &Method::POST | &Method::PUT | &Method::DELETE | &Method::PATCH
            );

            if !is_write_operation {
                // For read operations, skip locking and proceed directly
                let res = srv.call(req).await?.map_into_left_body();
                return Ok(res);
            }

            // Extract workspace and org IDs
            let workspace_id = req.request().get_workspace_id();
            let org_id = req.request().get_organisation_id();

            // If we don't have both IDs, we can't lock - proceed without locking
            // The OrgWorkspaceMiddleware will handle the validation
            let lock_keys = match (org_id, workspace_id) {
                (Some(org), Some(workspace)) => {
                    Some(compute_lock_keys(&org.0, &workspace.0))
                }
                _ => None,
            };

            // Get database connection from app state
            let app_state = match req.app_data::<Data<AppState>>() {
                Some(val) => val,
                None => {
                    log::error!("app state not set in workspace lock middleware");
                    return Err(error::ErrorInternalServerError(""));
                }
            };

            let mut db_conn = match app_state.db_pool.get() {
                Ok(conn) => conn,
                Err(e) => {
                    log::error!("failed to get database connection for workspace lock: {}", e);
                    return Err(error::ErrorInternalServerError(
                        "Failed to acquire database connection",
                    ));
                }
            };

            // Acquire advisory lock if we have lock keys and create guard
            let _lock_guard = if let Some((org_key, workspace_key)) = lock_keys {
                if let Err(e) = acquire_advisory_lock(&mut db_conn, org_key, workspace_key).await {
                    log::error!(
                        "failed to acquire advisory lock for org_key: {}, workspace_key: {}: {}",
                        org_key, workspace_key, e
                    );
                    return Err(error::ErrorInternalServerError(
                        "Failed to acquire workspace lock",
                    ));
                }
                log::debug!(
                    "acquired advisory lock for workspace (org_key: {}, workspace_key: {})",
                    org_key, workspace_key
                );
                Some(AdvisoryLockGuard::new(&mut db_conn, org_key, workspace_key))
            } else {
                None
            };

            // Call the actual handler
            // The lock guard will automatically release the lock when dropped,
            // even if the handler panics
            let result = srv.call(req).await;

            // Guard is dropped here, ensuring lock is always released
            result.map(|r| r.map_into_left_body())
        })
    }
}

/// Compute a consistent lock key pair from org_id and workspace_id
/// Returns (org_key, workspace_key) as two i32 values for pg_advisory_lock(int, int)
fn compute_lock_keys(org_id: &str, workspace_id: &str) -> (i32, i32) {
    let mut org_hasher = DefaultHasher::new();
    org_id.hash(&mut org_hasher);
    let org_hash = org_hasher.finish() as i32;

    let mut workspace_hasher = DefaultHasher::new();
    workspace_id.hash(&mut workspace_hasher);
    let workspace_hash = workspace_hasher.finish() as i32;

    (org_hash, workspace_hash)
}

/// Acquire PostgreSQL advisory lock using two-argument form with retry logic
/// Uses pg_try_advisory_lock() with exponential backoff to avoid indefinite blocking
async fn acquire_advisory_lock(
    conn: &mut PgConnection,
    org_key: i32,
    workspace_key: i32,
) -> Result<(), diesel::result::Error> {
    const MAX_RETRIES: u32 = 10;
    const INITIAL_BACKOFF_MS: u64 = 10;
    const MAX_BACKOFF_MS: u64 = 500;

    for attempt in 0..MAX_RETRIES {
        // Try to acquire the lock (non-blocking)
        let lock_acquired: bool = diesel::sql_query(
            "SELECT pg_try_advisory_lock($1, $2) as locked"
        )
            .bind::<diesel::sql_types::Integer, _>(org_key)
            .bind::<diesel::sql_types::Integer, _>(workspace_key)
            .get_result::<LockResult>(conn)?
            .locked;

        if lock_acquired {
            if attempt > 0 {
                log::info!(
                    "acquired advisory lock after {} attempts (org_key: {}, workspace_key: {})",
                    attempt + 1, org_key, workspace_key
                );
            }
            return Ok(());
        }

        // Lock not acquired, wait before retrying
        if attempt < MAX_RETRIES - 1 {
            let backoff_ms = std::cmp::min(
                INITIAL_BACKOFF_MS * 2_u64.pow(attempt),
                MAX_BACKOFF_MS
            );
            log::debug!(
                "lock contention detected, retrying in {}ms (attempt {}/{}, org_key: {}, workspace_key: {})",
                backoff_ms, attempt + 1, MAX_RETRIES, org_key, workspace_key
            );
            actix_web::rt::time::sleep(std::time::Duration::from_millis(backoff_ms)).await;
        }
    }

    // Failed to acquire lock after all retries
    Err(diesel::result::Error::DatabaseError(
        diesel::result::DatabaseErrorKind::Unknown,
        Box::new(
            format!(
                "Failed to acquire workspace lock after {} attempts (high contention)",
                MAX_RETRIES
            )
        ),
    ))
}

// Helper struct for deserializing pg_try_advisory_lock result
#[derive(QueryableByName)]
struct LockResult {
    #[diesel(sql_type = diesel::sql_types::Bool)]
    locked: bool,
}

/// Release PostgreSQL advisory lock using two-argument form
fn release_advisory_lock(
    conn: &mut PgConnection,
    org_key: i32,
    workspace_key: i32,
) -> Result<(), diesel::result::Error> {
    diesel::sql_query("SELECT pg_advisory_unlock($1, $2)")
        .bind::<diesel::sql_types::Integer, _>(org_key)
        .bind::<diesel::sql_types::Integer, _>(workspace_key)
        .execute(conn)?;
    Ok(())
}

/// RAII guard that ensures advisory lock is released even if handler panics
struct AdvisoryLockGuard<'a> {
    conn: &'a mut PgConnection,
    org_key: i32,
    workspace_key: i32,
}

impl<'a> AdvisoryLockGuard<'a> {
    fn new(conn: &'a mut PgConnection, org_key: i32, workspace_key: i32) -> Self {
        Self {
            conn,
            org_key,
            workspace_key,
        }
    }
}

impl Drop for AdvisoryLockGuard<'_> {
    fn drop(&mut self) {
        if let Err(e) = release_advisory_lock(self.conn, self.org_key, self.workspace_key) {
            log::error!(
                "failed to release advisory lock in drop guard (org_key: {}, workspace_key: {}): {}",
                self.org_key, self.workspace_key, e
            );
        } else {
            log::debug!(
                "released advisory lock via guard (org_key: {}, workspace_key: {})",
                self.org_key, self.workspace_key
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_lock_keys_consistency() {
        let keys1 = compute_lock_keys("org1", "workspace1");
        let keys2 = compute_lock_keys("org1", "workspace1");
        assert_eq!(keys1, keys2, "Same inputs should produce same lock keys");
    }

    #[test]
    fn test_compute_lock_keys_uniqueness() {
        let keys1 = compute_lock_keys("org1", "workspace1");
        let keys2 = compute_lock_keys("org1", "workspace2");
        let keys3 = compute_lock_keys("org2", "workspace1");
        let keys4 = compute_lock_keys("org2", "workspace2");

        assert_ne!(keys1, keys2, "Different workspaces should produce different keys");
        assert_ne!(keys1, keys3, "Different orgs should produce different keys");
        assert_ne!(keys1, keys4, "Different combinations should produce different keys");
        assert_ne!(keys2, keys3, "Different org/workspace combos should differ");
    }

    #[test]
    fn test_org_and_workspace_components_separate() {
        let (org_key1, workspace_key1) = compute_lock_keys("org1", "workspace1");
        let (org_key2, workspace_key2) = compute_lock_keys("org1", "workspace2");
        let (org_key3, workspace_key3) = compute_lock_keys("org2", "workspace1");

        // Same org should produce same org_key
        assert_eq!(org_key1, org_key2, "Same org should produce same org_key");

        // Different org should produce different org_key
        assert_ne!(org_key1, org_key3, "Different org should produce different org_key");

        // Same workspace should produce same workspace_key
        assert_eq!(workspace_key1, workspace_key3, "Same workspace should produce same workspace_key");

        // Different workspace should produce different workspace_key
        assert_ne!(workspace_key1, workspace_key2, "Different workspace should produce different workspace_key");
    }
}
