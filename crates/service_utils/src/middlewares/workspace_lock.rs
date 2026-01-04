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
use diesel::sql_types::BigInt;
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
            let lock_key = match (org_id, workspace_id) {
                (Some(org), Some(workspace)) => {
                    Some(compute_lock_key(&org.0, &workspace.0))
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

            // Acquire advisory lock if we have a lock key
            if let Some(key) = lock_key {
                if let Err(e) = acquire_advisory_lock(&mut db_conn, key) {
                    log::error!("failed to acquire advisory lock for key {}: {}", key, e);
                    return Err(error::ErrorInternalServerError(
                        "Failed to acquire workspace lock",
                    ));
                }
                log::debug!("acquired advisory lock for workspace, key: {}", key);
            }

            // Call the actual handler
            let result = srv.call(req).await;

            // Release advisory lock if we acquired one
            if let Some(key) = lock_key {
                if let Err(e) = release_advisory_lock(&mut db_conn, key) {
                    log::error!("failed to release advisory lock for key {}: {}", key, e);
                    // Continue even if unlock fails - PostgreSQL will auto-release on connection close
                }
                log::debug!("released advisory lock for workspace, key: {}", key);
            }

            // Return the result
            result.map(|r| r.map_into_left_body())
        })
    }
}

/// Compute a consistent lock key from org_id and workspace_id
fn compute_lock_key(org_id: &str, workspace_id: &str) -> i64 {
    let mut hasher = DefaultHasher::new();
    format!("{}:{}", org_id, workspace_id).hash(&mut hasher);
    let hash = hasher.finish();
    // Convert u64 to i64 for PostgreSQL bigint compatibility
    hash as i64
}

/// Acquire PostgreSQL advisory lock
fn acquire_advisory_lock(
    conn: &mut PgConnection,
    lock_key: i64,
) -> Result<(), diesel::result::Error> {
    diesel::sql_query("SELECT pg_advisory_lock($1)")
        .bind::<BigInt, _>(lock_key)
        .execute(conn)?;
    Ok(())
}

/// Release PostgreSQL advisory lock
fn release_advisory_lock(
    conn: &mut PgConnection,
    lock_key: i64,
) -> Result<(), diesel::result::Error> {
    diesel::sql_query("SELECT pg_advisory_unlock($1)")
        .bind::<BigInt, _>(lock_key)
        .execute(conn)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_lock_key_consistency() {
        let key1 = compute_lock_key("org1", "workspace1");
        let key2 = compute_lock_key("org1", "workspace1");
        assert_eq!(key1, key2, "Same inputs should produce same lock key");
    }

    #[test]
    fn test_compute_lock_key_uniqueness() {
        let key1 = compute_lock_key("org1", "workspace1");
        let key2 = compute_lock_key("org1", "workspace2");
        let key3 = compute_lock_key("org2", "workspace1");

        assert_ne!(key1, key2, "Different workspaces should produce different keys");
        assert_ne!(key1, key3, "Different orgs should produce different keys");
        assert_ne!(key2, key3, "Different combinations should produce different keys");
    }
}
