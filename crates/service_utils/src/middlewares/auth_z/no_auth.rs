use futures_util::future::LocalBoxFuture;
use superposition_types::{Resource, User};

use crate::{
    middlewares::auth_z::AuthZDomain,
    service::types::{OrganisationId, SchemaName, WorkspaceId},
};

use super::authorization::Authorizer;

pub struct NoAuth;

impl Authorizer for NoAuth {
    fn is_allowed(
        &self,
        _: &AuthZDomain,
        _: &User,
        _: &Resource,
        _: &str,
        _: Option<&[&str]>,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async { Ok(true) })
    }

    fn on_org_creation(
        &self,
        _: String,
        _: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async { Ok(true) })
    }

    fn on_workspace_creation(
        &self,
        _: OrganisationId,
        _: WorkspaceId,
        _: SchemaName,
        _: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async { Ok(true) })
    }

    fn on_workspace_admin_update(
        &self,
        _: SchemaName,
        _: String,
        _: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async { Ok(true) })
    }
}
