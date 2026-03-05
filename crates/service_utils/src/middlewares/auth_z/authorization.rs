use futures_util::future::LocalBoxFuture;
use superposition_types::{Resource, User};

use crate::{
    middlewares::auth_z::AuthZDomain,
    service::types::{OrganisationId, SchemaName, WorkspaceId},
};

pub trait Authorizer: Sync + Send {
    fn on_org_creation(
        &self,
        organisation_id: String,
        org_admin_email: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>>;

    fn on_workspace_creation(
        &self,
        organisation_id: OrganisationId,
        workspace_id: WorkspaceId,
        schema_name: SchemaName,
        workspace_admin_email: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>>;

    fn on_workspace_admin_update(
        &self,
        organisation_id: OrganisationId,
        schema_name: SchemaName,
        old_admin_email: String,
        new_admin_email: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>>;

    fn is_allowed(
        &self,
        domain: &AuthZDomain,
        user: &User,
        resource: &Resource,
        action: &str,
        attributes: Option<&[&str]>,
    ) -> LocalBoxFuture<'_, Result<bool, String>>;

    // async fn get_permitted_attributes(
    //     &self,
    //     domain: &AuthZDomain,
    //     user: &User,
    //     resource: &ResourceContext,
    //     action: &Action,
    // ) -> Result<Vec<String>, String>;

    // async fn enforce_with_context(
    //     &self,
    //     domain: &AuthZDomain,
    //     user: &User,
    //     resource: &ResourceContext,
    //     action: &Action,
    //     context: HashMap<String, Value>,
    // ) -> Result<bool>;
}
