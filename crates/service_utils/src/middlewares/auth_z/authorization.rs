use futures_util::future::LocalBoxFuture;
use superposition_types::User;

use crate::service::types::{OrganisationId, Resource, SchemaName};

pub trait Authorizer: Sync + Send {
    fn on_org_creation(
        &self,
        organisation_id: &str,
    ) -> LocalBoxFuture<'_, Result<bool, String>>;

    fn is_allowed(
        &self,
        workspace_context: &(OrganisationId, SchemaName),
        user: &User,
        resource: &Resource,
        action: &str,
        attributes: Option<&[&str]>,
    ) -> LocalBoxFuture<'_, Result<bool, String>>;

    // async fn get_permitted_attributes(
    //     &self,
    //     workspace_context: &(OrganisationId, SchemaName),
    //     user: &User,
    //     resource: &ResourceContext,
    //     action: &Action,
    // ) -> Result<Vec<String>, String>;

    // async fn enforce_with_context(
    //     &self,
    //     workspace_context: &(OrganisationId, SchemaName),
    //     user: &User,
    //     resource: &ResourceContext,
    //     action: &Action,
    //     context: HashMap<String, Value>,
    // ) -> Result<bool>;
}
