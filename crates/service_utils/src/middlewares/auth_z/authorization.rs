use actix_web::Scope;
use futures_util::future::LocalBoxFuture;
use superposition_types::User;

use crate::service::types::{Resource, WorkspaceContext};

pub trait Authorizer: Sync + Send {
    fn management_routes(&self) -> Scope;

    fn is_allowed(
        &self,
        workspace_request: &WorkspaceContext,
        user: &User,
        resource: &Resource,
        action: &str,
        attributes: Option<&String>,
    ) -> LocalBoxFuture<'_, Result<bool, String>>;

    // async fn get_permitted_attributes(
    //     &self,
    //     workspace_request: &WorkspaceContext,
    //     user: &User,
    //     resource: &ResourceContext,
    //     action: &Action,
    // ) -> Result<Vec<String>, String>;

    // async fn enforce_with_context(
    //     &self,
    //     workspace_request: &WorkspaceContext,
    //     user: &User,
    //     resource: &ResourceContext,
    //     action: &Action,
    //     context: HashMap<String, Value>,
    // ) -> Result<bool>;
}
