use std::collections::{HashMap, HashSet};

use futures_util::future::LocalBoxFuture;
use superposition_types::{Resource, User, api::authz::ResourceActionType};

use crate::{
    middlewares::auth_z::AuthZDomain, registry::ActionRegistry,
    service::types::SchemaName,
};

pub trait Authorizer: Sync + Send {
    fn on_org_creation(
        &self,
        organisation_id: String,
        org_admin_email: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>>;

    fn on_workspace_creation(
        &self,
        schema_name: SchemaName,
        workspace_admin_email: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>>;

    fn on_workspace_admin_update(
        &self,
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

    fn get_source_resource_action_map(
        &self,
    ) -> HashMap<Resource, HashSet<ResourceActionType>> {
        ActionRegistry::group_by_resource()
            .into_iter()
            .map(|(resource, actions)| {
                let action_names = actions
                    .into_iter()
                    .map(|action| {
                        ResourceActionType::Action(action.action_name.to_string())
                    })
                    .collect();
                (resource, action_names)
            })
            .collect::<HashMap<_, _>>()
    }

    // fn get_permitted_attributes<A: Action>(
    //     &self,
    //     domain: &AuthZDomain,
    //     user: &User,
    //     resource: &Resource,
    //     action: &AuthZ<A>,
    // ) -> LocalBoxFuture<'_, Result<PermittedAttributes, String>>;

    // async fn enforce_with_context(
    //     &self,
    //     domain: &AuthZDomain,
    //     user: &User,
    //     resource: &ResourceContext,
    //     action: &Action,
    //     context: HashMap<String, Value>,
    // ) -> Result<bool>;
}
