use leptos::*;

use crate::{
    api::casbin::AuthzScope,
    pages::authz_shared::{authz_editor, AuthzContext},
    types::{OrganisationId, Workspace},
};

fn authz_body(
    workspace: Option<Signal<Workspace>>,
    org: Signal<OrganisationId>,
) -> impl IntoView {
    let context_key = {
        let workspace = workspace.clone();
        move || {
            let ws = workspace.map(|w| w.get().0.clone());
            let org_id = org.get().0.clone();
            let scope = if ws.is_some() {
                AuthzScope::Workspace
            } else {
                AuthzScope::Org
            };
            AuthzContext {
                scope,
                workspace: ws,
                org_id: Some(org_id),
            }
        }
    };

    let context_value = move || {
        let ws = workspace.map(|w| w.get_untracked().0);
        let org_id = org.get_untracked().0;
        let scope = if ws.is_some() {
            AuthzScope::Workspace
        } else {
            AuthzScope::Org
        };
        AuthzContext {
            scope,
            workspace: ws,
            org_id: Some(org_id),
        }
    };

    authz_editor(
        "Organization Authorization",
        "Manage organization-level permissions, role assignments, and action-groups that apply across all workspaces.",
        context_key,
        context_value,
    )
}

#[component]
pub fn Authz() -> impl IntoView {
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let workspace = use_context::<Signal<Workspace>>();
    authz_body(workspace, org)
}
