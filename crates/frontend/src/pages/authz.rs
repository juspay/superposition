use leptos::*;
use leptos_router::A;

use crate::{
    api::casbin::AuthzScope,
    components::authz::AuthzEditor,
    types::{OrganisationId, Workspace},
};

#[component]
pub fn AdminAuthz() -> impl IntoView {
    view! {
        <div class="flex flex-col gap-4">
            <A
                href="../../organisations"
                class="text-sm text-gray-500 hover:text-gray-700 flex items-center gap-1"
            >
                <i class="ri-arrow-left-s-line" />
                "Back to Admin Dashboard"
            </A>
            <AuthzEditor
                title="Admin Authorization"
                description="Manage global permissions, role assignments, and action-groups for admin scope."
                authz_scope=AuthzScope::Admin
            />
        </div>
    }
}

#[component]
pub fn OrganisationAuthz() -> impl IntoView {
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    view! {
        <AuthzEditor
            title="Organization Authorization"
            description="Manage organization-level permissions, role assignments, and action-groups that apply across all workspaces."
            authz_scope=AuthzScope::Org(org.get_untracked().0)
        />
    }
}

#[component]
pub fn WorkspaceAuthz() -> impl IntoView {
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let workspace = use_context::<Signal<Workspace>>().unwrap();

    view! {
        <AuthzEditor
            title="Workspace Authorization"
            description="Manage workspace-level permissions, role assignments, and action-groups that apply only to this workspace."
            authz_scope=AuthzScope::Workspace(org.get_untracked().0, workspace.get_untracked().0)
        />
    }
}
