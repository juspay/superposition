use leptos::*;

use crate::{
    api::casbin::AuthzScope,
    pages::authz_shared::{authz_editor, AuthzContext},
};

#[component]
pub fn AuthzAdmin() -> impl IntoView {
    let context = || AuthzContext {
        scope: AuthzScope::Admin,
        workspace: None,
        org_id: None,
    };

    authz_editor(
        "Admin Authorization",
        "Manage global permissions, role assignments, and action-groups for admin scope.",
        context,
        context,
    )
}
