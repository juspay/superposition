use crate::{
    api::fetch_workspaces,
    components::{
        side_nav::SideNav,
        skeleton::{Skeleton, SkeletonVariant},
        toast::Toast,
    },
    providers::alert_provider::AlertQueue,
    types::{OrganisationId, Tenant},
};
use leptos::*;
use leptos_router::*;
use superposition_types::{
    custom_query::PaginationParams, database::models::Workspace, PaginatedResponse,
};

pub fn use_tenant() -> Tenant {
    let params_map = use_params_map();
    let route_context = use_route();
    logging::log!("use_route-params_map {:?}", params_map.get_untracked());
    logging::log!(
        "use_route-original_path {:?}",
        route_context.original_path()
    );
    logging::log!("use_route-path {:?}", route_context.path());

    match params_map.get_untracked().get("tenant") {
        Some(tenant) => Tenant(tenant.clone()),
        None => Tenant("no-tenant".into()),
    }
}

pub fn use_org() -> OrganisationId {
    let params_map = use_params_map();
    let route_context = use_route();
    logging::log!("use_route-params_map {:?}", params_map.get_untracked());
    logging::log!(
        "use_route-original_path {:?}",
        route_context.original_path()
    );
    logging::log!("use_route-path {:?}", route_context.path());

    match params_map.get_untracked().get("org_id") {
        Some(org) => OrganisationId(org.clone()),
        None => OrganisationId("no-org".into()),
    }
}

#[component]
fn workspace_provider(
    workspace: String,
    route: String,
    workspaces: Resource<String, PaginatedResponse<Workspace>>,
    children: ChildrenFn,
) -> impl IntoView {
    let children = StoredValue::new(children);
    let workspace = StoredValue::new(workspace);
    let workspace_context_not_needed =
        route.eq("/admin/organisations") || route.eq("/admin/:org_id/workspaces");
    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let Some(workspace_items) = workspaces.get() else {
                    logging::log!("No workspaces found");
                    return view! { <Skeleton variant=SkeletonVariant::DetailPage /> }.into_view();
                };
                if workspace_context_not_needed {
                    logging::log!("No workspace {} found", workspace.get_value());
                    return view! { <div>{children.get_value()()}</div> }.into_view();
                }
                let Some(workspace_settings) = workspace_items
                    .data
                    .into_iter()
                    .find(|w| w.workspace_name == workspace.get_value()) else {
                    return view! { <Skeleton variant=SkeletonVariant::DetailPage /> }.into_view();
                };
                logging::log!("Setting workspace context: {:#?}", workspace_settings);
                provide_context(StoredValue::new(workspace_settings));
                view! { <div>{children.get_value()()}</div> }.into_view()
            }}
        </Suspense>
    }
}

#[component]
pub fn layout(
    #[prop(default = true)] show_side_nav: bool,
    children: ChildrenFn,
) -> impl IntoView {
    let tenant_rws = create_rw_signal(use_tenant());
    let org_rws = create_rw_signal(use_org());
    provide_context(tenant_rws);
    provide_context(org_rws);

    // Signal for mobile navigation state
    let (is_mobile_nav_open, set_is_mobile_nav_open) = create_signal(false);

    let workspaces = create_blocking_resource(
        move || (org_rws.get().0),
        |org_id| async move {
            let filters = PaginationParams::all_entries();
            fetch_workspaces(&filters, &org_id)
                .await
                .unwrap_or_default()
        },
    );
    let route_context = use_route();
    let original_path = StoredValue::new(String::from(route_context.original_path()));
    let path = StoredValue::new(route_context.path());
    let children = StoredValue::new(children);
    view! {
        <WorkspaceProvider
            workspace=tenant_rws.get_untracked().0
            route=original_path.get_value()
            workspaces
        >
            // Hamburger/Close Menu Button (visible on < xl screens if side_nav is shown)
            <Show when=move || show_side_nav>
                <button
                    class="xl:hidden fixed top-4 right-4 z-[1000] btn btn-square btn-ghost bg-base-100 bg-opacity-80" // Changed left-4 to right-4
                    on:click=move |_| set_is_mobile_nav_open.update(|open| *open = !*open)
                >
                    <i class=move || if is_mobile_nav_open.get() { "ri-close-line text-xl" } else { "ri-menu-line text-xl" }></i>
                </button>
            </Show>

            // Side Navigation
            // It will control its own full-screen vs. desktop appearance via is_mobile_open prop
            <Show when=move || show_side_nav>
                <SideNav
                    resolved_path=path.get_value()
                    original_path=original_path.get_value()
                    workspace_resource=workspaces
                    is_mobile_open=is_mobile_nav_open
                />
            </Show>

            // Overlay for mobile (visible when mobile nav is open and screen < xl)
            // Re-adding this for the slide-over sidebar effect
            <Show when=move || is_mobile_nav_open.get() && show_side_nav>
                <div
                    class="xl:hidden fixed inset-0 bg-black bg-opacity-30 z-[980]" // z-index below SideNav
                    on:click=move |_| set_is_mobile_nav_open.set(false)
                ></div>
            </Show>

            // Main content area: no longer hidden by JS, overlay handles interaction blocking
            <main class=move || {
                format!(
                    "ease-soft-in-out {} relative h-full max-h-screen rounded-xl transition-all duration-200 overflow-y-auto",
                    if show_side_nav { "xl:ml-[350px]" } else { "p-4 md:p-10" }
                )
            }>{children.get_value()()}</main>

            {move || {
                let alert_queue = use_context::<ReadSignal<AlertQueue>>();
                let alerts = match alert_queue {
                    Some(queue) => queue.get().alerts,
                    None => Vec::new(),
                };
                view! { <Toast alerts /> }
            }}
        </WorkspaceProvider>
    }
}
