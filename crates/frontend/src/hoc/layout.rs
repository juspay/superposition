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
    api::workspace::WorkspaceResponse, custom_query::PaginationParams, PaginatedResponse,
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
    workspaces: Resource<String, PaginatedResponse<WorkspaceResponse>>,
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
            <Show when=move || show_side_nav>
                <SideNav
                    resolved_path=path.get_value()
                    original_path=original_path.get_value()
                    workspace_resource=workspaces
                />
            </Show>
            // params_map=params_map
            <main class=format!(
                "ease-soft-in-out {} relative h-full max-h-screen rounded-xl transition-all duration-200 overflow-y-auto",
                if show_side_nav { "xl:ml-[350px]" } else { "p-10" },
            )>{children.get_value()()}</main>

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
