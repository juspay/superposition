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

pub fn use_tenant() -> Signal<Tenant> {
    let params_map = use_params_map();

    Signal::derive(move || match params_map.get().get("tenant") {
        Some(tenant) => Tenant(tenant.clone()),
        None => Tenant("no-tenant".into()),
    })
}

pub fn use_org() -> Signal<OrganisationId> {
    let params_map = use_params_map();

    Signal::derive(move || match params_map.get().get("org_id") {
        Some(org) => OrganisationId(org.clone()),
        None => OrganisationId("no-org".into()),
    })
}

#[component]
fn workspace_provider(
    workspace_context_not_needed: bool,
    workspaces: Resource<String, PaginatedResponse<WorkspaceResponse>>,
    children: ChildrenFn,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let children = StoredValue::new(children);

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let workspace = workspace.get().0;
                let Some(workspace_items) = workspaces.get() else {
                    logging::log!("No workspaces found");
                    return view! { <Skeleton variant=SkeletonVariant::DetailPage /> }.into_view();
                };
                if workspace_context_not_needed {
                    logging::log!("No workspace {} found", workspace);
                    return view! { <div>{children.get_value()()}</div> }.into_view();
                }
                let Some(workspace_settings) = workspace_items
                    .data
                    .into_iter()
                    .find(|w| w.workspace_name == workspace) else {
                    return view! { <Skeleton variant=SkeletonVariant::DetailPage /> }.into_view();
                };
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
    let workspace = use_tenant();
    let org = use_org();
    provide_context(workspace);
    provide_context(org);
    let workspaces = create_blocking_resource(
        move || (org.get().0),
        |org_id| async move {
            let filters = PaginationParams::all_entries();
            fetch_workspaces(&filters, &org_id)
                .await
                .unwrap_or_default()
        },
    );
    let children = StoredValue::new(children);
    view! {
        <WorkspaceProvider workspace_context_not_needed=!show_side_nav workspaces>
            <Show when=move || show_side_nav>
                <SideNav workspace_resource=workspaces />
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
