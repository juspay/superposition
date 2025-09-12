use crate::{
    api::workspaces,
    components::{
        side_nav::SideNav,
        skeleton::{Skeleton, SkeletonVariant},
        toast::Toast,
    },
    providers::{
        alert_provider::{AlertProvider, AlertQueue},
        csr_provider::ClientSideReadyProvider,
    },
    types::{OrganisationId, Tenant},
};
use leptos::*;
use leptos_router::*;
use superposition_types::custom_query::PaginationParams;

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
pub fn CommonLayout(children: Children) -> impl IntoView {
    view! {
        <main class="relative h-full w-full p-8 overflow-x-hidden transition-all duration-200 ease-soft-in-out">
            {children()}
        </main>
        {move || {
            let alert_queue = use_context::<ReadSignal<AlertQueue>>();
            let alerts = match alert_queue {
                Some(queue) => queue.get().alerts,
                None => Vec::new(),
            };
            view! { <Toast alerts /> }
        }}
    }
}

#[component]
pub fn Layout() -> impl IntoView {
    let workspace = use_tenant();
    let org = use_org();
    provide_context(workspace);
    provide_context(org);

    let workspace_resource = create_blocking_resource(
        move || (org.get().0),
        |org_id| async move {
            workspaces::fetch_all(&PaginationParams::all_entries(), &org_id)
                .await
                .unwrap_or_default()
        },
    );

    view! {
        <SideNav workspace_resource />
        <CommonLayout>
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
            }>
                {move || {
                    let workspace = workspace.get().0;
                    let Some(resource) = workspace_resource.get() else {
                        logging::log!("No workspaces found");
                        return view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
                            .into_view();
                    };
                    let Some(workspace_settings) = resource
                        .data
                        .iter()
                        .find(|w| w.workspace_name == workspace) else {
                        logging::log!("No such workspaces found");
                        return view! {
                            <div>
                                "No such workspace found. Please choose a valid workspace from the dropdown"
                            </div>
                        }
                            .into_view();
                    };
                    provide_context(StoredValue::new(workspace_settings.clone()));

                    view! { <Outlet /> }
                }}
            </Suspense>
        </CommonLayout>
    }
}

#[component]
pub fn providers(children: Children) -> impl IntoView {
    view! {
        <ClientSideReadyProvider>
            <AlertProvider>{children()}</AlertProvider>
        </ClientSideReadyProvider>
    }
}
