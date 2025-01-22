use crate::{
    components::{side_nav::SideNav, toast::Toast},
    providers::alert_provider::AlertQueue,
    types::{OrganisationId, Tenant},
};
use leptos::*;
use leptos_router::*;

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
pub fn layout(
    #[prop(default = true)] show_side_nav: bool,
    children: Children,
) -> impl IntoView {
    let tenant_rws = create_rw_signal(use_tenant());
    let org_rws = create_rw_signal(use_org());
    provide_context(tenant_rws);
    provide_context(org_rws);

    let route_context = use_route();
    let original_path = String::from(route_context.original_path());
    let path = route_context.path();
    // let params_map = route_context.params();

    view! {
        <div>
            <Show
            when=move || show_side_nav
            >
                <SideNav resolved_path=path.clone() original_path=original_path.clone()/>
            </Show>
            // params_map=params_map
            <main class={format!("ease-soft-in-out {} relative h-full max-h-screen rounded-xl transition-all duration-200 overflow-y-auto", if show_side_nav {
                "xl:ml-[350px]"
            } else {
                "p-10"
            })}>
                {children()}
            </main>

            {move || {
                let alert_queue = use_context::<ReadSignal<AlertQueue>>();
                let alerts = match alert_queue {
                    Some(queue) => queue.get().alerts,
                    None => Vec::new(),
                };
                view! { <Toast alerts/> }
            }}

        </div>
    }
}
