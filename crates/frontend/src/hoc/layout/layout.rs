use crate::{
    components::{side_nav::side_nav::SideNav, toast::Toast},
    providers::alert_provider::AlertQueue,
};
use leptos::*;
use leptos_router::*;

pub fn use_tenant() -> String {
    let params_map = use_params_map();
    let route_context = use_route();
    logging::log!("use_route-params_map {:?}", params_map.get());
    logging::log!(
        "use_route-original_path {:?}",
        route_context.original_path()
    );
    logging::log!("use_route-path {:?}", route_context.path());

    match params_map.get().get("tenant") {
        Some(tenant) => tenant.clone(),
        None => String::from("no-tenant"),
    }
}

#[component]
pub fn Layout(children: Children) -> impl IntoView {
    let (tenant_rs, tenant_ws) = create_signal(use_tenant());
    provide_context(tenant_rs);
    provide_context(tenant_ws);

    let route_context = use_route();
    let original_path = route_context.original_path();
    let path = route_context.path();
    // let params_map = route_context.params();

    view! {
        <div>
            <SideNav resolved_path=path original_path=original_path.to_string()/>
            // params_map=params_map
            <main class="ease-soft-in-out xl:ml-96 relative h-full max-h-screen rounded-xl transition-all duration-200 overflow-y-auto">
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
