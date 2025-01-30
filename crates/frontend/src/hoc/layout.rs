use std::ops::Deref;

use crate::{
    api::fetch_workspaces,
    components::{side_nav::SideNav, toast::Toast},
    hoc::nav_breadcrums::Breadcrumbs,
    providers::alert_provider::AlertQueue,
    types::{OrganisationId, Tenant},
    utils::use_url_base,
};

use derive_more::Deref;
use leptos::*;
use leptos_router::*;
use superposition_types::custom_query::PaginationParams;
use web_sys::Event;

#[derive(Clone, Deref)]
pub struct PageHeading(String);
impl AsRef<str> for PageHeading {
    fn as_ref(&self) -> &str {
        self.deref().as_ref()
    }
}

#[component]
pub fn layout(#[prop(into, default = true)] show_side_nav: bool) -> impl IntoView {
    let params_map = use_params_map();
    let tenant = Signal::derive(move || match params_map.get().get("tenant") {
        Some(tenant) => Tenant(tenant.clone()),
        None => Tenant("no-tenant".into()),
    });
    let org_id = Signal::derive(move || match params_map.get().get("org_id") {
        Some(org) => OrganisationId(org.clone()),
        None => OrganisationId("no-org".into()),
    });
    provide_context(tenant);
    provide_context(org_id);

    view! {
        <div class="relative p-4 flex min-h-screen bg-surface">
            <Show when=move || show_side_nav>
                <SideNav />
            </Show>

            <Outlet />
            {move || {
                let alert_queue = use_context::<ReadSignal<AlertQueue>>();
                let alerts = match alert_queue {
                    Some(queue) => queue.get().alerts,
                    None => Vec::new(),
                };
                view! { <Toast alerts /> }
            }}

        </div>
    }
}

#[component]
pub fn page(children: Children) -> impl IntoView {
    view! {
        <section class="relative w-full h-full ml-[366px] overflow-y-auto px-4">
            <Header />
            <main class="py-8 min-h-[calc(100vh-16px-12px-12px-16px-36px)]">{children()}</main>
        </section>
    }
}

#[component]
pub fn unstyled_page(children: Children) -> impl IntoView {
    view! {
        <section class="relative w-full h-full overflow-y-auto px-4">
            <main class="py-8 min-h-[calc(100vh-16px-12px-12px-16px-36px)]">{children()}</main>
        </section>
    }
}

#[component]
pub fn workspace_select() -> impl IntoView {
    let org_id = use_context::<Signal<OrganisationId>>().unwrap();
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let workspace_resource = create_blocking_resource(
        move || org_id.get().0,
        |org_id| async move {
            let filters = PaginationParams::default();
            fetch_workspaces(&filters, &org_id)
                .await
                .unwrap_or_default()
        },
    );

    view! {
        <Suspense fallback=move || {}>
            {move || {
                let workspaces = workspace_resource
                    .get()
                    .unwrap_or_default()
                    .data
                    .into_iter()
                    .map(|workspace| workspace.workspace_name)
                    .collect::<Vec<String>>();
                let tenant = tenant_s.get();
                view! {
                    <select
                        value=tenant.to_string()
                        on:change=move |event: Event| {
                            let service_prefix = use_url_base();
                            let current_tenant = tenant_s.get().0;
                            let selected_tenant = event_target_value(&event);
                            let current_base = format!("admin/{current_tenant}");
                            let new_base = format!("admin/{selected_tenant}");
                            let current_pathname = use_location()
                                .pathname
                                .get()
                                .replace(&service_prefix, "");
                            let redirect_url = current_pathname.replace(&current_base, &new_base);
                            let navigate = use_navigate();
                            navigate(redirect_url.as_str(), Default::default())
                        }

                        class="select w-full max-w-xs bg-surface-2 py-1 h-[2.25rem] min-h-[2.25rem]"
                    >

                        {move || {
                            if workspaces.is_empty() {
                                return view! {
                                    <option disabled=true>{"Loading workspaces..."}</option>
                                }
                                    .into_view();
                            }
                            workspaces
                                .iter()
                                .map(|t| {
                                    view! { <option selected=t == &(tenant.0)>{t}</option> }
                                })
                                .collect_view()
                        }}

                    </select>
                }
            }}
        </Suspense>
    }
}

#[component]
fn header() -> impl IntoView {
    view! {
        <header class="flex justify-between border-b-2 pb-3 pt-3">
            <Breadcrumbs />
            <WorkspaceSelect />
        </header>
    }
}
