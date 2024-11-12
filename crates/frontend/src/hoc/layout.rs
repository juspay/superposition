use std::ops::Deref;

use crate::{
    components::{side_nav::SideNav, toast::Toast},
    hoc::nav_breadcrums::{BreadcrumbCtx, Breadcrumbs},
    providers::alert_provider::AlertQueue,
    utils::{use_tenants, use_url_base},
};

use derive_more::Deref;
use leptos::*;
use leptos_router::*;
use web_sys::Event;

#[derive(Clone, Deref)]
pub struct PageHeading(String);
impl AsRef<str> for PageHeading {
    fn as_ref(&self) -> &str {
        self.deref().as_ref()
    }
}

#[component]
pub fn Layout() -> impl IntoView {
    let params_map = use_params_map();
    let tenant = Signal::derive(move || match params_map.get().get("tenant") {
        Some(tenant) => tenant.clone(),
        None => String::from("no-tenant"),
    });
    let breadcrumbs = create_rw_signal(BreadcrumbCtx {
        current_path: String::new(),
        params: ParamsMap::new()
    });
    provide_context(breadcrumbs);
    provide_context(tenant);

    view! {
        <div class="relative p-4 flex min-h-screen bg-surface">
            <SideNav />

            <section class="relative w-full h-full ml-[366px] overflow-y-auto px-4">
                <Header tenant />
                <main class="py-8 min-h-[calc(100vh-16px-12px-12px-16px-36px)]">
                    <Outlet />
                </main>
            </section>

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
fn header(tenant: Signal<String>) -> impl IntoView {
    view! {
        <header class="flex justify-between border-b-2 pb-3 pt-3">
            <Breadcrumbs />
            <select
                value=tenant.get()
                on:change=move |event: Event| {
                    let service_prefix = use_url_base();
                    let current_tenant = tenant.get();
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
                    let tenants = use_tenants();
                    if tenants.is_empty() {
                        return view! { <option disabled=true>{"Loading tenants..."}</option> }
                            .into_view();
                    }
                    tenants
                        .iter()
                        .map(|t| {
                            view! { <option selected=t == &tenant.get()>{t}</option> }
                        })
                        .collect_view()
                }}

            </select>
        </header>
    }
}
