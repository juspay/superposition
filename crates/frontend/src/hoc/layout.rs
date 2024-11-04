use std::ops::Deref;

use crate::{
    components::{side_nav::SideNav, toast::Toast},
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
pub fn Layout(children: Children) -> impl IntoView {
    let route_ctx = use_route();
    let params_map = use_params_map();
    let tenant = Signal::derive(move || match params_map.get().get("tenant") {
        Some(tenant) => tenant.clone(),
        None => String::from("no-tenant"),
    });
    provide_context(tenant);

    view! {
        <div class="relative p-4 flex min-h-screen bg-surface">
            <SideNav />

            <section class="relative w-full h-full ml-[366px] overflow-y-auto px-4">
                <header class="flex justify-between border-b-2 pb-3 pt-3">
                    <nav class="flex" aria-label="Breadcrumb">
                        <ol class="inline-flex items-center space-x-1 md:space-x-2">
                            <li class="inline-flex items-center">
                                <A
                                    href="#"
                                    class="inline-flex items-center text font-medium text-blue-600"
                                >
                                    <i class="ri-test-tube-fill text-blue mr-1"></i>
                                    Experiments
                                </A>
                            </li>
                            <li aria-current="page">
                                <div class="flex items-center">
                                    <i class="ri-arrow-right-s-line"></i>
                                    <span class="ms-1 text-lg font-medium text-gray-500 md:ms-2">
                                        New
                                    </span>
                                </div>
                            </li>
                        </ol>
                    </nav>
                    <select
                        value=tenant.get()
                        on:change=move |event: Event| {
                            let base = use_url_base();
                            let resolved_path = route_ctx.path().replace(&base, "");
                            let original_path = route_ctx.original_path().replace(&base, "");
                            let redirect_url = std::iter::zip(
                                    original_path.split('/'),
                                    resolved_path.split('/'),
                                )
                                .map(|(o_token, r_token)| match o_token {
                                    ":tenant" => event_target_value(&event),
                                    _ => r_token.to_string(),
                                })
                                .collect::<Vec<String>>()
                                .join("/");
                            let navigate = use_navigate();
                            navigate(redirect_url.as_str(), Default::default())
                        }

                        class="select w-full max-w-xs bg-surface-2 py-1 h-[2.25rem] min-h-[2.25rem]"
                    >

                        {move || {
                            let tenants = use_tenants();
                            if tenants.is_empty() {
                                return view! {
                                    <option disabled=true>{"Loading tenants..."}</option>
                                }
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
                <main class="py-8 min-h-[calc(100vh-16px-12px-12px-16px-36px)]">{children()}</main>
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
