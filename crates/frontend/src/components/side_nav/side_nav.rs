use std::{collections::HashSet, env::VarError, str::FromStr};

use crate::components::nav_item::nav_item::NavItem;
use crate::types::AppRoute;
use leptos::{logging::log, *};
use leptos_router::{use_location, use_navigate, A};

fn get_from_env_unsafe<F>(name: &str) -> Result<F, VarError>
where
    F: FromStr,
    <F as FromStr>::Err: std::fmt::Debug,
{
    std::env::var(name)
        .map(|val| val.parse().unwrap())
        .map_err(|e| {
            return e;
        })
}

#[component]
pub fn SideNav() -> impl IntoView {
    let location = use_location();
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let tenant_ws = use_context::<WriteSignal<String>>().unwrap();
    let (app_routes, set_app_routes) = create_signal(create_routes(&tenant_rs.get()));

    let tenants: HashSet<String> = get_from_env_unsafe::<String>("TENANTS")
        .unwrap_or("m,s".into())
        .split(",")
        .map(|tenant| tenant.to_string())
        .collect::<HashSet<String>>();

    let mut view_vector = vec![];
    for tenant in tenants.into_iter() {
        if tenant == tenant_rs.get() {
            view_vector
                .push(view! { <option selected=true>{tenant}</option> }.into_view());
        } else {
            view_vector.push(view! { <option>{tenant}</option> }.into_view());
        }
    }

    create_effect(move |_| {
        let current_path = location.pathname.get();

        set_app_routes.update(|app_routes| {
            for route in app_routes {
                if current_path.contains(&route.path) {
                    route.key = format!("{}-{}", route.path, "active");
                } else {
                    route.key = route.path.to_string();
                }
            }
        })
    });

    view! {
        <div class="max-w-xs z-990 fixed my-4 ml-4 block w-full h-full flex-wrap inset-y-0 items-center justify-between overflow-y-auto rounded-2xl border-0 bg-white p-0 shadow-none -translate-x-full transition-transform duration-200 xl:left-0 xl:translate-x-0 xl:bg-transparent">
            <div class="h-19.5">
                <A
                    href="/admin"
                    class="block px-8 py-6 m-0 text-sm whitespace-nowrap text-slate-700"
                >
                    <span class="ml-1 font-semibold transition-all duration-200">
                        Superposition Platform
                    </span>
                </A>
            </div>
            <select
                value=tenant_rs.get()
                on:change=move |change| {
                    let new_tenant = event_target_value(&change);
                    let location = use_location();
                    let mut path_tokens = location
                        .pathname
                        .get()
                        .split("/")
                        .into_iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>();
                    log!("{}{:?}", new_tenant, path_tokens);
                    path_tokens.remove(0);
                    path_tokens.remove(0);
                    path_tokens.remove(0);
                    log!("{}{:?}", new_tenant, path_tokens);
                    let nav = use_navigate();
                    set_app_routes.set(create_routes(&new_tenant));
                    tenant_ws.set(new_tenant.clone());
                    nav(
                        format!("admin/{new_tenant}/{}", path_tokens.join("/")).as_str(),
                        Default::default(),
                    );
                }

                class="select w-full max-w-xs shadow-md"
            >
                {view_vector}
            </select>
            // <hr class="h-px mt-0 mb-1 bg-transparent bg-gradient-to-r from-transparent via-black/40 to-transparent"/>
            <div class="items-center block w-auto max-h-screen overflow-auto h-sidenav grow basis-full">
                <ul class="menu">
                    <For
                        each=move || app_routes.get()
                        key=|route: &AppRoute| route.key.to_string()
                        children=move |route: AppRoute| {
                            let path = route.path.to_string();
                            let is_active = location.pathname.get().contains(&path);
                            view! {
                                <li class="mt-1 w-full">
                                    <NavItem
                                        href=route.path.to_string()
                                        icon=route.icon.to_string()
                                        text=route.label.to_string()
                                        is_active=is_active
                                    />
                                </li>
                            }
                        }
                    />

                </ul>
            </div>
        </div>
    }
}

fn create_routes(tenant: &String) -> Vec<AppRoute> {
    vec![
        AppRoute {
            key: format!("admin/{}/experiments", tenant),
            path: format!("admin/{}/experiments", tenant),
            icon: "ri-test-tube-fill".to_string(),
            label: "Experiments".to_string(),
        },
        AppRoute {
            key: format!("admin/{}/dimensions", tenant),
            path: format!("admin/{}/dimensions", tenant),
            icon: "ri-ruler-2-fill".to_string(),
            label: "Dimensions".to_string(),
        },
        AppRoute {
            key: format!("admin/{}/default-config", tenant),
            path: format!("admin/{}/default-config", tenant),
            icon: "ri-tools-line".to_string(),
            label: "Default Config".to_string(),
        },
        AppRoute {
            key: format!("admin/{}/overrides", tenant),
            path: format!("admin/{}/overrides", tenant),
            icon: "ri-guide-fill".to_string(),
            label: "Overrides".to_string(),
        },
        AppRoute {
            key: format!("admin/{}/resolve", tenant),
            path: format!("admin/{}/resolve", tenant),
            icon: "ri-equalizer-fill".to_string(),
            label: "Resolve".to_string(),
        },
    ]
}
