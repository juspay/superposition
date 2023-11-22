use crate::components::nav_item::nav_item::NavItem;
use crate::types::AppRoute;
use leptos::*;
use leptos_router::{use_location, A};

pub fn SideNav() -> impl IntoView {
    let location = use_location();
    let (app_routes, set_app_routes) = create_signal(vec![
        AppRoute {
            key: "/admin/experiments".to_string(),
            path: "/admin/experiments".to_string(),
            icon: "ri-test-tube-fill".to_string(),
            label: "Experiments".to_string(),
        },
        AppRoute {
            key: "/admin/dimensions".to_string(),
            path: "/admin/dimensions".to_string(),
            icon: "ri-ruler-2-fill".to_string(),
            label: "Dimensions".to_string(),
        },
        AppRoute {
            key: "/admin/default-config".to_string(),
            path: "/admin/default-config".to_string(),
            icon: "ri-tools-line".to_string(),
            label: "Default Config".to_string(),
        },
        AppRoute {
            key: "/admin/overrides".to_string(),
            path: "/admin/overrides".to_string(),
            icon: "ri-guide-fill".to_string(),
            label: "Overrides".to_string(),
        },
        AppRoute {
            key: "/admin/resolve".to_string(),
            path: "/admin/resolve".to_string(),
            icon: "ri-equalizer-fill".to_string(),
            label: "Resolve".to_string(),
        },
    ]);

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
                <A href="/" class="block px-8 py-6 m-0 text-sm whitespace-nowrap text-slate-700">
                    <span class="ml-1 font-semibold transition-all duration-200">
                        Superposition Platform
                    </span>
                </A>
            </div>
            <hr class="h-px mt-0 mb-1 bg-transparent bg-gradient-to-r from-transparent via-black/40 to-transparent"/>
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
                                        href={route.path.to_string()}
                                        icon={route.icon.to_string()}
                                        text={route.label.to_string()}
                                        is_active={is_active}
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
