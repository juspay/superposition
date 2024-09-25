use crate::components::nav_item::NavItem;
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::types::{AppRoute, OrganisationId, Tenant};
use crate::utils::use_url_base;

use leptos::*;
use leptos_router::{use_location, A};

fn create_routes(org: &str, tenant: &str) -> Vec<AppRoute> {
    let base = use_url_base();
    vec![
        AppRoute {
            key: format!("{base}/admin/{org}/{tenant}/experiments"),
            path: format!("{base}/admin/{org}/{tenant}/experiments"),
            icon: "ri-test-tube-fill".to_string(),
            label: "Experiments".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{tenant}/function"),
            path: format!("{base}/admin/{org}/{tenant}/function"),
            icon: "ri-code-box-fill".to_string(),
            label: "Functions".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{tenant}/dimensions"),
            path: format!("{base}/admin/{org}/{tenant}/dimensions"),
            icon: "ri-ruler-2-fill".to_string(),
            label: "Dimensions".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{tenant}/default-config"),
            path: format!("{base}/admin/{org}/{tenant}/default-config"),
            icon: "ri-tools-line".to_string(),
            label: "Default Config".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{tenant}/overrides"),
            path: format!("{base}/admin/{org}/{tenant}/overrides"),
            icon: "ri-guide-fill".to_string(),
            label: "Overrides".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{tenant}/resolve"),
            path: format!("{base}/admin/{org}/{tenant}/resolve"),
            icon: "ri-equalizer-fill".to_string(),
            label: "Resolve".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{tenant}/types"),
            path: format!("{base}/admin/{org}/{tenant}/types"),
            icon: "ri-t-box-fill".to_string(),
            label: "Type Templates".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{tenant}/config/versions"),
            path: format!("{base}/admin/{org}/{tenant}/config/versions"),
            icon: "ri-camera-lens-fill".to_string(),
            label: "Config Versions".to_string(),
        },
    ]
}

#[component]
pub fn side_nav() -> impl IntoView {
    let location = use_location();
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();

    let app_routes = Signal::derive(move || {
        let tenant = tenant_s.get();
        let org = org_s.get();
        create_routes(&org, &tenant)
    });

    view! {
        <div class="fixed h-[calc(100vh-32px)] w-[350px] items-center justify-between rounded-2xl bg-surface-3">
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
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Content /> }
            }>
                <div class="items-center block w-auto grow basis-full">
                    <ul class="menu">
                        <For
                            each=move || {
                                app_routes
                                    .get()
                                    .into_iter()
                                    .map(|route| {
                                        let active = location.pathname.get().contains(&route.path);
                                        (route, active)
                                    })
                                    .collect::<Vec<(AppRoute, bool)>>()
                            }
                            key=|(route, is_active)| { format!("{}-{}", route.path, is_active) }
                            children=move |(route, active)| {
                                view! {
                                    <li class="mt-1 w-full">
                                        <NavItem
                                            href=route.path.to_string()
                                            icon=route.icon.to_string()
                                            text=route.label.to_string()
                                            active
                                        />
                                    </li>
                                }
                            }
                        />

                    </ul>
                </div>
            </Suspense>
        </div>
    }
}
