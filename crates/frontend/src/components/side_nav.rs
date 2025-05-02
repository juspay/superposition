use crate::components::nav_item::NavItem;
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::types::{AppRoute, OrganisationId, Tenant};
use crate::utils::use_url_base;

use leptos::*;
use leptos_router::{use_location, use_navigate, A};
use superposition_types::{database::models::Workspace, PaginatedResponse};
use web_sys::Event;

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
pub fn side_nav(
    resolved_path: String,
    original_path: String,
    workspace_resource: Resource<String, PaginatedResponse<Workspace>>,
) -> impl IntoView {
    let location = use_location();
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (app_routes, set_app_routes) = create_signal(create_routes(
        org_rws.get_untracked().as_str(),
        tenant_rws.get_untracked().as_str(),
    ));
    let resolved_path = create_rw_signal(resolved_path);
    let original_path = create_rw_signal(original_path);

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
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Content /> }
            }>
                <select
                    value=tenant_rws.get().0
                    on:change=move |event: Event| {
                        let selected_tenant = event_target_value(&event);
                        let base = use_url_base();
                        let resolved_path_c = resolved_path.get().replace(&base, "");
                        let original_path_c = original_path.get().replace(&base, "");
                        logging::log!("ORIGINAL_PATH: {:?}", original_path_c);
                        let redirect_url = std::iter::zip(
                                original_path_c.split('/'),
                                resolved_path_c.split('/'),
                            )
                            .map(|(o_token, r_token)| match o_token {
                                ":tenant" => selected_tenant.clone(),
                                _ => r_token.to_string(),
                            })
                            .collect::<Vec<String>>()
                            .join("/");
                        tenant_rws.set(Tenant(selected_tenant.clone()));
                        set_app_routes
                            .set(
                                create_routes(
                                    org_rws.get_untracked().as_str(),
                                    selected_tenant.as_str(),
                                ),
                            );
                        let navigate = use_navigate();
                        navigate(redirect_url.as_str(), Default::default())
                    }

                    class="select w-full max-w-xs shadow-md"
                >

                    {move || {
                        let workspaces = workspace_resource
                            .get()
                            .unwrap_or_default()
                            .data
                            .into_iter()
                            .map(|workspace| workspace.workspace_name)
                            .collect::<Vec<String>>();
                        match workspaces.is_empty() {
                            false => {
                                workspaces
                                    .iter()
                                    .map(|tenant| {
                                        view! {
                                            <option selected=tenant
                                                == tenant_rws.get().as_str()>{tenant}</option>
                                        }
                                    })
                                    .collect::<Vec<_>>()
                            }
                            true => {
                                vec![
                                    view! { <option disabled=true>{"Loading tenants..."}</option> },
                                ]
                            }
                        }
                    }}

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
            </Suspense>
        </div>
    }
}
