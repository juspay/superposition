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
            key: format!("{base}/admin/{org}/{tenant}/compare"),
            path: format!("{base}/admin/{org}/{tenant}/compare"),
            icon: "ri-arrow-left-right-line".to_string(),
            label: "Compare".to_string(),
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
        AppRoute {
            key: format!("{base}/admin/{org}/{tenant}/webhooks"),
            path: format!("{base}/admin/{org}/{tenant}/webhooks"),
            icon: "ri-webhook-line".to_string(),
            label: "Webhooks".to_string(),
        },
    ]
}

#[component]
pub fn side_nav(
    resolved_path: String,
    original_path: String,
    workspace_resource: Resource<String, PaginatedResponse<Workspace>>,
    is_mobile_open: ReadSignal<bool>, // New prop
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

    let nav_class = move || {
        if is_mobile_open.get() {
            // Mobile: Nav Open - Responsive width up to 360px
            "fixed top-0 left-0 bottom-0 z-[990] flex flex-col w-full max-w-[360px] h-screen bg-base-100 shadow-xl \
            transition-transform duration-300 ease-in-out translate-x-0".to_string()
            // w-full takes full screen on very small devices, max-w-[360px] caps it otherwise.
        } else {
            // Mobile: Nav Closed OR Desktop view (original styling)
            format!(
                "max-w-xs z-[990] fixed my-4 ml-4 flex flex-col w-full h-[calc(100vh-2rem)] \
                rounded-2xl border-0 bg-base-100 p-0 shadow-lg xl:shadow-none \
                transition-transform duration-300 ease-in-out xl:left-0 \
                xl:bg-transparent {}", // bg-base-100 for mobile closed consistency, xl:bg-transparent for desktop
                "-translate-x-full xl:translate-x-0"
            )
        }
    };

    // Padding for internal elements: use consistent padding for both mobile open and desktop
    // No need for conditional classes here if the fixed width provides enough space.
    // The p-0 on the desktop version's root means internal elements define their padding.
    // For mobile open (w-[360px]), we can also rely on internal padding.
    // Let's ensure the internal elements have consistent padding.
    // The header_wrapper_class, select_wrapper_class, menu_ul_class will now be static.

    let header_wrapper_class = "h-19.5 px-4 pt-4"; // Revert to original desktop style, should be fine for 360px width
    let select_wrapper_class = "px-4 mt-2 mb-4"; // Revert to original
    let menu_ul_class = "menu px-4"; // Revert to original

    view! {
        <div class=nav_class>
            <div class=header_wrapper_class>
                <A
                    href="/admin"
                    class="block py-2 m-0 text-sm whitespace-nowrap text-slate-700"
                >
                    <span class="ml-1 font-semibold transition-all duration-200 text-lg">
                        Superposition
                    </span>
                </A>
            </div>
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Content /> }
            }>
                <div class=select_wrapper_class>
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
                        class="select select-bordered w-full max-w-xs text-sm" // Added select-bordered
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
                </div>
                // <hr class="h-px mt-0 mb-1 bg-transparent bg-gradient-to-r from-transparent via-black/40 to-transparent"/>
                <div class="flex-1 overflow-y-auto pb-6"> // Added more pb for scroll spacing at bottom
                    <ul class=menu_ul_class> // Use new class
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
