use leptos::*;
use leptos_router::{use_location, use_route, A};
use superposition_types::{api::workspace::WorkspaceResponse, PaginatedResponse};

use crate::components::{
    nav_item::NavItem,
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::types::{AppRoute, OrganisationId, Tenant};
use crate::utils::use_url_base;

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
            key: format!("{base}/admin/{org}/{tenant}/experiment-groups"),
            path: format!("{base}/admin/{org}/{tenant}/experiment-groups"),
            icon: "ri-flask-fill".to_string(),
            label: "Experiment Groups".to_string(),
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
pub fn workspace_selector(
    workspace_resource: Resource<String, PaginatedResponse<WorkspaceResponse>>,
    #[prop(into)] app_routes: Signal<Vec<AppRoute>>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();

    let base = use_url_base();
    let route_context = use_route();
    let original_path =
        StoredValue::new(route_context.original_path().replace(&base, ""));
    let resolved_path = StoredValue::new({
        let curr_path = route_context.path();
        app_routes
            .get_untracked()
            .iter()
            .find_map(|r| curr_path.contains(&r.path).then_some(r.path.clone()))
            .unwrap_or(curr_path)
            .replace(&base, "")
    });

    let search_term_rws = RwSignal::new(String::new());
    let csr_rws = RwSignal::new(false);
    Effect::new(move |_| csr_rws.set(true));

    let node_ref = create_node_ref::<html::Input>();

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10" /> }
        }>
            <div class="dropdown dropdown-downm w-full max-w-xs">
                <label
                    tabindex="0"
                    class="select flex items-center shadow-md"
                    on:click:undelegated=move |_| {
                        if let Some(element) = node_ref.get() {
                            let _ = element.focus();
                        }
                    }
                >
                    {move || { workspace.get().0 }}
                </label>
                <ul
                    tabindex="0"
                    class="dropdown-content menu z-[1] max-h-96 w-[inherit] p-2 flex-nowrap bg-base-100 rounded-box overflow-y-scroll overflow-x-hidden shadow"
                >
                    <Show when=move || csr_rws.get()>
                        <label class="input input-bordered mb-3 flex items-center gap-2 h-10">
                            <i class="ri-search-line"></i>
                            <input
                                type="text"
                                class="grow"
                                placeholder="Search"
                                ref_=node_ref
                                value=search_term_rws.get_untracked()
                                on:input=move |ev| search_term_rws.set(event_target_value(&ev))
                            />
                        </label>
                    </Show>
                    {move || {
                        let workspaces = workspace_resource
                            .with(|r| r.as_ref().map(|r| r.data.clone()).unwrap_or_default())
                            .into_iter()
                            .filter_map(|workspace| {
                                workspace
                                    .workspace_name
                                    .starts_with(search_term_rws.get().as_str())
                                    .then_some(workspace.workspace_name)
                            })
                            .collect::<Vec<_>>();
                        match workspaces.is_empty() {
                            false => {
                                workspaces
                                    .into_iter()
                                    .map(move |option| {
                                        let resolved_path = resolved_path.get_value();
                                        let original_path = original_path.get_value();
                                        let redirect_url = std::iter::zip(
                                                original_path.split('/'),
                                                resolved_path.split('/'),
                                            )
                                            .map(|(o_token, r_token)| match o_token {
                                                ":tenant" => &option,
                                                _ => r_token,
                                            })
                                            .map(String::from)
                                            .collect::<Vec<_>>()
                                            .join("/");
                                        let selected = workspace.get_untracked().0 == option;
                                        let disable_class = if selected {
                                            "pointer-events-none cursor-default"
                                        } else {
                                            ""
                                        };

                                        view! {
                                            <li class=format!("w-full {disable_class}")>
                                                <A class="w-full flex justify-between" href=redirect_url>
                                                    {option}
                                                    <Show when=move || selected>
                                                        <i class="ri-check-line" />
                                                    </Show>
                                                </A>
                                            </li>
                                        }
                                    })
                                    .collect_view()
                            }
                            true => {
                                view! { <option disabled=true>{"Loading..."}</option> }.into_view()
                            }
                        }
                    }}
                </ul>
            </div>
        </Suspense>
    }
}

#[component]
pub fn side_nav(
    workspace_resource: Resource<String, PaginatedResponse<WorkspaceResponse>>,
) -> impl IntoView {
    let location = use_location();
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let app_routes_rws = RwSignal::new(create_routes(
        org.get_untracked().as_str(),
        workspace.get_untracked().as_str(),
    ));

    create_effect(move |_| {
        let current_path = location.pathname.get();

        app_routes_rws.update(|app_routes| {
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
        <div class="fixed z-990 inset-y-0 xl:left-0 h-full w-full max-w-xs py-4 pl-4 flex flex-col gap-2 overflow-y-auto bg-white xl:bg-transparent rounded-2xl -translate-x-full xl:translate-x-0 transition-transform duration-200">
            <A
                href="/admin"
                class="flex-0 px-8 py-6 text-sm font-semibold text-center text-slate-700 whitespace-nowrap transition-all duration-200"
            >
                Superposition Platform
            </A>
            <WorkspaceSelector workspace_resource app_routes=app_routes_rws />
            // <hr class="h-px mt-0 mb-1 bg-transparent bg-gradient-to-r from-transparent via-black/40 to-transparent"/>
            <ul class="menu flex-1 h-full flex-nowrap gap-1 overflow-auto">
                <For
                    each=move || app_routes_rws.get()
                    key=|route: &AppRoute| route.key.to_string()
                    children=move |route: AppRoute| {
                        let path = route.path.to_string();
                        let is_active = location.pathname.get().contains(&path);
                        view! {
                            <li class="w-full">
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
    }
}
