use leptos::*;
use leptos_router::{use_location, use_route, A};
use superposition_types::{api::workspace::WorkspaceResponse, PaginatedResponse};

use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::providers::csr_provider::use_client_side_ready;
use crate::types::{AppRoute, OrganisationId, Workspace};
use crate::utils::use_url_base;

fn create_routes(org: &str, workspace: &str) -> Vec<AppRoute> {
    let base = use_url_base();
    vec![
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/experiments"),
            path: format!("{base}/admin/{org}/{workspace}/experiments"),
            icon: "ri-test-tube-fill".to_string(),
            label: "Experiments".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/experiment-groups"),
            path: format!("{base}/admin/{org}/{workspace}/experiment-groups"),
            icon: "ri-flask-fill".to_string(),
            label: "Experiment Groups".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/dimensions"),
            path: format!("{base}/admin/{org}/{workspace}/dimensions"),
            icon: "ri-ruler-2-fill".to_string(),
            label: "Dimensions".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/default-config"),
            path: format!("{base}/admin/{org}/{workspace}/default-config"),
            icon: "ri-tools-line".to_string(),
            label: "Default Config".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/overrides"),
            path: format!("{base}/admin/{org}/{workspace}/overrides"),
            icon: "ri-guide-fill".to_string(),
            label: "Overrides".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/compare"),
            path: format!("{base}/admin/{org}/{workspace}/compare"),
            icon: "ri-arrow-left-right-line".to_string(),
            label: "Compare".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/types"),
            path: format!("{base}/admin/{org}/{workspace}/types"),
            icon: "ri-t-box-fill".to_string(),
            label: "Type Templates".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/variables"),
            path: format!("{base}/admin/{org}/{workspace}/variables"),
            icon: "ri-braces-line".to_string(),
            label: "Variables".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/function"),
            path: format!("{base}/admin/{org}/{workspace}/function"),
            icon: "ri-code-box-fill".to_string(),
            label: "Functions".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/webhooks"),
            path: format!("{base}/admin/{org}/{workspace}/webhooks"),
            icon: "ri-webhook-line".to_string(),
            label: "Webhooks".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/response-templates"),
            path: format!("{base}/admin/{org}/{workspace}/response-templates"),
            icon: "ri-file-text-fill".to_string(),
            label: "Response Templates".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/config/versions"),
            path: format!("{base}/admin/{org}/{workspace}/config/versions"),
            icon: "ri-camera-lens-fill".to_string(),
            label: "Config Versions".to_string(),
        },
        AppRoute {
            key: format!("{base}/admin/{org}/{workspace}/audit-log"),
            path: format!("{base}/admin/{org}/{workspace}/audit-log"),
            icon: "ri-file-list-3-line".to_string(),
            label: "Audit Log".to_string(),
        },
    ]
}

#[component]
pub fn workspace_selector(
    workspace_resource: Resource<String, PaginatedResponse<WorkspaceResponse>>,
    #[prop(into)] app_routes: Signal<Vec<AppRoute>>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let client_side_ready = use_client_side_ready();

    let base = use_url_base();
    let route_context = use_route();
    let location = use_location();
    let workspace_path_template = Signal::derive(move || {
        let curr_path = location.pathname.get();
        let current_tap_path = app_routes
            .get_untracked()
            .iter()
            .find_map(|r| curr_path.starts_with(&r.path).then_some(r.path.clone()))
            .unwrap_or(curr_path.clone());

        let resolved_path = route_context.path();
        let original_path = format!("{base}{}", route_context.original_path());
        let redirect_workspace =
            std::iter::zip(original_path.split('/'), resolved_path.split('/'))
                .map(|(o_token, r_token)| match o_token {
                    ":workspace" => ":workspace",
                    _ => r_token,
                })
                .map(String::from)
                .collect::<Vec<_>>()
                .join("/");

        current_tap_path.replace(&resolved_path, &redirect_workspace)
    });

    let search_term_rws = RwSignal::new(String::new());

    let node_ref = create_node_ref::<html::Input>();

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10" /> }
        }>
            <div class="dropdown dropdown-downm w-full max-w-xs">
                <label
                    tabindex="0"
                    class="hidden xl:group-[.collapsed]:hidden xl:flex max-xl:group-hover:flex select items-center shadow-md"
                    on:click:undelegated=move |_| {
                        if let Some(element) = node_ref.get() {
                            let _ = element.focus();
                        }
                    }
                >
                    {move || { workspace.get().0 }}
                </label>
                <div
                    tabindex="0"
                    class="xl:hidden xl:group-[.collapsed]:inline-flex max-xl:group-hover:hidden select items-center shadow-md"
                >
                    <i class="ri-briefcase-line" />
                </div>
                <ul
                    tabindex="0"
                    class="dropdown-content menu z-[1000] max-h-96 max-xl:group-hover:w-[inherit] xl:w-[inherit] xl:group-[.collapsed]:w-[unset] p-2 flex-nowrap bg-base-100 rounded-box overflow-y-scroll overflow-x-hidden shadow"
                >
                    <Show when=move || *client_side_ready.get()>
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
                                        let redirect_url = workspace_path_template
                                            .get()
                                            .replace(":workspace", &option);
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
pub fn nav_item(
    is_active: bool,
    href: String,
    text: String,
    icon: String,
) -> impl IntoView {
    let (anchor_class, icon_wrapper_class, icon_class) = if is_active {
        (
            "active",
            "text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 hover:bg-gradient-to-br focus:ring-4 focus:outline-none focus:ring-purple-300 dark:focus:ring-purple-800 shadow-lg shadow-purple-500/50 dark:shadow-lg dark:shadow-purple-800/80",
            "text-white"
        )
    } else {
        ("", "shadow-xl shadow-slate-300 bg-white", "text-purple-800")
    };

    view! {
        <A
            href
            class=format!(
                "py-2.5 px-2.5 xl:px-4 xl:group-[.collapsed]:px-2.5 max-xl:group-hover:px-4 flex items-center gap-3 whitespace-nowrap {anchor_class}",
            )
        >
            <div class=format!(
                "w-8 h-8 pt-0.5 flex content-center justify-center rounded-lg {icon_wrapper_class}",
            )>
                <i class=format!("{icon} text-lg font-normal {icon_class}") />
            </div>
            <span class="max-xl:hidden xl:group-[.collapsed]:hidden max-xl:group-hover:block duration-300 opacity-100 pointer-events-none ease-soft">
                {text}
            </span>
        </A>
    }
}

#[component]
pub fn nav_component(
    is_placeholder: bool,
    workspace_resource: Resource<String, PaginatedResponse<WorkspaceResponse>>,
    app_routes: Signal<Vec<AppRoute>>,
) -> impl IntoView {
    let collapsed_rws = RwSignal::new(false);
    let location = use_location();
    let base = use_url_base();

    move || {
        let placeholder_class = if is_placeholder {
            "max-xl:opacity-0 xl:hidden".to_string()
        } else {
            let collapsed = if collapsed_rws.get() { "collapsed" } else { "" };
            format!("group max-xl:fixed max-xl:z-[999] {collapsed}")
        };

        view! {
            <nav class=format!(
                "{placeholder_class} h-full max-xl:min-w-fit xl:[&.collapsed]:min-w-fit xl:[&.collapsed]:w-[unset] xl:w-full max-xl:hover:w-full max-w-xs pl-2 xl:pl-4 max-xl:hover:pl-4 xl:[&.collapsed]:pl-2 py-2 max-xl:pr-2 xl:[&.collapsed]:pr-2 flex flex-col gap-2 overflow-x-visible bg-gray-50 max-xl:hover:rounded-r-xl max-xl:[&.group]:shadow-lg xl:[&.collapsed]:shadow-lg transition-all duration-500",
            )>
                <div class="h-[84px] px-4 py-2 flex items-center justify-center gap-8">
                    <A
                        href=format!("{base}/admin")
                        class="max-xl:hidden max-xl:group-hover:block xl:group-[.collapsed]:hidden text-sm font-semibold text-center text-slate-700 whitespace-nowrap"
                    >
                        Superposition Platform
                    </A>
                    <i
                        class="ri-menu-line ri-xl h-10 w-fit px-2 xl:group-[.collapsed]:flex max-xl:group-hover:hidden flex justify-center items-center border-2 border-solid max-xl:border-transparent xl:hover:border-gray-400 rounded-lg cursor-pointer"
                        on:click=move |_| collapsed_rws.update(|v| *v = !*v)
                    />
                </div>
                <WorkspaceSelector workspace_resource app_routes />
                <ul class="menu flex-1 h-full w-fit xl:w-full xl:group-[.collapsed]:w-fit max-xl:group-hover:w-full !py-0 !px-2 flex-nowrap gap-1 overflow-y-auto">
                    {move || {
                        let pathname = location.pathname.get();
                        app_routes
                            .get()
                            .into_iter()
                            .map(|route| {
                                let is_active = pathname.contains(&route.path);
                                view! {
                                    <li class="w-full">
                                        <NavItem
                                            href=route.path.to_string()
                                            icon=route.icon.to_string()
                                            text=route.label.to_string()
                                            is_active
                                        />
                                    </li>
                                }
                            })
                            .collect_view()
                    }}
                </ul>
            </nav>
        }
    }
}

#[component]
pub fn side_nav(
    workspace_resource: Resource<String, PaginatedResponse<WorkspaceResponse>>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let app_routes =
        Signal::derive(move || create_routes(&org.get().0, &workspace.get().0));

    view! {
        <NavComponent is_placeholder=true workspace_resource app_routes />
        <NavComponent is_placeholder=false workspace_resource app_routes />
    }
}
