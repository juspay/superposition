use leptos::*;
use leptos_router::*;
use once_cell::sync::Lazy;
use std::{collections::HashMap, sync::Arc};

use crate::utils::use_url_base;

// Types and Structures
#[derive(Clone)]
struct RouteMetadata {
    default_label: String,
    parent: Option<String>,
    icon: Option<String>,
    dynamic_label: Option<DynamicLabelFn>,
}

type DynamicLabelFn = Box<Arc<dyn Fn(&ParamsMap) -> Option<String> + Send + Sync>>;

static BREADCRUMB_STORE: Lazy<HashMap<String, RouteMetadata>> = Lazy::new(|| {
    let mut routes = HashMap::new();

    // Dimensions
    routes.insert(
        "/dimensions".to_string(),
        RouteMetadata {
            default_label: "Dimensions".to_string(),
            parent: None,
            icon: Some("ri-ruler-2-fill".to_string()),
            dynamic_label: None,
        },
    );

    routes.insert(
        "/dimensions/new".to_string(),
        RouteMetadata {
            default_label: "New".to_string(),
            parent: Some("/dimensions".to_string()),
            icon: None,
            dynamic_label: None,
        },
    );

    routes.insert(
        "/dimensions/:name/update".to_string(),
        RouteMetadata {
            default_label: "Update Dimension".to_string(),
            parent: Some("/dimensions".to_string()),
            icon: None,
            dynamic_label: Some(Box::new(Arc::new(|params| {
                params.get("name").map(|name| format!("Update {}", name))
            }))),
        },
    );

    // Functions
    routes.insert(
        "/function".to_string(),
        RouteMetadata {
            default_label: "Functions".to_string(),
            parent: None,
            icon: Some("ri-code-box-fill".to_string()),
            dynamic_label: None,
        },
    );

    routes.insert(
        "/function/create".to_string(),
        RouteMetadata {
            default_label: "Create Function".to_string(),
            parent: Some("/function".to_string()),
            icon: None,
            dynamic_label: None,
        },
    );

    routes.insert(
        "/function/:function_name".to_string(),
        RouteMetadata {
            default_label: "Function Details".to_string(),
            parent: Some("/function".to_string()),
            icon: None,
            dynamic_label: Some(Box::new(Arc::new(|params| {
                params.get("function_name").map(|name| name.to_string())
            }))),
        },
    );

    // Experiments
    routes.insert(
        "/experiments".to_string(),
        RouteMetadata {
            default_label: "Experiments".to_string(),
            parent: None,
            icon: Some("ri-test-tube-fill".to_string()),
            dynamic_label: None,
        },
    );

    routes.insert(
        "/experiments/new".to_string(),
        RouteMetadata {
            default_label: "New Experiment".to_string(),
            parent: Some("/experiments".to_string()),
            icon: None,
            dynamic_label: None,
        },
    );

    routes.insert(
        "/experiments/:id".to_string(),
        RouteMetadata {
            default_label: "Experiment Details".to_string(),
            parent: Some("/experiments".to_string()),
            icon: None,
            dynamic_label: Some(Box::new(Arc::new(|params| {
                params.get("id").map(|id| format!("{}", id))
            }))),
        },
    );

    // default-config
    routes.insert(
        "/default-config".to_string(),
        RouteMetadata {
            default_label: "Default Config".to_string(),
            parent: None,
            icon: Some("ri-tools-line".to_string()),
            dynamic_label: None,
        },
    );

    routes.insert(
        "/default-config/new".to_string(),
        RouteMetadata {
            default_label: "New".to_string(),
            parent: Some("/default-config".to_string()),
            icon: None,
            dynamic_label: None,
        },
    );

    routes.insert(
        "/default-config/:key/update".to_string(),
        RouteMetadata {
            default_label: "Update Default Config".to_string(),
            parent: Some("/default-config".to_string()),
            icon: None,
            dynamic_label: Some(Box::new(Arc::new(|params| {
                params.get("key").map(|name| format!("Update {}", name))
            }))),
        },
    );

    // overrides
    routes.insert(
        "/overrides".to_string(),
        RouteMetadata {
            default_label: "Overrides".to_string(),
            parent: None,

            icon: Some("ri-guide-fill".to_string()),
            dynamic_label: None,
        },
    );

    routes.insert(
        "/overrides/new".to_string(),
        RouteMetadata {
            default_label: "New".to_string(),
            parent: Some("/overrides".to_string()),
            icon: None,
            dynamic_label: None,
        },
    );

    routes.insert(
        "/overrides/:id/update".to_string(),
        RouteMetadata {
            default_label: "Update".to_string(),
            parent: Some("/overrides".to_string()),
            icon: None,
            dynamic_label: Some(Box::new(Arc::new(|params| {
                params.get("id").map(|name| format!("Update {}", name))
            }))),
        },
    );

    routes.insert(
        "/resolve".to_string(),
        RouteMetadata {
            default_label: "Resolve".to_string(),
            parent: None,
            icon: Some("ri-equalizer-fill".to_string()),
            dynamic_label: None,
        },
    );

    // type templates
    routes.insert(
        "/types".to_string(),
        RouteMetadata {
            default_label: "Types".to_string(),
            parent: None,
            icon: Some("ri-t-box-fill".to_string()),
            dynamic_label: None,
        },
    );

    routes.insert(
        "/types/new".to_string(),
        RouteMetadata {
            default_label: "New Type".to_string(),
            parent: Some("/types".to_string()),
            icon: None,
            dynamic_label: None,
        },
    );

    routes.insert(
        "/types/:type_name/update".to_string(),
        RouteMetadata {
            default_label: "Update Type".to_string(),
            parent: Some("/types".to_string()),
            icon: None,
            dynamic_label: Some(Box::new(Arc::new(|params| {
                params
                    .get("type_name")
                    .map(|name| format!("Update {}", name))
            }))),
        },
    );

    // config versions
    routes.insert(
        "/config/versions".to_string(),
        RouteMetadata {
            default_label: "Config Versions".to_string(),
            parent: None,
            icon: Some("ri-camera-lens-fill".to_string()),
            dynamic_label: None,
        },
    );
    routes.insert(
        "/config/versions/:version".to_string(),
        RouteMetadata {
            default_label: "Config Versions Detail".to_string(),
            parent: Some("/config/versions".to_string()),
            icon: None,
            dynamic_label: Some(Box::new(Arc::new(|params| {
                params.get("version").map(|name| format!("{}", name))
            }))),
        },
    );

    routes
});

// Store Implementation
pub fn get_breadcrumbs(
    current_path: &str,
    params: &ParamsMap,
) -> Vec<(String, String, Option<String>)> {
    let mut breadcrumbs = Vec::new();
    let mut current = Some(current_path.to_string());

    while let Some(path) = current {
        if let Some(metadata) = BREADCRUMB_STORE.get(&path) {
            // Replace path parameters with actual values
            let mut resolved_path = path.clone();
            for (param_name, param_value) in params.0.clone() {
                resolved_path =
                    resolved_path.replace(&format!(":{}", param_name), &param_value);
            }

            // Get label using dynamic handler if available
            let label = metadata
                .dynamic_label
                .as_ref()
                .and_then(|handler| handler(params))
                .unwrap_or(metadata.default_label.clone());

            breadcrumbs.push((resolved_path, label, metadata.icon.clone()));
            current = metadata.parent.clone();
        } else {
            break;
        }
    }

    breadcrumbs.reverse();
    breadcrumbs
}

#[component]
pub fn breadcrumbs() -> impl IntoView {
    let params = use_params_map();
    let tenant = use_context::<Signal<String>>().unwrap();

    let breadcrumbs = create_memo(move |_| {
        let base = format!("{}/admin/:tenant", use_url_base());
        let current_path = use_route().original_path().replace(&base, "");
        let params = params.get();
        logging::log!("BREAD {current_path}");
        logging::log!("BREAD {:?}", params.0);
        get_breadcrumbs(&current_path, &params)
    });

    view! {
        <nav class="flex" aria-label="Breadcrumb">
            <ol class="inline-flex items-center gap-x-2">
                {move || {
                    breadcrumbs
                        .get()
                        .into_iter()
                        .enumerate()
                        .map(|(index, (path, label, icon))| {
                            let is_last = index == breadcrumbs.get().len() - 1;
                            let show_separator = index > 0;
                            let base = format!("{}/admin/{}", use_url_base(), tenant.get());
                            let path = format!("{}{}", base, path);
                            view! {
                                <li class="flex items-center">
                                    {if show_separator {
                                        view! { <i class="ri-arrow-right-s-line me-2"></i> }
                                            .into_view()
                                    } else {
                                        view! { <></> }.into_view()
                                    }}
                                    {if is_last {
                                        view! {
                                            <span class="font-medium text-gray-500 inline-flex items-center">
                                                {if let Some(i) = icon {
                                                    view! { <i class=format!("{i} mr-1")></i> }.into_view()
                                                } else {
                                                    view! { <></> }.into_view()
                                                }} {label}
                                            </span>
                                        }
                                            .into_view()
                                    } else {
                                        view! {
                                            <A
                                                href=path
                                                class="inline-flex items-center text font-medium text-blue-600"
                                            >
                                                {if let Some(i) = icon {
                                                    view! { <i class=format!("{i} mr-1")></i> }.into_view()
                                                } else {
                                                    view! { <></> }.into_view()
                                                }}
                                                {label}
                                            </A>
                                        }
                                            .into_view()
                                    }}
                                </li>
                            }
                        })
                        .collect::<Vec<_>>()
                }}
            </ol>
        </nav>
    }
}
