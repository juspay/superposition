use leptos::*;
use leptos_router::{A, use_location};

use crate::types::{BreadcrumbSegment, OrganisationId, Workspace};
use crate::utils::use_url_base;

const SKIP_SEGMENTS: [&str; 4] = ["admin", "action", "authz", "org-authz"];

/// Maps a URL path segment to a human-readable label.
/// Returns None for segments that should be skipped (like "action").
fn segment_to_label(segment: &str) -> Option<String> {
    let title_segments = [
        "types",
        "audit-log",
        "default-config",
        "experiment-groups",
        "compare",
        "config",
        "dimensions",
        "experiments",
        "function",
        "resolve",
        "secrets",
        "variables",
        "versions",
        "webhooks",
        "workspaces",
        "overrides",
        "create",
        "edit",
    ];
    match segment {
        x if !title_segments.contains(&x) => Some(segment.to_string()),
        x => Some(
            x.split('-')
                .map(|s| {
                    let mut chars = s.chars();
                    match chars.next() {
                        Some(first) => {
                            first.to_uppercase().collect::<String>() + chars.as_str()
                        }
                        None => String::new(),
                    }
                })
                .collect::<Vec<String>>()
                .join(" "),
        ),
    }
}

/// Builds the breadcrumb trail from the current URL path.
fn build_breadcrumbs(path: &str, base: &str) -> Vec<BreadcrumbSegment> {
    // Strip the base prefix if present
    let path = path.replace(base, "");
    let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();

    let mut previous_segments = String::new();
    let mut breadcrumbs = Vec::new();
    for (i, segment) in segments.iter().enumerate() {
        let is_current = i == segments.len() - 1;
        if !SKIP_SEGMENTS.contains(segment) {
            if let Some(label) = segment_to_label(segment) {
                breadcrumbs.push(BreadcrumbSegment {
                    label,
                    href: if is_current {
                        String::new()
                    } else {
                        let current = if previous_segments == "/admin" {
                            "organisations"
                        } else if previous_segments
                            .split("/")
                            .filter(|s| !s.is_empty())
                            .collect::<Vec<&str>>()
                            .len()
                            == 2
                        {
                            "workspaces"
                        } else {
                            segment
                        };
                        format!("{}{}/{}", base, previous_segments, current)
                    },
                    is_current,
                });
            }
        }
        previous_segments.push('/');
        previous_segments.push_str(segment);
    }
    breadcrumbs
}

/// Global navigation breadcrumbs component.
/// Displays a breadcrumb trail based on the current URL path.
/// Structure: <org> >> <workspace> >> <section> >> <detail>
#[component]
pub fn Breadcrumbs() -> impl IntoView {
    let location = use_location();
    let base = use_url_base();

    // Check if we have org/workspace context (for workspace-level pages)
    let org_context = use_context::<Signal<OrganisationId>>();
    let workspace_context = use_context::<Signal<Workspace>>();

    // Only show breadcrumbs if we have context (org or workspace level)
    let has_context = org_context.is_some() || workspace_context.is_some();

    let breadcrumbs = Signal::derive(move || {
        if !has_context {
            return Vec::new();
        }
        let path = location.pathname.get();
        build_breadcrumbs(&path, &base)
    });

    view! {
        <Show when=move || !breadcrumbs.get().is_empty() fallback=|| view! { <></> }>
            <nav class="flex items-center flex-wrap gap-1 text-sm mb-4" aria-label="Breadcrumb">
                <ol class="flex items-center flex-wrap gap-1">
                    <For
                        each=move || breadcrumbs.get()
                        key=|crumb| format!("{}-{}", crumb.label, crumb.href)
                        children=move |crumb: BreadcrumbSegment| {
                            view! {
                                <li class="flex items-center gap-1">
                                    <BreadcrumbItem item=crumb />
                                </li>
                            }
                        }
                    />
                </ol>
            </nav>
        </Show>
    }
}

/// Renders a single breadcrumb item - either a link or plain text.
#[component]
fn BreadcrumbItem(item: BreadcrumbSegment) -> impl IntoView {
    // is_current means this is the last item (current page), so no separator needed
    let show_separator = !item.is_current;

    view! {
        <div class="flex items-center gap-1">
            {if item.is_current || item.href.is_empty() {
                view! { <span class="font-bold text-gray-800">{item.label.clone()}</span> }
                    .into_view()
            } else {
                view! {
                    <A
                        href=item.href.clone()
                        class="text-blue-600 hover:text-blue-800 dark:text-blue-400 dark:hover:text-blue-300 hover:underline"
                    >
                        {item.label.clone()}
                    </A>
                }
                    .into_view()
            }}
            {if show_separator {
                view! {
                    <span class="text-gray-400 dark:text-gray-500">
                        <i class="ri-arrow-right-s-line"></i>
                    </span>
                }
                    .into_view()
            } else {
                ().into_view()
            }}
        </div>
    }
}
