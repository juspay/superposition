use std::str::FromStr;

use leptos::*;
use leptos_router::{A, use_location};

use crate::types::{BreadcrumbSegment, OrganisationId, RouteSegment, Workspace};
use crate::utils::use_url_base;

const SKIP_SEGMENTS: [RouteSegment; 4] = [
    RouteSegment::Admin,
    RouteSegment::Action,
    RouteSegment::Authz,
    RouteSegment::OrgAuthz,
];

/// Maps a URL path segment to a human-readable label.
/// Returns None for segments that should be skipped (like "action").
fn segment_to_label(segment: &str) -> String {
    let is_known_segment = RouteSegment::from_str(segment).is_ok();

    if !is_known_segment {
        segment.to_string()
    } else {
        segment
            .split('-')
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
            .join(" ")
    }
}

/// Builds the breadcrumb trail from the current URL path.
fn build_breadcrumbs(path: &str, base: &str) -> Vec<BreadcrumbSegment> {
    // Strip the base prefix if present
    let path = path.strip_prefix(base).unwrap_or(path);
    let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();

    let mut previous_segments = String::new();
    let mut breadcrumbs = Vec::new();
    for (i, segment) in segments.iter().enumerate() {
        let is_current = i == segments.len() - 1;
        let is_skipped = RouteSegment::try_from_str(segment)
            .map(|seg| SKIP_SEGMENTS.contains(&seg))
            .unwrap_or(false);

        let is_config_versions_config =
            *segment == "config" && segments.get(i + 1).copied() == Some("versions");
        if is_config_versions_config {
            previous_segments.push('/');
            previous_segments.push_str(segment);
            continue;
        }

        if !is_skipped {
            let label = segment_to_label(segment);
            let is_versions_in_config_flow = *segment == "versions"
                && i > 0
                && segments.get(i - 1).copied() == Some("config");

            if i == 1 {
                breadcrumbs.push(BreadcrumbSegment {
                    label: "Organisations".to_string(),
                    href: format!("{}/admin/organisations", base),
                    is_current: false,
                });
            }

            let href = if is_current {
                String::new()
            } else if i == 1 {
                format!("{}/admin/{}/workspaces", base, segment)
            } else if i == 2 {
                format!("{}{}/{}/default-config", base, previous_segments, segment)
            } else {
                format!("{}{}/{}", base, previous_segments, segment)
            };

            breadcrumbs.push(BreadcrumbSegment {
                label: if is_versions_in_config_flow {
                    "Versions".to_string()
                } else {
                    label
                },
                href,
                is_current,
            });
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
