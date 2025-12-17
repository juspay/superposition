use leptos::*;
use leptos_router::use_navigate;

use crate::components::{
    alert::AlertType,
    button::Button,
    default_config_form::DefaultConfigForm,
    stat::Stat,
};
use crate::providers::alert_provider::enqueue_alert;
use crate::query_updater::use_signal_from_query;
use crate::types::{OrganisationId, Tenant};
use crate::utils::use_url_base;

#[component]
pub fn CreateDefaultConfig() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let navigate = use_navigate();
    let base = use_url_base();

    // Extract prefix from query parameters using the standard approach
    let prefix = use_signal_from_query(move |query_string| {
        url::Url::parse(&format!("http://placeholder.com/{}", query_string))
            .ok()
            .and_then(|url| url.query_pairs().find(|(key, _)| key == "prefix").map(|(_, value)| value.into_owned()))
            .unwrap_or_default()
    });

    // Create breadcrumb navigation
    let base_breadcrumb = base.clone();
    let bread_crumb_items = Signal::derive(move || {
        let mut items = vec![
            format!("{}/admin/{}/{}", base_breadcrumb, org.get().0, workspace.get().0),
            "Default Config".to_string(),
        ];
        
        let prefix_val = prefix.get();
        if !prefix_val.is_empty() {
            items.push(format!("Create in {}", prefix_val));
        } else {
            items.push("Create".to_string());
        }
        
        items
    });

    // Handle form submission - navigate back to list page
    let base_submit = base.clone();
    let nav_submit = navigate.clone();
    let org_submit = org;
    let workspace_submit = workspace;
    let handle_submit = Callback::new(move |_| {
        enqueue_alert(
            "Default config key created successfully!".to_string(),
            AlertType::Success,
            5000,
        );
        
        // Navigate back to the list page, preserving current prefix if any
        let current_prefix = prefix.get();
        let navigate_to = if current_prefix.is_empty() {
            format!("{}/admin/{}/{}/default-config", base_submit, org_submit.get().0, workspace_submit.get().0)
        } else {
            format!(
                "{}/admin/{}/{}/default-config?prefix={}",
                base_submit, org_submit.get().0, workspace_submit.get().0, current_prefix
            )
        };
        
        nav_submit(&navigate_to, Default::default());
    });

    // Handle cancel - navigate back to list page
    let base_cancel = base.clone();
    let nav_cancel = navigate.clone();
    let org_cancel = org;
    let workspace_cancel = workspace;
    let handle_cancel = Callback::new(move |_| {
        let current_prefix = prefix.get();
        let navigate_to = if current_prefix.is_empty() {
            format!("{}/admin/{}/{}/default-config", base_cancel, org_cancel.get().0, workspace_cancel.get().0)
        } else {
            format!(
                "{}/admin/{}/{}/default-config?prefix={}",
                base_cancel, org_cancel.get().0, workspace_cancel.get().0, current_prefix
            )
        };
        
        nav_cancel(&navigate_to, Default::default());
    });

    view! {
        <div class="h-full flex flex-col gap-6">
            // Header with title and stats
            <div class="flex justify-between items-start">
                <div>
                    <Stat
                        heading="Create New Default Config Key"
                        icon="ri-add-line"
                        number="".to_string()
                    />
                    // Breadcrumb navigation
                    <div class="breadcrumbs text-sm mt-2">
                        <ul>
                            <For
                                each=move || bread_crumb_items.get()
                                key=|item| item.clone()
                                children=move |item| {
                                    let base_bread = base.clone();
                                    let org_bread = org;
                                    let workspace_bread = workspace;
                                    if item.starts_with(&base_bread) {
                                        view! {
                                            <li>
                                                <a href=item class="link link-hover">
                                                    "Home"
                                                </a>
                                            </li>
                                        }
                                    } else if item == "Default Config" {
                                        view! {
                                            <li>
                                                <a href=format!("{}/admin/{}/{}/default-config", base_bread, org_bread.get().0, workspace_bread.get().0) class="link link-hover">
                                                    {item}
                                                </a>
                                            </li>
                                        }
                                    } else {
                                        view! {
                                            <li>{item}</li>
                                        }
                                    }
                                }
                            />
                        </ul>
                    </div>
                </div>
                <Button
                    on_click=move |_| handle_cancel.call(())
                    text="Cancel"
                    icon_class="ri-arrow-left-line"
                    style=crate::components::button::ButtonStyle::Outline
                />
            </div>

            // Main form container
            <div class="card w-full bg-base-100 rounded-lg overflow-hidden shadow flex-1">
                <div class="card-body overflow-y-auto">
                    <DefaultConfigForm
                        edit=false
                        prefix=if prefix.get().is_empty() { None } else { Some(prefix.get()) }
                        handle_submit=handle_submit
                    />
                </div>
            </div>
        </div>
    }
}