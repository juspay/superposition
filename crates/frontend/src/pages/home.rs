use leptos::*;
use serde_json::{json, Value}; // Removed Map
use strum::EnumProperty;
use strum_macros::Display;
use superposition_types::custom_query::PaginationParams; // Removed Config
                                                         // Removed wasm_bindgen::JsCast;
use web_sys::MouseEvent; // Removed HtmlButtonElement

use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::logic::Conditions;
use crate::types::{OrganisationId, Tenant};
use crate::{
    api::{fetch_config, fetch_dimensions, resolve_config},
    components::{
        button::Button, context_form::ContextForm, dropdown::DropdownDirection,
    },
    utils::check_url_and_return_val, // Removed get_element_by_id
};

#[derive(Clone, Debug, Copy, Display, strum_macros::EnumProperty, PartialEq)]
enum ResolveTab {
    #[strum(props(id = "resolved_config_tab"))]
    ResolvedConfig,
    // #[strum(props(id = "selected_configs_tab"))]
    // SelectedConfig,
    // #[strum(props(id = "all_configs_tab"))]
    // AllConfig, // Removed AllConfig
}

#[component]
pub fn home() -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let config_data = create_blocking_resource(
        move || (tenant_rws.get().0, org_rws.get().0),
        |(tenant, org)| fetch_config(tenant, None, org),
    );
    let dimension_resource = create_resource(
        move || (tenant_rws.get().0, org_rws.get().0),
        |(tenant, org)| async {
            fetch_dimensions(&PaginationParams::all_entries(), tenant, org)
                .await
                .unwrap_or_default()
        },
    );

    let (context_rs, context_ws) = create_signal::<Conditions>(Conditions::default());
    let (selected_tab_rs, selected_tab_ws) = create_signal(ResolveTab::ResolvedConfig); // Default to ResolvedConfig
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let fn_environment = create_memo(move |_| {
        let context = context_rs.get();
        json!({
            "context": context,
            "overrides": [],
        })
    });

    let resolve_click = move |ev: MouseEvent| {
        ev.prevent_default();
        req_inprogress_ws.set(true);
        let context_updated = context_rs.get();
        // resolve the context and get the config that would apply
        spawn_local(async move {
            let context = context_updated.as_query_string();
            // Calling resolve_config with 5 arguments as per rustc error
            let config = resolve_config(
                &tenant_rws.get_untracked().0,
                &context,
                &org_rws.get_untracked().0,
                true, // show_reasoning: bool
                None, // context_id: Option<&String>
            )
            .await
            .unwrap_or_default();
            logging::log!("resolved config {:#?}", config);

            // The logic for unstriking elements has been removed as AllContextView is gone.
            // The selected_tab_rs check is still relevant for updating the correct table.
            if selected_tab_rs.get_untracked() == ResolveTab::ResolvedConfig {
                let resolution_card = document()
                    .get_element_by_id("resolved_table_body")
                    .expect("resolve table card not found");

                let mut table_rows = String::new();
                for (key, value) in config.iter() {
                    table_rows.push_str(
                    format!(
                        "<tr><td>{key}</td><td style='word-break: break-word;'>{}</td></tr>",
                        check_url_and_return_val(serde_json::from_value(value.to_owned()).unwrap_or(format!("{}", value)))
                    )
                    .as_str(),
                )
                }
                resolution_card.set_inner_html(&table_rows);
            }
            req_inprogress_ws.set(false);
        });
    };
    view! {
        <div class="w-full mt-5">
            <div class="mr-5 ml-5 mt-6">
                <Suspense fallback=move || {
                    view! { <Skeleton variant=SkeletonVariant::Block /> }
                }>
                    {move || {
                        dimension_resource
                            .with(|dimension| {
                                let dimension = dimension.to_owned().unwrap_or_default().data;
                                view! {
                                    <div class="card h-4/5 shadow bg-base-100">
                                        <div class="card flex flex-row m-2 bg-base-100">
                                            <div class="card-body">
                                                <h2 class="card-title">Resolve Configs</h2>

                                                <ContextForm
                                                    dimensions=dimension
                                                    context_rs
                                                    context_ws
                                                    heading_sub_text="Query your configs".to_string()
                                                    dropdown_direction=DropdownDirection::Right
                                                    resolve_mode=true
                                                    handle_change=move |new_context| {
                                                        context_ws
                                                            .update(|value| {
                                                                *value = new_context;
                                                            });
                                                    }
                                                    fn_environment
                                                />

                                                <div class="card-actions mt-6 justify-end">
                                                    {move || {
                                                        let loading = req_inprogess_rs.get();
                                                        view! {
                                                            <Button
                                                                id="resolve_btn".to_string()
                                                                text="Resolve".to_string()
                                                                on_click=resolve_click
                                                                loading=loading
                                                            />
                                                        }
                                                    }}

                                                </div>
                                            </div>
                                        </div>
                                    </div>
                                }
                            })
                    }}

                </Suspense>
            </div>
            <div role="tablist" class="tabs m-6 w-30 self-start tabs-lifted tabs-md">
                <a
                    role="tab"
                    id=ResolveTab::ResolvedConfig
                        .get_str("id")
                        .expect("ID not defined for Resolve tab")
                    class="tab tab-active [--tab-border-color:orange] text-center" // Simplified class, always active

                    on:click=move |_| {
                        selected_tab_ws.set(ResolveTab::ResolvedConfig);
                        // Removed automatic click on tab select
                    }
                >

                    Resolved Configuration
                </a>
            </div>
            {move || {
                // Since AllConfig is removed, this match is simpler.
                // It will always be ResolveTab::ResolvedConfig.
                // Consider removing the selected_tab_rs signal and match if this is the only view.
                // For now, keeping the structure for minimal changes.
                match selected_tab_rs.get() {
                    ResolveTab::ResolvedConfig => {
                        view! {
                            <Suspense fallback=move || {
                                        view! {
                                            <div class="m-6">
                                                <Skeleton variant=SkeletonVariant::Content />
                                            </div>
                                        }
                                    }>

                                        {config_data
                                            .with(move |conf| {
                                                match conf {
                                                    Some(Ok(config)) => {
                                                        let default_configs = config.default_configs.clone();
                                                        view! {
                                                            <div class="card m-6 shadow bg-base-100">
                                                                <div class="card-body">
                                                                    <h2 class="card-title">Resolved Config</h2>
                                                                    <table class="table table-zebra">
                                                                        <thead>
                                                                            <tr>
                                                                                <th>Config Key</th>
                                                                                <th>Value</th>
                                                                            </tr>
                                                                        </thead>
                                                                        <tbody id="resolved_table_body">
                                                                            <For
                                                                                each=move || { default_configs.clone().into_iter() }

                                                                                key=|(key, value)| format!("{key}-{value}")
                                                                                children=move |(config, value)| {
                                                                                    view! {
                                                                                        <tr class="min-w-48 max-w-72">
                                                                                            <td>{config}</td>
                                                                                            <td style="word-break: break-word;">
                                                                                                {match value {
                                                                                                    Value::String(s) => check_url_and_return_val(s),
                                                                                                    Value::Number(num) => num.to_string(),
                                                                                                    Value::Bool(b) => b.to_string(),
                                                                                                    _ => "".into(),
                                                                                                }}

                                                                                            </td>

                                                                                        </tr>
                                                                                    }
                                                                                }
                                                                            />

                                                                        </tbody>
                                                                    </table>
                                                                </div>
                                                            </div>
                                                        }
                                                    }
                                                    Some(Err(error)) => {
                                                        view! {
                                                            <div class="error">
                                                                {"Failed to fetch config data: "} {error.to_string()}
                                                            </div>
                                                        }
                                                    }
                                                    None => {
                                                        view! {
                                                            <div class="error">{"No config data fetched"}</div>
                                                        }
                                                    }
                                                }
                                            })}

                                    </Suspense>
                                }
                            }
                        }
            }}

        </div>
    }
}
