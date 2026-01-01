use std::time::Duration;

use leptos::*;
use serde_json::{Map, Value};
use strum_macros::Display;
use superposition_types::{
    Config,
    api::{
        config::{ConfigQuery, ResolveConfigQuery},
        functions::FunctionEnvironment,
    },
    custom_query::{DimensionQuery, PaginationParams},
};
use wasm_bindgen::JsCast;
use web_sys::{HtmlButtonElement, HtmlSpanElement, MouseEvent};

use crate::components::condition_pills::Condition as ConditionComponent;
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::logic::Conditions;
use crate::providers::condition_collapse_provider::ConditionCollapseProvider;
use crate::types::{OrganisationId, Workspace};
use crate::{
    api::{dimensions, fetch_config, resolve_config},
    components::{button::Button, context_form::ContextForm},
    utils::{check_url_and_return_val, get_element_by_id},
};

#[derive(Clone, Debug, Copy, Display, strum_macros::EnumProperty, PartialEq)]
enum ResolveTab {
    ResolvedConfig,
    // SelectedConfig,
    AllConfig,
}

fn gen_name_id(s0: &String, s1: &String, s2: &String) -> String {
    format!("{s0}::{s1}::{s2}")
}

#[component]
fn AllContextView(config: Config) -> impl IntoView {
    let Config {
        contexts,
        overrides,
        default_configs,
        dimensions: _,
    } = config;
    let rows = |m: &Map<String, Value>, striked: bool| {
        m.iter()
            .map(|(key, value)| {
                let value = value
                    .as_str()
                    .unwrap_or(value.to_string().trim_matches('"'))
                    .into();
                let unique_name = gen_name_id(key, key, &value);
                view! {
                    <tr>
                        <td class="min-w-48 max-w-72">
                            <span
                                name=format!("{unique_name}-1")
                                class="config-name"
                                class:text-black=!striked
                                class:font-bold=!striked
                                class:text-gray-300=striked
                            >
                                {key}
                            </span>
                        </td>
                        <td class="min-w-48 max-w-72" style="word-break: break-word;">
                            <span
                                name=format!("{unique_name}-2")
                                class="config-value"
                                class:text-black=!striked
                                class:font-bold=!striked
                                class:text-gray-300=striked
                            >
                                {check_url_and_return_val(value)}
                            </span>
                        </td>
                    </tr>
                }
            })
            .collect_view()
    };

    view! {
        <div class="flex flex-col w-full gap-y-6 p-6">
            <ConditionCollapseProvider>

                {contexts
                    .iter()
                    .map(|context| {
                        let rows: Vec<_> = context
                            .override_with_keys
                            .iter()
                            .filter_map(|key| overrides.get(key).map(|o| rows(o, true)))
                            .collect();
                        let conditions: Conditions = context.try_into().unwrap_or_default();
                        view! {
                            <div class="card bg-base-100 shadow gap-4 p-6">
                                <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md m-0 w-max">
                                    "Condition"
                                </h3>
                                <div class="pl-5">
                                    <ConditionComponent
                                        conditions=conditions
                                        id=context.id.clone()
                                        class="xl:w-[400px] h-fit"
                                    />
                                    <div class="overflow-auto pt-5">
                                        <table class="table table-zebra">
                                            <thead>
                                                <tr>
                                                    <th>Key</th>
                                                    <th>Value</th>
                                                </tr>
                                            </thead>
                                            <tbody>{rows}</tbody>
                                        </table>
                                    </div>
                                </div>
                            </div>
                        }
                    })
                    .rev()
                    .collect::<Vec<_>>()}

            </ConditionCollapseProvider>
            <div class="card bg-base-100 shadow m-6">
                <div class="card-body">
                    <h2 class="card-title">"Default Configuration"</h2>
                    <table class="table table-zebra">
                        <thead>
                            <tr>
                                <th>Key</th>
                                <th>Value</th>
                            </tr>
                        </thead>
                        <tbody>

                            {rows(&default_configs, false)}

                        </tbody>
                    </table>
                </div>
            </div>
        </div>
    }
}

#[component]
pub fn Home() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let config_data = create_blocking_resource(
        move || (workspace.get().0, org.get().0),
        |(workspace, org)| async move {
            fetch_config(
                &DimensionQuery::default(),
                &ConfigQuery::default(),
                &workspace,
                &org,
            )
            .await
        },
    );
    let dimension_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0),
        |(workspace, org)| async move {
            dimensions::list(&PaginationParams::all_entries(), &workspace, &org)
                .await
                .unwrap_or_default()
        },
    );

    let (context_rs, context_ws) = create_signal::<Conditions>(Conditions::default());
    let (selected_tab_rs, selected_tab_ws) = create_signal(ResolveTab::AllConfig);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let fn_environment = Memo::new(move |_| FunctionEnvironment {
        context: context_rs.get().as_context_json(),
        overrides: Map::new(),
    });

    let unstrike = |search_field_prefix: &String, config: &Map<String, Value>| {
        for (dimension, value) in config.into_iter() {
            let search_field_prefix = if search_field_prefix.is_empty() {
                dimension
            } else {
                search_field_prefix
            };
            let search_field_prefix = gen_name_id(
                search_field_prefix,
                dimension,
                &value
                    .as_str()
                    .unwrap_or(value.to_string().trim_matches('"'))
                    .to_string(),
            );
            logging::log!("search field prefix {:#?}", search_field_prefix);
            let config_name_elements = document()
                .get_elements_by_name(format!("{search_field_prefix}-1").as_str());
            let config_value_elements = document()
                .get_elements_by_name(format!("{search_field_prefix}-2").as_str());
            logging::log!("config_name_elements {:#?}", config_name_elements.length());
            logging::log!(
                "config_value_elements {:#?}",
                config_value_elements.length()
            );
            for i in 0..config_name_elements.length() {
                let item_one = config_name_elements.item(i).expect("missing span");
                let item_two = config_value_elements.item(i).expect("missing span");

                let (config_name_element, config_value_element) = (
                    item_one.dyn_ref::<HtmlSpanElement>().unwrap(),
                    item_two.dyn_ref::<HtmlSpanElement>().unwrap(),
                );
                let _ = config_name_element
                    .class_list()
                    .add_2("text-black", "font-bold");
                let _ = config_name_element
                    .class_list()
                    .remove_2("text-gray-300", "line-through");
                let _ = config_value_element
                    .class_list()
                    .add_2("text-black", "font-bold");
                let _ = config_value_element
                    .class_list()
                    .remove_2("text-gray-300", "line-through");
                logging::log!(
                    "config name after replace {} and value {}",
                    config_name_element.to_string(),
                    config_value_element.to_string()
                );
            }
        }
    };

    let resolve_click = move |ev: MouseEvent| {
        ev.prevent_default();
        req_inprogress_ws.set(true);
        // strike out all config elements on the page
        let config_name_elements = document().get_elements_by_class_name("config-name");
        let config_value_elements = document().get_elements_by_class_name("config-value");
        for i in 0..config_name_elements.length() {
            let (config_name_element, config_value_element) = (
                config_name_elements.item(i).unwrap(),
                config_value_elements.item(i).unwrap(),
            );
            let _ = config_name_element
                .class_list()
                .remove_2("text-black", "font-bold");
            let _ = config_name_element
                .class_list()
                .add_2("text-gray-300", "line-through");
            let _ = config_value_element
                .class_list()
                .remove_2("text-black", "font-bold");
            let _ = config_value_element
                .class_list()
                .add_2("text-gray-300", "line-through");
        }
        let context_updated = context_rs.get();
        // resolve the context and get the config that would apply
        spawn_local(async move {
            let context = DimensionQuery::from(context_updated.as_resolve_context());
            let mut config = resolve_config(
                &context,
                &ResolveConfigQuery {
                    show_reasoning: Some(true),
                    ..Default::default()
                },
                &workspace.get_untracked().0,
                &org.get_untracked().0,
            )
            .await
            .unwrap_or_default();
            logging::log!("resolved config {:#?}", config);
            // unstrike those that we want to show the user
            // if metadata field is found, unstrike only that override
            match config.remove("metadata") {
                Some(Value::Array(metadata)) => {
                    if metadata.is_empty() {
                        logging::log!("unstrike default config");
                        unstrike(&String::new(), &config);
                    }
                    for applied in metadata.iter() {
                        logging::log!("applied config {:#?}", applied);
                        applied["override"]
                            .as_array()
                            .unwrap_or(&vec![])
                            .iter()
                            .for_each(|override_id| {
                                logging::log!("unstrike {:#?}", override_id);
                                unstrike(
                                    &override_id.as_str().unwrap().to_string(),
                                    &config,
                                )
                            });
                    }
                }
                _ => {
                    logging::log!(
                        "no metadata recieved, default config is the config to be used"
                    );
                }
            }
            logging::log!("unstrike default config if needed");
            unstrike(&String::new(), &config);

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
                                                <h2 class="card-title">"Resolve Configs"</h2>

                                                <ContextForm
                                                    dimensions=dimension
                                                    context=context_rs.get_untracked()
                                                    on_context_change=move |new_context| {
                                                        context_ws.set(new_context)
                                                    }
                                                    heading_sub_text="Query your configs".to_string()
                                                    resolve_mode=true
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
                    id="all_configs_tab"
                    class=move || match selected_tab_rs.get() {
                        ResolveTab::AllConfig => {
                            "tab tab-active [--tab-border-color:#a651f5] text-center"
                        }
                        _ => "tab",
                    }

                    on:click=move |_| {
                        selected_tab_ws.set(ResolveTab::AllConfig);
                        set_timeout(
                            || {
                                if let Some(btn) = get_element_by_id::<
                                    HtmlButtonElement,
                                >("resolve_btn") {
                                    btn.click()
                                }
                            },
                            Duration::new(1, 0),
                        );
                    }
                >

                    All Contexts
                </a>
                <a
                    role="tab"
                    id="resolved_config_tab"
                    class=move || match selected_tab_rs.get() {
                        ResolveTab::ResolvedConfig => {
                            "tab tab-active [--tab-border-color:orange] text-center"
                        }
                        _ => "tab",
                    }

                    on:click=move |_| {
                        selected_tab_ws.set(ResolveTab::ResolvedConfig);
                        set_timeout(
                            || {
                                if let Some(btn) = get_element_by_id::<
                                    HtmlButtonElement,
                                >("resolve_btn") {
                                    btn.click()
                                }
                            },
                            Duration::new(1, 0),
                        );
                    }
                >

                    Resolved Configuration
                </a>
            </div>
            {move || {
                selected_tab_rs
                    .with(|tab| {
                        match tab {
                            ResolveTab::AllConfig => {
                                view! {
                                    <Suspense fallback=move || {
                                        view! {
                                            <div class="m-6">
                                                <Skeleton variant=SkeletonVariant::Content />
                                            </div>
                                        }
                                    }>
                                        {config_data
                                            .with(move |result| {
                                                match result {
                                                    Some(Ok(config)) => {
                                                        vec![
                                                            view! { <AllContextView config=config.clone() /> }
                                                                .into_view(),
                                                        ]
                                                    }
                                                    Some(Err(error)) => {
                                                        vec![
                                                            view! {
                                                                <div class="error">
                                                                    Failed to fetch config data: {error.to_string()}
                                                                </div>
                                                            }
                                                                .into_view(),
                                                        ]
                                                    }
                                                    None => {
                                                        vec![
                                                            view! { <div class="error">No config data fetched</div> }
                                                                .into_view(),
                                                        ]
                                                    }
                                                }
                                            })}

                                    </Suspense>
                                }
                            }
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
                                                        let default_configs_map = (*config.default_configs).clone();
                                                        view! {
                                                            <div class="card m-6 shadow bg-base-100">
                                                                <div class="card-body">
                                                                    <h2 class="card-title">"Resolved Config"</h2>
                                                                    <table class="table table-zebra">
                                                                        <thead>
                                                                            <tr>
                                                                                <th>Config Key</th>
                                                                                <th>Value</th>
                                                                            </tr>
                                                                        </thead>
                                                                        <tbody id="resolved_table_body">
                                                                            <For
                                                                                each=move || { default_configs_map.clone().into_iter() }
                                                                                key=|(key, value)| format!("{key}-{value}")
                                                                                children=move |(config, value)| {
                                                                                    view! {
                                                                                        <tr class="min-w-48 max-w-72">
                                                                                            <td>{config}</td>
                                                                                            <td class="break-words">
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
                    })
            }}

        </div>
    }
}
