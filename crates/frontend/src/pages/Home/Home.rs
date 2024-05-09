use std::time::Duration;

use crate::components::condition_pills::condition_pills::ContextPills;
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::{
    api::{fetch_config, fetch_dimensions},
    components::{
        button::button::Button, context_form::context_form::ContextForm,
        dropdown::dropdown::DropdownDirection,
    },
    utils::{check_url_and_return_val, get_element_by_id, get_host},
};
use leptos::*;
use serde_json::{Map, Value};
use strum::EnumProperty;
use strum_macros::Display;
use wasm_bindgen::JsCast;
use web_sys::{
    HtmlButtonElement, HtmlInputElement, HtmlSelectElement, HtmlSpanElement, MouseEvent,
};

#[derive(Clone, Debug, Copy, Display, strum_macros::EnumProperty, PartialEq)]
enum ResolveTab {
    #[strum(props(id = "resolved_config_tab"))]
    ResolvedConfig,
    // #[strum(props(id = "selected_configs_tab"))]
    // SelectedConfig,
    #[strum(props(id = "all_configs_tab"))]
    AllConfig,
}

async fn resolve_config(tenant: String, context: String) -> Result<Value, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    let url = format!("{host}/config/resolve?{context}");
    match client
        .get(url)
        .query(&[("show_reasoning", "true")])
        .header("x-tenant", tenant)
        .send()
        .await
    {
        Ok(response) => {
            let config = response.json().await.map_err(|e| e.to_string())?;
            Ok(config)
        }
        Err(e) => Err(e.to_string()),
    }
}

fn gen_name_id(s0: &String, s1: &String, s2: &String) -> String {
    format!("{s0}::{s1}::{s2}")
}

#[component]
pub fn home() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let config_data = create_blocking_resource(
        move || tenant_rs.get(),
        move |tenant| fetch_config(tenant),
    );
    let dimension_resource = create_resource(
        move || tenant_rs.get(),
        |tenant| async {
            match fetch_dimensions(tenant).await {
                Ok(data) => data,
                Err(_) => vec![],
            }
        },
    );

    let (selected_tab_rs, selected_tab_ws) = create_signal(ResolveTab::AllConfig);

    let unstrike = |search_field_prefix: &String, config: &Map<String, Value>| {
        for (dimension, value) in config.into_iter() {
            let search_field_prefix = if search_field_prefix.is_empty() {
                dimension
            } else {
                &search_field_prefix
            };
            let search_field_prefix = gen_name_id(
                search_field_prefix,
                dimension,
                &value
                    .as_str()
                    .unwrap_or(&value.to_string().trim_matches('"')[..])
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

    let gen_query_context = |query: Vec<(String, String, String)>| -> String {
        let mut context: Vec<String> = vec![];
        for (dimension, op, value) in query.iter() {
            let op = match op.as_str() {
                "==" => "=",
                _ => break, // query params do not support the other operators :  != and IN, do something differently later
            };
            context.push(format!("{}{op}{}", dimension, value));
        }
        context.join("&").to_string()
    };

    let resolve_click = move |ev: MouseEvent| {
        ev.prevent_default();
        let dimension_labels = document().get_elements_by_name("context-dimension-name");
        let dimension_ops = document().get_elements_by_name("context-dimension-operator");
        let dimension_values = document().get_elements_by_name("context-dimension-value");
        let mut query_vector: Vec<(String, String, String)> = vec![];
        for i in 0..dimension_labels.length() {
            query_vector.push((
                dimension_labels
                    .item(i)
                    .expect("missing input")
                    .dyn_ref::<HtmlInputElement>()
                    .unwrap()
                    .value(),
                dimension_ops
                    .item(i)
                    .expect("missing input")
                    .dyn_ref::<HtmlSelectElement>()
                    .unwrap()
                    .value(),
                dimension_values
                    .item(i)
                    .expect("missing input")
                    .dyn_ref::<HtmlInputElement>()
                    .unwrap()
                    .value(),
            ))
        }
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
        logging::log!("query vector {:#?}", query_vector);
        // resolve the context and get the config that would apply
        spawn_local(async move {
            let context = gen_query_context(query_vector);
            let mut config = match resolve_config(tenant_rs.get(), context).await.unwrap()
            {
                Value::Object(m) => m,
                _ => Map::new(),
            };
            logging::log!("resolved config {:#?}", config);
            // unstrike those that we want to show the user
            // if metadata field is found, unstrike only that override
            match config.remove("metadata") {
                Some(Value::Array(metadata)) => {
                    if metadata.len() == 0 {
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
        });
    };
    view! {
        <div class="flex w-full flex-col flex-wrap mt-5 justify-evenly">
            <div class="mr-5 ml-5 mt-6">
                <Suspense fallback=move || {
                    view! { <Skeleton variant=SkeletonVariant::Block/> }
                }>
                    {move || {
                        dimension_resource
                            .with(|dimension| {
                                view! {
                                    <div class="card h-4/5 shadow bg-base-100">
                                        <div class="card flex flex-row m-2 bg-base-100">
                                            <div class="card-body">
                                                <h2 class="card-title">Resolve Configs</h2>

                                                <ContextForm
                                                    dimensions=dimension.to_owned().unwrap_or(vec![])
                                                    context=vec![]
                                                    heading_sub_text="Query your configs".to_string()
                                                    dropdown_direction=DropdownDirection::Right
                                                    is_standalone=false
                                                    resolve_mode=true
                                                    handle_change=|_| ()
                                                />
                                                <div class="card-actions mt-6 justify-end">
                                                    <Button
                                                        id="resolve_btn".to_string()
                                                        text="Resolve".to_string()
                                                        on_click=resolve_click
                                                    />
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
                    id=ResolveTab::AllConfig.get_str("id").expect("ID not defined for Resolve tab")
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
                                get_element_by_id::<HtmlButtonElement>("resolve_btn")
                                    .map(|btn| btn.click());
                            },
                            Duration::new(1, 0),
                        );
                    }
                >

                    All Contexts
                </a>
                <a
                    role="tab"
                    id=ResolveTab::ResolvedConfig
                        .get_str("id")
                        .expect("ID not defined for Resolve tab")
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
                                get_element_by_id::<HtmlButtonElement>("resolve_btn")
                                    .map(|btn| btn.click());
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
                                                <Skeleton variant=SkeletonVariant::Content/>
                                            </div>
                                        }
                                    }>
                                        {config_data
                                            .with(move |result| {
                                                match result {
                                                    Some(Ok(config)) => {
                                                        let rows = |k: &String, v: &Value, striked: bool| {
                                                            let mut view_vector = vec![];
                                                            let default_iter = vec![(k.clone(), v.clone())];
                                                            for (key, value) in v
                                                                .as_object()
                                                                .unwrap_or(&Map::from_iter(default_iter))
                                                                .iter()
                                                            {
                                                                let key = key.replace("\"", "").trim().to_string();
                                                                let value = value
                                                                    .as_str()
                                                                    .unwrap_or(&value.to_string().trim_matches('"')[..])
                                                                    .into();
                                                                let unique_name = gen_name_id(k, &key, &value);
                                                                view_vector
                                                                    .push(
                                                                        view! {
                                                                            < tr > < td class = "min-w-48 max-w-72 font-mono" > < span
                                                                            name = format!("{unique_name}-1") class = "config-name"
                                                                            class : text - black = { ! striked } class : font - bold = {
                                                                            ! striked } class : text - gray - 300 = { striked } > { key
                                                                            } </ span > </ td > < td class =
                                                                            "min-w-48 max-w-72 font-mono" style =
                                                                            "word-break: break-word;" > < span name =
                                                                            format!("{unique_name}-2") class = "config-value" class :
                                                                            text - black = { ! striked } class : font - bold = { !
                                                                            striked } class : text - gray - 300 = { striked } > {
                                                                            check_url_and_return_val(value) } </ span > </ td > </ tr >
                                                                        },
                                                                    )
                                                            }
                                                            view_vector
                                                        };
                                                        let contexts_views: Vec<_> = config
                                                            .contexts
                                                            .iter()
                                                            .map(|context| {
                                                                let rows: Vec<_> = context
                                                                    .override_with_keys
                                                                    .iter()
                                                                    .filter_map(|key| {
                                                                        let o = config.overrides.get(key);
                                                                        if o.is_some() { Some((key, o.unwrap())) } else { None }
                                                                    })
                                                                    .map(|(k, v)| { rows(&k, &v, true) })
                                                                    .collect();
                                                                view! {
                                                                    <div class="card bg-base-100 shadow m-6">
                                                                        <div class="card-body">
                                                                            <h2 class="card-title">
                                                                                <ContextPills context=context.condition.clone()/>
                                                                            </h2>
                                                                            <table class="table table-zebra mt-10">
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
                                                                }
                                                            })
                                                            .collect::<Vec<_>>();
                                                        let new_context_views = contexts_views
                                                            .into_iter()
                                                            .rev()
                                                            .collect::<Vec<_>>();
                                                        let default_config: Vec<_> = config
                                                            .default_configs
                                                            .iter()
                                                            .map(|(k, v)| { rows(&k, &v, false) })
                                                            .collect();
                                                        vec![
                                                            view! {
                                                                <div class="mb-4 overflow-y-scroll">
                                                                    {new_context_views}
                                                                    <div class="card bg-base-100 shadow m-6">
                                                                        <div class="card-body">
                                                                            <h2 class="card-title">Default Configuration</h2>
                                                                            <table class="table table-zebra">
                                                                                <thead>
                                                                                    <tr>
                                                                                        <th>Key</th>
                                                                                        <th>Value</th>
                                                                                    </tr>
                                                                                </thead>
                                                                                <tbody>{default_config}</tbody>
                                                                            </table>
                                                                        </div>
                                                                    </div>
                                                                </div>
                                                            },
                                                        ]
                                                    }
                                                    Some(Err(error)) => {
                                                        vec![
                                                            view! {
                                                                <div class="error">
                                                                    {"Failed to fetch config data: "} {error.to_string()}
                                                                </div>
                                                            },
                                                        ]
                                                    }
                                                    None => {
                                                        vec![
                                                            view! {
                                                                <div class="error">{"No config data fetched"}</div>
                                                            },
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
                                                <Skeleton variant=SkeletonVariant::Content/>
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
                    })
            }}

        </div>
    }
}
