use leptos::*;
use serde_json::{Map, Value};
use std::collections::HashMap;

use crate::components::table::Table;
use crate::schema::HtmlDisplay;
use crate::types::{Experiment, ExperimentStatusType};
use crate::{
    components::table::types::Column,
    types::{Variant, VariantType},
};

pub fn gen_variant_table(
    variants: &[Variant],
) -> Result<(Vec<Map<String, Value>>, Vec<Column>), String> {
    let mut columns = vec![Column::default("Config Key".into())];
    let mut row_map: HashMap<&String, Map<String, Value>> = HashMap::new();
    for (i, variant) in variants.iter().enumerate() {
        let name = match variant.variant_type {
            VariantType::CONTROL => format!("{}", variant.variant_type),
            VariantType::EXPERIMENTAL => format!("Variant-{}", i),
        };
        columns.push(Column::new(name.clone(), None, |value: &str, _| {
            view! { <span>{value.to_string()}</span> }.into_view()
        }));
        for (config, value) in variant.overrides.iter() {
            match row_map.get_mut(config) {
                Some(c) => {
                    c.insert(name.clone(), value.clone());
                }
                None => {
                    let mut m = Map::new();
                    m.insert("Config Key".into(), Value::String(config.clone()));
                    m.insert(name.clone(), value.clone());
                    row_map.insert(config, m);
                }
            }
        }
    }
    let rows = row_map.into_values().collect();
    Ok((rows, columns))
}

#[component]
pub fn experiment<HS, HR, HC, HE>(
    experiment: Experiment,
    handle_start: HS,
    handle_ramp: HR,
    handle_conclude: HC,
    handle_edit: HE,
) -> impl IntoView
where
    HS: Fn(String) + 'static + Clone,
    HR: Fn() + 'static + Clone,
    HC: Fn() + 'static + Clone,
    HE: Fn() + 'static + Clone,
{
    let experiment = store_value(experiment);
    let contexts = experiment.with_value(|v| v.context.clone());
    let badge_class = match experiment.with_value(|v| v.status) {
        ExperimentStatusType::CREATED => "badge text-white ml-3 mb-1 badge-xl badge-info",
        ExperimentStatusType::INPROGRESS => {
            "badge text-white ml-3 mb-1 badge-xl badge-warning"
        }
        ExperimentStatusType::CONCLUDED => {
            "badge text-white ml-3 mb-1 badge-xl badge-success"
        }
    };
    let (variant_rows, variant_col) =
        gen_variant_table(&experiment.with_value(|v| v.variants.clone())).unwrap();

    view! {
        <div class="flex flex-col overflow-x-auto p-2 bg-transparent">
            <h1 class="text-2xl pt-4 font-extrabold">
                {experiment.with_value(|v| v.name.clone())}
                <span class=badge_class>{experiment.with_value(|v| v.status.to_string())}</span>
            </h1>
            <div class="divider"></div>
            <div class="flex flex-row justify-end join m-5">

                {move || {
                    let handle_start = handle_start.clone();
                    let handle_conclude = handle_conclude.clone();
                    let handle_ramp = handle_ramp.clone();
                    let handle_edit = handle_edit.clone();
                    match experiment.with_value(|v| v.status) {
                        ExperimentStatusType::CREATED => {
                            view! {
                                <button
                                    class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                    on:click=move |_| { handle_edit() }
                                >

                                    <i class="ri-edit-line"></i>
                                    Edit
                                </button>
                                <button
                                    class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                    on:click=move |_| {
                                        handle_start(experiment.with_value(|v| v.id.clone()))
                                    }
                                >

                                    <i class="ri-guide-line"></i>
                                    Start
                                </button>
                            }
                                .into_view()
                        }
                        ExperimentStatusType::INPROGRESS => {
                            view! {
                                <button
                                    class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                    on:click=move |_| { handle_conclude() }
                                >

                                    <i class="ri-stop-circle-line"></i>
                                    Conclude
                                </button>
                                <button
                                    class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                    on:click=move |_| { handle_ramp() }
                                >

                                    <i class="ri-flight-takeoff-line"></i>
                                    Ramp
                                </button>
                            }
                                .into_view()
                        }
                        ExperimentStatusType::CONCLUDED => {
                            view! {
                                <div class="stat">
                                    <div class="stat-title">Chosen Variant</div>
                                    <div class="stat-value">
                                        {match experiment.with_value(|v| v.chosen_variant.clone()) {
                                            Some(ref v) => v.to_string(),
                                            None => String::new(),
                                        }}

                                    </div>
                                </div>
                            }
                                .into_view()
                        }
                    }
                }}

            </div>
            <div class="flex bg-base-100 flex-row gap-2 justify-between flex-wrap shadow m-5">
                <div class="stat w-2/12">
                    <div class="stat-title">Experiment ID</div>
                    <div class="stat-value text-sm">{experiment.with_value(|v| v.id.clone())}</div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Current Traffic Percentage</div>
                    <div class="stat-value text-sm">
                        {experiment.with_value(|v| v.traffic_percentage)}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Created by</div>
                    <div class="stat-value text-sm">
                        {experiment.with_value(|v| v.created_by.clone())}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Created at</div>
                    <div class="stat-value text-sm">
                        {format!("{}", experiment.with_value(|v| v.created_at.format("%v")))}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Last Modified</div>
                    <div class="stat-value text-sm">

                        {format!("{}", experiment.with_value(|v| v.last_modified.format("%v")))}

                    </div>
                </div>
            </div>
            <div class="card bg-base-100 max-w-screen shadow m-5">
                <div class="card-body">
                    <h2 class="card-title">Context</h2>
                    <div class="flex flex-row flex-wrap gap-2">
                        {move || {
                            let mut view = Vec::new();
                            for token in contexts.clone() {
                                let (dimension, values) = (token.left_operand, token.right_operand);
                                let mut value_views = Vec::new();
                                for value in values.iter() {
                                    if value.is_object() && value.get("var").is_some() {
                                        continue;
                                    }
                                    value_views
                                        .push(
                                            view! {
                                                <div class="stat-value text-base">
                                                    {value.html_display()}
                                                </div>
                                            },
                                        );
                                }
                                view.push(
                                    view! {
                                        <div class="stat w-3/12">
                                            <div class="stat-title">{dimension}</div>
                                            {value_views}
                                        </div>
                                    },
                                );
                            }
                            view
                        }}

                    </div>
                </div>
            </div>
            <div class="card bg-base-100 max-w-screen shadow m-5">
                <div class="card-body">
                    <h2 class="card-title">Variants</h2>

                    <Table
                        cell_class="min-w-48 font-mono".to_string()
                        rows=variant_rows
                        key_column="overrides".to_string()
                        columns=variant_col
                        class="overflow-y-auto"
                    />

                </div>
            </div>
        </div>
    }
}
