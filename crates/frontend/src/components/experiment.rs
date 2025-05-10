use std::collections::HashMap;

use leptos::*;
use serde_json::{Map, Value};
use superposition_types::api::experiments::ExperimentResponse;
use superposition_types::database::models::experimentation::{
    ExperimentStatusType, Variant, VariantType,
};

use crate::components::table::types::Column;
use crate::components::table::Table;

use crate::logic::Conditions;
use crate::schema::HtmlDisplay;

fn badge_class(status_type: ExperimentStatusType) -> &'static str {
    match status_type {
        ExperimentStatusType::CREATED => "badge-info",
        ExperimentStatusType::INPROGRESS => "badge-warning",
        ExperimentStatusType::CONCLUDED => "badge-success",
        ExperimentStatusType::DISCARDED => "badge-neutral",
    }
}

use super::table::types::{
    default_column_formatter, default_formatter, ColumnSortable, Expandable,
};

#[allow(clippy::type_complexity)]
pub fn gen_variant_table(variants: &[Variant]) -> (Vec<Map<String, Value>>, Vec<Column>) {
    let mut columns = vec![Column::default_no_collapse("Config Key".into())];
    let mut row_map: HashMap<String, Map<String, Value>> = HashMap::new();
    for (i, variant) in variants.iter().enumerate() {
        let name = match variant.variant_type {
            VariantType::CONTROL => format!("{}", variant.variant_type),
            VariantType::EXPERIMENTAL => format!("Variant-{}", i),
        };
        columns.push(Column::new(
            name.clone(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Enabled(100),
            default_column_formatter,
        ));
        for (config, value) in variant.overrides.clone().into_inner().into_iter() {
            match row_map.get_mut(&config) {
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
    (rows, columns)
}

#[component]
pub fn experiment<HS, HR, HC, HE, HD>(
    experiment: ExperimentResponse,
    handle_start: HS,
    handle_ramp: HR,
    handle_conclude: HC,
    handle_edit: HE,
    handle_discard: HD,
) -> impl IntoView
where
    HS: Fn(String) + 'static + Clone,
    HR: Fn() + 'static + Clone,
    HC: Fn() + 'static + Clone,
    HE: Fn() + 'static + Clone,
    HD: Fn() + 'static + Clone,
{
    let experiment = store_value(experiment);
    let metrics_load_err = RwSignal::new(false);
    let contexts = experiment
        .with_value(|v| Conditions::from_context_json(&v.context).unwrap_or_default());
    let badge_class = format!(
        "badge text-white ml-3 mb-1 badge-xl {}",
        experiment.with_value(|v| badge_class(v.status))
    );
    let (variant_rows, variant_col) =
        gen_variant_table(&experiment.with_value(|v| v.variants.clone()));

    view! {
        <div class="flex flex-col overflow-x-auto p-2 bg-transparent">
            <h1 class="text-2xl pt-4 font-extrabold">
                {experiment.with_value(|v| v.name.clone())}
                <span class=badge_class>{experiment.with_value(|v| v.status.to_string())}</span>
            </h1>
            <div class="flex flex-row justify-end join m-5">

                {move || {
                    let handle_start = handle_start.clone();
                    let handle_conclude = handle_conclude.clone();
                    let handle_ramp = handle_ramp.clone();
                    let handle_edit = handle_edit.clone();
                    let handle_discard = handle_discard.clone();
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
                                    on:click=move |_| { handle_discard() }
                                >

                                    <i class="ri-delete-bin-line"></i>
                                    Discard
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
                        ExperimentStatusType::DISCARDED => view! { <></> }.into_view(),
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
                        {experiment.with_value(|v| *v.traffic_percentage)}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Created by</div>
                    <div class="stat-value text-sm">
                        {experiment.with_value(|v| v.created_by.clone())}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Description</div>
                    <div class="stat-value text-sm">
                        {experiment.with_value(|v| String::from(&v.description))}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Change Reason</div>
                    <div class="stat-value text-sm">
                        {experiment.with_value(|v| String::from(&v.change_reason))}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Created at</div>
                    <div class="stat-value text-sm">
                        {format!("{}", experiment.with_value(|v| v.created_at.format("%v %T")))}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Last Modified</div>
                    <div class="stat-value text-sm">

                        {format!("{}", experiment.with_value(|v| v.last_modified.format("%v %T")))}

                    </div>
                </div>
            </div>
            <div class="card bg-base-100 max-w-screen shadow m-5">
                <div class="card-body">
                    <h2 class="card-title">Context</h2>
                    <div class="flex flex-row flex-wrap gap-2">

                        {contexts
                            .iter()
                            .map(|condition| {
                                let dimension = condition.variable.clone();
                                let operand_views = condition
                                    .expression
                                    .to_constants_vec()
                                    .iter()
                                    .map(|c| {
                                        view! {
                                            <div class="stat-value text-base">{c.html_display()}</div>
                                        }
                                    })
                                    .collect_view();
                                view! {
                                    <div class="stat w-3/12">
                                        <div class="stat-title">{dimension}</div>
                                        {operand_views}
                                    </div>
                                }
                            })
                            .collect_view()}

                    </div>
                </div>
            </div>
            <Show when=move || {
                experiment
                    .with_value(|v| {
                        v.metrics.enabled || v.status == ExperimentStatusType::CREATED
                    })
            }>
                <div class="card bg-base-100 max-w-screen shadow m-5">
                    <div class="card-body collapse collapse-arrow">
                        <input type="checkbox" checked=true />
                        <h2 class="card-title collapse-title h-fit !p-0">Metrics</h2>
                        <div class="collapse-content !p-0">
                            {move || {
                                view! {
                                    {match experiment.with_value(|v| v.metrics_url.clone()) {
                                        Some(url) if !metrics_load_err.get() => {
                                            view! {
                                                <iframe
                                                    class="rounded-xl"
                                                    src=url.to_string()
                                                    width="100%"
                                                    height="750"
                                                    frameborder="0"
                                                    on:error=move |_| {
                                                        logging::log!("Error loading Grafana iframe");
                                                        metrics_load_err.set(true);
                                                    }
                                                />
                                            }
                                                .into_view()
                                        }
                                        _ => {
                                            let message = if experiment
                                                .with_value(|e| {
                                                    !e.metrics.enabled
                                                        && e.status == ExperimentStatusType::CREATED
                                                })
                                            {
                                                "Metrics not configured"
                                            } else if experiment
                                                .with_value(|e| {
                                                    e.status == ExperimentStatusType::CREATED
                                                })
                                            {
                                                "Experiment not started yet"
                                            } else {
                                                "Metrics not configured / Metrics data not available"
                                            };
                                            view! {
                                                <div class="stat-title h-[100px] !p-0 flex justify-center items-center">
                                                    {message}
                                                </div>
                                            }
                                                .into_view()
                                        }
                                    }}
                                }
                            }}
                        </div>
                    </div>
                </div>
            </Show>
            <div class="card bg-base-100 max-w-screen shadow m-5">
                <div class="card-body">
                    <h2 class="card-title">Variants</h2>
                    <Table
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
