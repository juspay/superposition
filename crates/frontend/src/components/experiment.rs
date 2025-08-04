use std::collections::HashMap;

use leptos::*;
use serde_json::{Map, Value};
use superposition_types::api::experiments::ExperimentResponse;
use superposition_types::database::models::experimentation::{
    ExperimentStatusType, Variant, VariantType,
};

use crate::components::{
    button::Button,
    table::{types::Column, Table},
};
use crate::logic::Conditions;
use crate::schema::HtmlDisplay;

fn badge_class(status_type: ExperimentStatusType) -> &'static str {
    match status_type {
        ExperimentStatusType::CREATED => "badge-info",
        ExperimentStatusType::INPROGRESS => "badge-warning",
        ExperimentStatusType::CONCLUDED => "badge-success",
        ExperimentStatusType::DISCARDED => "badge-neutral",
        ExperimentStatusType::PAUSED => "badge-error",
    }
}

#[component]
fn experiment_info(experiment: StoredValue<ExperimentResponse>) -> impl IntoView {
    view! {
        <div class="card bg-base-100 max-w-screen shadow">
            <div class="card-body flex flex-row gap-2 flex-wrap">
                <div class="h-fit w-[300px]">
                    <div class="stat-title">Experiment ID</div>
                    <div class="stat-value text-sm">{experiment.with_value(|v| v.id.clone())}</div>
                </div>
                <div class="h-fit w-[300px]">
                    <div class="stat-title">Description</div>
                    <div
                        class="tooltip tooltip-bottom w-[inherit] text-left"
                        data-tip=experiment.with_value(|v| String::from(&v.description))
                    >
                        <div class="stat-value text-sm text-ellipsis overflow-hidden">
                            {experiment.with_value(|v| String::from(&v.description))}
                        </div>
                    </div>
                </div>
                <div class="h-fit w-[300px]">
                    <div class="stat-title">Created by</div>
                    <div class="stat-value text-sm">
                        {experiment.with_value(|v| v.created_by.clone())}
                    </div>
                </div>
                <div class="h-fit w-[300px]">
                    <div class="stat-title">Created at</div>
                    <div class="stat-value text-sm">
                        {format!("{}", experiment.with_value(|v| v.created_at.format("%v %T")))}
                    </div>
                </div>
                <div class="h-fit w-[300px]">
                    <div class="stat-title">Traffic</div>
                    <div class="stat-value text-sm">
                        {experiment.with_value(|v| *v.traffic_percentage).to_string() + "%"}
                    </div>
                </div>
                {experiment
                    .with_value(|e| e.experiment_group_id.clone())
                    .map(|experiment_group_id| {
                        view! {
                            <div class="h-fit w-[300px]">
                                <div class="stat-title">Experiment group</div>
                                <div
                                    class="tooltip tooltip-bottom w-[inherit] text-left"
                                    data-tip=&experiment_group_id
                                >
                                    <div class="stat-value text-sm text-ellipsis overflow-hidden">
                                        {experiment_group_id}
                                    </div>
                                </div>
                            </div>
                        }
                            .into_view()
                    })
                    .unwrap_or_default()}
                {experiment
                    .with_value(|e| e.started_by.clone())
                    .map(|started_by| {
                        view! {
                            <div class="h-fit w-[300px]">
                                <div class="stat-title">Started by</div>
                                <div class="stat-value text-sm">{started_by}</div>
                            </div>
                        }
                            .into_view()
                    })
                    .unwrap_or_default()}
                {experiment
                    .with_value(|e| e.started_at)
                    .map(|started_at| {
                        view! {
                            <div class="h-fit w-[300px]">
                                <div class="stat-title">Started at</div>
                                <div class="stat-value text-sm">
                                    {format!("{}", started_at.format("%v %T"))}
                                </div>
                            </div>
                        }
                            .into_view()
                    })
                    .unwrap_or_default()}
                <div class="h-fit w-[300px]">
                    <div class="stat-title">Last Modified by</div>
                    <div class="stat-value text-sm">
                        {experiment.with_value(|v| v.last_modified_by.clone())}
                    </div>
                </div>
                <div class="h-fit w-[300px]">
                    <div class="stat-title">Last Modified at</div>
                    <div class="stat-value text-sm">
                        {format!("{}", experiment.with_value(|v| v.last_modified.format("%v %T")))}
                    </div>
                </div>
                <div class="h-fit w-[300px]">
                    <div class="stat-title">Change Reason</div>
                    <div
                        class="tooltip tooltip-bottom w-[inherit] text-left"
                        data-tip=experiment.with_value(|v| String::from(&v.change_reason))
                    >
                        <div class="stat-value text-sm text-ellipsis overflow-hidden">
                            {experiment.with_value(|v| String::from(&v.change_reason))}
                        </div>
                    </div>
                </div>
                <div class="h-fit w-[300px]">
                    <div class="stat-title">Experiment Type</div>
                    <div class="stat-value text-sm">
                        <span class="badge badge-neutral">
                            {experiment.with_value(|v| v.experiment_type.to_string())}
                        </span>
                    </div>
                </div>
            </div>
        </div>
    }
}

#[component]
fn create_actions<HS, HD, HE>(
    handle_start: HS,
    handle_discard: HD,
    handle_edit: HE,
) -> impl IntoView
where
    HS: Fn() + 'static + Clone,
    HD: Fn() + 'static + Clone,
    HE: Fn() + 'static + Clone,
{
    view! {
        <Button
            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
            on_click=move |_| handle_edit()
            icon_class="ri-edit-line"
            text="Edit"
        />
        <Button
            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
            on_click=move |_| handle_discard()
            icon_class="ri-delete-bin-line"
            text="Discard"
        />
        <Button
            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
            on_click=move |_| handle_start()
            icon_class="ri-guide-line"
            text="Start"
        />
    }
}

#[component]
fn inprogress_actions<HR, HC, HD, HP>(
    handle_ramp: HR,
    handle_conclude: HC,
    handle_discard: HD,
    handle_pause: HP,
) -> impl IntoView
where
    HR: Fn() + 'static + Clone,
    HC: Fn() + 'static + Clone,
    HD: Fn() + 'static + Clone,
    HP: Fn() + 'static + Clone,
{
    view! {
        <Button
            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg"
            on_click=move |_| handle_discard()
            icon_class="ri-delete-bin-line"
            text="Discard"
        />
        <Button
            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg"
            on_click=move |_| handle_conclude()
            icon_class="ri-stop-circle-line"
            text="Conclude"
        />
        <Button
            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg"
            on_click=move |_| handle_ramp()
            icon_class="ri-flight-takeoff-line"
            text="Ramp"
        />
        <Button
            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg"
            on_click=move |_| handle_pause()
            icon_class="ri-pause-line"
            text="Pause"
        />
    }
}

#[component]
fn conclude_actions(experiment: StoredValue<ExperimentResponse>) -> impl IntoView {
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
}

#[component]
fn discard_actions() -> impl IntoView {
    ().into_view()
}

#[component]
fn pause_actions<HR, HD>(handle_resume: HR, handle_discard: HD) -> impl IntoView
where
    HR: Fn() + 'static + Clone,
    HD: Fn() + 'static + Clone,
{
    view! {
        <Button
            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg"
            on_click=move |_| handle_resume()
            icon_class="ri-play-line"
            text="Resume"
        />
        <Button
            force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg"
            on_click=move |_| handle_discard()
            icon_class="ri-delete-bin-line"
            text="Discard"
        />
    }
}

#[allow(clippy::type_complexity)]
pub fn gen_variant_table(variants: &[Variant]) -> (Vec<Map<String, Value>>, Vec<Column>) {
    let mut columns = vec![Column::default_no_collapse("Config Key".into())];
    let mut row_map: HashMap<String, Map<String, Value>> = HashMap::new();
    for (i, variant) in variants.iter().enumerate() {
        let name = match variant.variant_type {
            VariantType::CONTROL => format!("{}", variant.variant_type),
            VariantType::EXPERIMENTAL => format!("Variant-{}", i),
        };
        columns.push(Column::default(name.clone()));
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
pub fn experiment<HS, HR, HC, HE, HD, HP, HRS>(
    experiment: ExperimentResponse,
    handle_start: HS,
    handle_ramp: HR,
    handle_conclude: HC,
    handle_edit: HE,
    handle_discard: HD,
    handle_pause: HP,
    handle_resume: HRS,
) -> impl IntoView
where
    HS: Fn() + 'static + Clone,
    HR: Fn() + 'static + Clone,
    HC: Fn() + 'static + Clone,
    HE: Fn() + 'static + Clone,
    HD: Fn() + 'static + Clone,
    HP: Fn() + 'static + Clone,
    HRS: Fn() + 'static + Clone,
{
    let experiment = store_value(experiment);
    let metrics_load_err = RwSignal::new(false);
    let contexts = experiment
        .with_value(|v| Conditions::from_context_json(&v.context).unwrap_or_default());
    let badge_class = format!(
        "badge text-white badge-xl {}",
        experiment.with_value(|v| badge_class(v.status))
    );
    let (variant_rows, variant_col) =
        gen_variant_table(&experiment.with_value(|v| v.variants.clone()));

    view! {
        <div class="flex flex-col gap-10 overflow-x-auto bg-transparent">
            <h1 class="flex gap-3 items-center text-2xl font-extrabold">
                {experiment.with_value(|v| v.name.clone())}
                <span class=badge_class>{experiment.with_value(|v| v.status.to_string())}</span>
            </h1>
            <div class="-mt-5 flex flex-row justify-end join">

                {move || {
                    let handle_start = handle_start.clone();
                    let handle_conclude = handle_conclude.clone();
                    let handle_ramp = handle_ramp.clone();
                    let handle_edit = handle_edit.clone();
                    let handle_discard = handle_discard.clone();
                    let handle_pause = handle_pause.clone();
                    let handle_resume = handle_resume.clone();
                    match experiment.with_value(|v| v.status) {
                        ExperimentStatusType::CREATED => {
                            view! { <CreateActions handle_start handle_discard handle_edit /> }
                        }
                        ExperimentStatusType::INPROGRESS => {
                            view! {
                                <InprogressActions
                                    handle_ramp
                                    handle_conclude
                                    handle_discard
                                    handle_pause
                                />
                            }
                        }
                        ExperimentStatusType::CONCLUDED => view! { <ConcludeActions experiment /> },
                        ExperimentStatusType::DISCARDED => view! { <DiscardActions /> },
                        ExperimentStatusType::PAUSED => {
                            view! { <PauseActions handle_resume handle_discard /> }
                        }
                    }
                }}

            </div>
            <ExperimentInfo experiment />
            <div class="card bg-base-100 max-w-screen shadow">
                <div class="card-body">
                    <h2 class="card-title">"Context"</h2>
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
                <div class="card bg-base-100 max-w-screen shadow">
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
            <div class="card bg-base-100 max-w-screen shadow">
                <div class="card-body">
                    <h2 class="card-title">"Variants"</h2>
                    <Table rows=variant_rows key_column="overrides" columns=variant_col />
                </div>
            </div>
        </div>
    }
}
