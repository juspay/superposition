use std::rc::Rc;

use leptos::*;

use crate::components::condition_pills::utils::extract_and_format;
use crate::components::table::table::Table;

use super::utils::gen_variant_table;
use crate::types::{Experiment, ExperimentStatusType};

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
    let experiment_rc = Rc::new(experiment.clone());
    let contexts = extract_and_format(&experiment_rc.clone().context);

    view! {
        <div class="flex flex-col overflow-x-auto p-2 bg-transparent">

            {
                let experiment_clone = experiment_rc.clone();
                move || {
                    let exp = experiment_clone.clone();
                    let class_name = match exp.status {
                        ExperimentStatusType::CREATED => {
                            "badge text-white ml-3 mb-1 badge-xl badge-info"
                        }
                        ExperimentStatusType::INPROGRESS => {
                            "badge text-white ml-3 mb-1 badge-xl badge-warning"
                        }
                        ExperimentStatusType::CONCLUDED => {
                            "badge text-white ml-3 mb-1 badge-xl badge-success"
                        }
                    };
                    view! {
                        <h1 class="text-2xl pt-4 font-extrabold">
                            {&exp.name} <span class=class_name>{exp.status.to_string()}</span>
                        </h1>
                    }
                }
            }
            <div class="divider"></div>
            <div class="flex flex-row justify-end join m-5">

                {
                    let experiment_clone = experiment_rc.clone();
                    move || {
                        let exp = experiment_clone.clone();
                        let handle_start = handle_start.clone();
                        let handle_conclude = handle_conclude.clone();
                        let handle_ramp = handle_ramp.clone();
                        let handle_edit = handle_edit.clone();
                        match exp.status {
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
                                        on:click=move |_| { handle_start(exp.id.to_string()) }
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
                                            {match exp.chosen_variant {
                                                Some(ref v) => format!("{}", v),
                                                None => String::new(),
                                            }}

                                        </div>
                                    </div>
                                }
                                    .into_view()
                            }
                        }
                    }
                }

            </div>
            <div class="flex bg-base-100 flex-row gap-2 justify-between flex-wrap shadow m-5">
                <div class="stat w-2/12">
                    <div class="stat-title">Experiment ID</div>
                    <div class="stat-value text-sm">{experiment.id}</div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Current Traffic Percentage</div>
                    <div class="stat-value text-sm">{experiment.traffic_percentage}</div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Created by</div>
                    <div class="stat-value text-sm">{experiment.created_by}</div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Created at</div>
                    <div class="stat-value text-sm">
                        {format!("{}", experiment.created_at.format("%v"))}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Last Modified</div>
                    <div class="stat-value text-sm">

                        {format!("{}", experiment.last_modified.format("%v"))}

                    </div>
                </div>
            </div> <div class="card bg-base-100 max-w-screen shadow m-5">
                <div class="card-body">
                    <h2 class="card-title">Context</h2>
                    <div class="flex flex-row flex-wrap gap-2">
                        {move || {
                            let mut view = Vec::new();
                            for token in contexts.clone() {
                                let (dimension, value) = (token.left_operand, token.right_operand);
                                view.push(
                                    view! {
                                        <div class="stat w-3/12">
                                            <div class="stat-title">{dimension}</div>
                                            <div class="stat-value text-base">
                                                {&value.replace("\"", "")}
                                            </div>
                                        </div>
                                    },
                                );
                            }
                            view
                        }}

                    </div>
                </div>
            </div> <div class="card bg-base-100 max-w-screen shadow m-5">
                <div class="card-body">
                    <h2 class="card-title">Variants</h2>
                    <div class="overflow-x-auto overflow-y-auto">

                        {
                            let experiment_clone = experiment_rc.clone();
                            move || {
                                let exp = experiment_clone.clone();
                                let (rows, columns) = gen_variant_table(&exp.variants).unwrap();
                                view! {
                                    <Table
                                        cell_style="min-w-48 font-mono".to_string()
                                        rows=rows
                                        key_column="overrides".to_string()
                                        columns=columns
                                    />
                                }
                            }
                        }

                    </div>
                </div>
            </div>
        </div>
    }
}
