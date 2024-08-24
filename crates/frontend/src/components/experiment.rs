pub mod utils;

use std::time::Duration;

use leptos::*;

use crate::components::condition_pills::{
    utils::extract_conditions, Condition as ConditionComponent,
};
use crate::components::table::Table;
use crate::providers::condition_collapse_provider::ConditionCollapseProvider;

use self::utils::gen_variant_table;
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
    let experiment = store_value(experiment);
    let contexts = extract_conditions(&experiment.get_value().context);

    let (copied_rs, copied_ws) = create_signal(false);


    let handle_id_copy = Callback::new(move |event: web_sys::MouseEvent| {
        event.prevent_default();

        let copy_code = format!(
            "navigator.clipboard.writeText('{}')",
            &experiment.with_value(|v| v.id.clone())
        );
        match js_sys::eval(&copy_code) {
            Ok(_) => {
                copied_ws.set(true);
                set_timeout(
                    move || {
                        copied_ws.set(false);
                    },
                    Duration::new(1, 0),
                );
            }
            Err(_) => logging::log!("unable to copy to clipboard"),
        }
    });

    view! {
        <div class="flex flex-col gap-4 p-6">

            {move || {
                let exp = experiment.get_value();
                let class_name = match exp.status {
                    ExperimentStatusType::CREATED => {
                        "badge text-white text-sm badge-md badge-info"
                    }
                    ExperimentStatusType::INPROGRESS => {
                        "badge text-white text-sm badge-md badge-warning"
                    }
                    ExperimentStatusType::CONCLUDED => {
                        "badge text-white text-sm badge-md badge-success"
                    }
                };
                let handle_start = handle_start.clone();
                let handle_conclude = handle_conclude.clone();
                let handle_ramp = handle_ramp.clone();
                let handle_edit = handle_edit.clone();
                view! {
                    <div class="flex justify-between items-center">
                        <div class="flex flex-col gap-2">
                            <h1 class="text-lg font-extrabold">
                                {&exp.name}
                            </h1>
                            <div class="flex gap-2">
                                <div class="stat p-0 gap-0.5">
                                    <div class="stat-title text-xs">Status</div>
                                    <span class=class_name>{exp.status.to_string()}</span>
                                </div>
                                <div class="divider divider-horizontal" />
                                <div class="stat p-0 gap-0.5">
                                    <div class="stat-title text-xs">ID</div>
                                    <div class="stat-value text-sm text-left">
                                        {exp.id.clone()}
                                        <Show when=move || !copied_rs.get()>
                                        <i class="ri-file-copy-line cursor-pointer ml-2" on:click:undelegated=move |e| { handle_id_copy.call(e); }></i>
                                        </Show>
                                        <Show when=move || copied_rs.get()>
                                            <i class="ri-check-double-line text-green-500 ml-2"></i>
                                        </Show>
                                    </div>
                                </div>
                                <div class="divider divider-horizontal" />
                                <div class="stat p-0 gap-0.5">
                                    <div class="stat-title text-xs">Author</div>
                                    <div class="stat-value text-sm text-left">
                                        {exp.created_by.clone()}
                                    </div>
                                </div>
                            </div>
                        </div>

                        <div class="join">

                            {match exp.status {
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
                                                    Some(ref v) => v.to_string(),
                                                    None => String::new(),
                                                }}

                                            </div>
                                        </div>
                                    }
                                        .into_view()
                                }
                            }}

                        </div>
                    </div>
                }
            }}
            // <div class="divider"></div>
            <div class="card bg-base-100 shadow w-full">
                <div class="card-title">
                    Context and Variants
                </div>
                <div class="card-body">

                </div>
            </div>
            <div class="flex max-md:flex-col gap-4 w-full">
                <div class="card bg-base-100 shadow md:w-[41.333333%]">
                    <div class="card-body md:flex-row gap-4">
                        <div class="flex flex-col gap-4">
                            <div class="stat p-0">
                                <div class="stat-title">Experiment ID</div>
                                <div class="stat-value text-sm">
                                    {experiment.with_value(|v| v.id.clone())}
                                </div>
                            </div>
                            <div class="stat p-0">
                                <div class="stat-title">Current Traffic Percentage</div>
                                <div class="stat-value text-sm">
                                    {experiment.with_value(|v| v.traffic_percentage)}
                                </div>
                            </div>
                            <div class="stat p-0">
                                <div class="stat-title">Created by</div>
                                <div class="stat-value text-sm">
                                    {experiment.with_value(|v| v.created_by.clone())}
                                </div>
                            </div>
                            <div class="stat p-0">
                                <div class="stat-title">Created at</div>
                                <div class="stat-value text-sm">
                                    {format!(
                                        "{}",
                                        experiment.with_value(|v| v.created_at.format("%v")),
                                    )}

                                </div>
                            </div>
                            <div class="stat p-0">
                                <div class="stat-title">Last Modified</div>
                                <div class="stat-value text-sm">

                                    {format!(
                                        "{}",
                                        experiment.with_value(|v| v.last_modified.format("%v")),
                                    )}

                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                <div class="card bg-base-100 shadow gap-4 md:w-[calc(100%-41.333333%-1rem)]">
                    <div class="card-body gap-4">
                        <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md font-mono m-0 w-max">
                            "Condition"
                        </h3>
                        // <h2 class="card-title">Context</h2>
                        <div class="pl-5">
                            <ConditionCollapseProvider>
                                <ConditionComponent
                                    conditions=contexts
                                    id=experiment.with_value(|v| v.id.clone())
                                    class="w-full h-fit"
                                />
                            </ConditionCollapseProvider>
                        </div>
                    </div>
                </div>
            </div> <div class="card bg-base-100 max-w-screen shadow">
                <div class="card-body">
                    <h2 class="card-title">Variants</h2>
                    <div class="overflow-x-auto overflow-y-auto">

                        {move || {
                            let exp = experiment.get_value();
                            let (rows, columns) = gen_variant_table(&exp.variants).unwrap();
                            view! {
                                <Table
                                    cell_style="min-w-48 font-mono".to_string()
                                    rows=rows
                                    key_column="overrides".to_string()
                                    columns=columns
                                />
                            }
                        }}

                    </div>
                </div>
            </div>
        </div>
    }
}
