use chrono::{DateTime, Utc};
use leptos::{html::Input, logging::log, *};
use leptos_router::use_params_map;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use tracing::debug;
use web_sys::SubmitEvent;

use crate::{
    api::{fetch_default_config, fetch_dimensions},
    components::table::{table::Table, types::Column},
    pages::Home::Home::extract_and_format,
};

#[derive(
    Debug, Clone, Copy, PartialEq, Deserialize, Serialize, strum_macros::Display,
)]
#[strum(serialize_all = "UPPERCASE")]
pub(crate) enum ExperimentStatusType {
    CREATED,
    INPROGRESS,
    CONCLUDED,
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq, strum_macros::Display)]
#[strum(serialize_all = "UPPERCASE")]
pub(crate) enum VariantType {
    CONTROL,
    EXPERIMENTAL,
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Variant {
    pub id: String,
    pub override_id: String,
    pub context_id: String,
    pub overrides: Value,
    pub(crate) variant_type: VariantType,
}

pub type Variants = Vec<Variant>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Experiment {
    pub(crate) variants: Variants,
    pub(crate) name: String,
    pub(crate) id: String,
    pub(crate) traffic_percentage: u8,
    pub(crate) context: Value,
    pub(crate) status: ExperimentStatusType,
    pub(crate) override_keys: Value,
    pub(crate) created_by: String,
    pub(crate) created_at: DateTime<Utc>,
    pub(crate) last_modified: DateTime<Utc>,
    pub(crate) chosen_variant: Option<String>,
}

async fn get_experiment(exp_id: &String, tenant: &String) -> Result<Experiment, String> {
    let client = reqwest::Client::new();
    match client
        .get(format!("http://localhost:8080/experiments/{}", exp_id))
        .header("x-tenant", tenant)
        .send()
        .await
    {
        Ok(experiment) => {
            debug!("experiment response {:?}", experiment);
            Ok(experiment
                .json::<Experiment>()
                .await
                .map_err(|err| err.to_string())?)
        }
        Err(e) => Err(e.to_string()),
    }
}

async fn ramp_experiment(exp_id: &String, percent: u8) -> Result<Experiment, String> {
    let client = reqwest::Client::new();
    match client
        .patch(format!("http://localhost:8080/experiments/{}/ramp", exp_id))
        .header("x-tenant", "mjos")
        .json(&json!({ "traffic_percentage": percent }))
        .send()
        .await
    {
        Ok(experiment) => {
            debug!("experiment response {:?}", experiment);
            Ok(experiment
                .json::<Experiment>()
                .await
                .map_err(|err| err.to_string())?)
        }
        Err(e) => Err(e.to_string()),
    }
}

async fn conclude_experiment(
    exp_id: String,
    variant_id: String,
) -> Result<Experiment, String> {
    let client = reqwest::Client::new();
    match client
        .patch(format!(
            "http://localhost:8080/experiments/{}/conclude",
            exp_id
        ))
        .header("x-tenant", "mjos")
        .json(&json!({ "chosen_variant": variant_id }))
        .send()
        .await
    {
        Ok(experiment) => {
            debug!("experiment response {:?}", experiment);
            Ok(experiment
                .json::<Experiment>()
                .await
                .map_err(|err| err.to_string())?)
        }
        Err(e) => Err(e.to_string()),
    }
}

#[component]
pub fn experiment_page() -> impl IntoView {
    let exp_params = use_params_map();
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let source = move || {
        let t = tenant_rs.get();
        let exp_id =
            exp_params.with(|params| params.get("id").cloned().unwrap_or("1".into()));
        (exp_id, t)
    };

    let experiment_info = create_resource(source, |(exp_id, tenant)| async move {
        get_experiment(&exp_id, &tenant).await
    });
    view! {
        <Transition fallback=move || {
            view! { <h1>Loading....</h1> }
        }>
            {move || match experiment_info.get() {
                Some(Ok(experiment)) => {
                    experiment_detail_view(experiment, experiment_info).into_view()
                }
                Some(Err(err)) => view! { <h1>{err.to_string()}</h1> }.into_view(),
                None => view! { <h1>No elements</h1> }.into_view(),
            }}

        </Transition>
    }
}

fn experiment_detail_view(
    initial_data: Experiment,
    exp_resource: Resource<(String, String), Result<Experiment, String>>,
) -> impl IntoView {
    let contexts = extract_and_format(&initial_data.context);
    let (experiment, _) = create_signal(initial_data);
    let (ctxs, _) = create_signal(contexts);

    view! {
        <div class="flex flex-col overflow-x-auto p-2 bg-transparent">
            {move || {
                experiment
                    .with(|exp| {
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
                    })
            }}
            <div class="divider"></div>
            <div class="flex flex-row justify-end join m-5">
                {move || {
                    experiment
                        .with(|exp| {
                            match exp.status {
                                ExperimentStatusType::CREATED => {
                                    view! {
                                        <button
                                            class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                            onclick="edit_exp_modal.showModal()"
                                        >
                                            <i class="ri-edit-line"></i>
                                            Edit
                                        </button>
                                        <button
                                            class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                            value=&exp.id
                                            on:click=move |button_event| spawn_local(async move {
                                                let value = event_target_value(&button_event);
                                                let _ = ramp_experiment(&value, 1).await;
                                                exp_resource.refetch();
                                            })
                                        >

                                            <i class="ri-guide-line"></i>
                                            Start
                                        </button>
                                    }
                                }
                                ExperimentStatusType::INPROGRESS => {
                                    view! {
                                        <button
                                            class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                            onclick="conclude_exp_modal.showModal()"
                                        >
                                            <i class="ri-stop-circle-line"></i>
                                            Conclude
                                        </button>
                                        <button
                                            class="btn join-item text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lgont-medium rounded-lg text-sm px-5 py-2.5 text-center"
                                            onclick="ramp_exp_modal.showModal()"
                                        >
                                            <i class="ri-flight-takeoff-line"></i>
                                            Ramp
                                        </button>
                                    }
                                }
                                ExperimentStatusType::CONCLUDED => {
                                    view! {
                                        <></>
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
                                }
                            }
                        })
                }}

            </div>
            <div class="flex bg-base-100 flex-row gap-2 justify-between flex-wrap shadow m-5">
                <div class="stat w-2/12">
                    <div class="stat-title">Experiment ID</div>
                    <div class="stat-value text-sm">{experiment.get().id}</div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Current Traffic Percentage</div>
                    <div class="stat-value text-sm">
                        {move || experiment.get().traffic_percentage}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Created by</div>
                    <div class="stat-value text-sm">{experiment.get().created_by}</div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Created at</div>
                    <div class="stat-value text-sm">
                        {format!("{}", experiment.get().created_at.format("%v"))}
                    </div>
                </div>
                <div class="stat w-2/12">
                    <div class="stat-title">Last Modified</div>
                    <div class="stat-value text-sm">
                        {move || {
                            experiment.with(|exp| format!("{}", &exp.last_modified.format("%v")))
                        }}

                    </div>
                </div>
            </div> <div class="card bg-base-100 max-w-screen shadow m-5">
                <div class="card-body">
                    <h2 class="card-title">Context</h2>
                    <div class="flex flex-row flex-wrap gap-2">
                        {move || {
                            let context = ctxs.get();
                            let mut view = Vec::new();
                            let tokens = context.split("&&");
                            for token in tokens.into_iter() {
                                let mut t = token.trim().split(" ");
                                let (dimension, _, value) = (t.next(), t.next(), t.next());
                                view.push(
                                    view! {
                                        <div class="stat w-3/12">
                                            <div class="stat-title">
                                                {format!("{}", dimension.unwrap())}
                                            </div>
                                            <div class="stat-value text-base">
                                                {format!(
                                                    "{}",
                                                    &value.unwrap()[1..value.unwrap().chars().count() - 1],
                                                )}

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
                        {move || {
                            let exp = move || experiment.get();
                            let rows = gen_variant_rows(&exp().variants).unwrap();
                            let mut columns: Vec<Column> = Vec::new();
                            columns.push(Column::default("Variant".into()));
                            for okey in exp().override_keys.as_array().unwrap().into_iter() {
                                columns.push(Column::default(okey.as_str().unwrap().into()));
                            }
                            view! {
                                <Table
                                    table_style="abc".to_string()
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
        {add_dialogs(experiment, exp_resource)}
    }
}

fn gen_variant_rows(variants: &Vec<Variant>) -> Result<Vec<Map<String, Value>>, String> {
    let mut rows: Vec<Map<String, Value>> = Vec::new();
    for (i, variant) in variants.into_iter().enumerate() {
        let variant_name = match variant.variant_type {
            VariantType::CONTROL => "Control".into(),
            VariantType::EXPERIMENTAL => format!("Variant-{i}"),
        };
        let mut m = Map::new();
        m.insert("Variant".into(), variant_name.into());
        m.insert("variant_id".into(), variant.id.clone().into());
        for (o, value) in variant.overrides.as_object().unwrap().into_iter() {
            m.insert(o.clone(), value.clone());
        }
        rows.push(m);
    }
    Ok(rows)
}

fn add_dialogs(
    experiment_rs: ReadSignal<Experiment>,
    experiment_ws: Resource<(String, String), Result<Experiment, String>>,
) -> impl IntoView {
    let input_element: NodeRef<Input> = create_node_ref();
    let experiment = move || experiment_rs.get();
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (traffic, set_traffic) = create_signal(experiment().traffic_percentage);

    let on_submit = move |ev: SubmitEvent| {
        ev.prevent_default();
        let value = input_element
            .get()
            .expect("<input> to exist")
            .value_as_number() as u8;
        spawn_local(async move {
            let _ = ramp_experiment(&experiment().id, value).await;
            experiment_ws.refetch();
        });
    };

    let _dimensions = create_resource(
        move || tenant_rs.get(),
        |tenant| async {
            match fetch_dimensions(tenant).await {
                Ok(data) => data,
                Err(_) => vec![],
            }
        },
    );

    let _default_config = create_resource(
        move || tenant_rs.get(),
        |tenant| async {
            match fetch_default_config(tenant).await {
                Ok(data) => data,
                Err(_) => vec![],
            }
        },
    );

    match experiment_rs.get().status {
        ExperimentStatusType::CREATED => view! {
            <dialog id="edit_exp_modal" class="modal">
                <div class="modal-box">
                    <form method="dialog">
                        <button class="btn btn-sm btn-circle btn-ghost absolute right-2 top-2">
                            <i class="ri-close-line"></i>
                        </button>
                    </form>
                    <h3 class="font-bold text-lg">Edit Experiment</h3>
                    // <ExperimentForm
                    // name=experiment_rs.get().name
                    // context=vec![]
                    // variants=vec![]
                    // dimensions=dimensions.get().unwrap_or(vec![])
                    // default_config=default_config.get().unwrap_or(vec![])
                    // />
                    <div class="modal-action"></div>
                </div>
            </dialog>
        }
        .into_view(),
        ExperimentStatusType::INPROGRESS => view! {
            <dialog id="conclude_exp_modal" class="modal">
                <div class="modal-box">
                    <form method="dialog">
                        <button class="btn btn-sm btn-circle btn-ghost absolute right-2 top-2">
                            <i class="ri-close-line"></i>
                        </button>
                    </form>

                    <h3 class="font-bold text-lg">Conclude This Experiment</h3>
                    <p class="py-4">
                        Choose a variant to conclude with, this variant becomes
                        the new default that is served to requests that match this context
                    </p>
                    <form method="dialog">
                        {move || {
                            let mut view_arr = vec![];
                            for (i, v) in experiment_rs.get().variants.into_iter().enumerate() {
                                let (variant, _) = create_signal(v);
                                let view = match variant.get().variant_type {
                                    VariantType::CONTROL => {
                                        view! {
                                            <button
                                                class="btn btn-block btn-outline btn-info m-2"
                                                on:click=move |_| spawn_local(async move {
                                                    let e = experiment_rs.get();
                                                    let variant = variant.get();
                                                    conclude_experiment(e.id, variant.id.clone())
                                                        .await
                                                        .unwrap();
                                                    experiment_ws.refetch();
                                                })
                                            >

                                                Control
                                            </button>
                                        }
                                    }
                                    VariantType::EXPERIMENTAL => {
                                        view! {
                                            <button
                                                class="btn btn-block btn-outline btn-success m-2"
                                                on:click=move |_| spawn_local(async move {
                                                    let e = experiment_rs.get();
                                                    let variant = variant.get();
                                                    conclude_experiment(e.id, variant.id.clone())
                                                        .await
                                                        .unwrap();
                                                    experiment_ws.refetch();
                                                })
                                            >

                                                {format!("Variant-{i}")}
                                            </button>
                                        }
                                    }
                                };
                                view_arr.push(view);
                            }
                            view_arr
                        }}

                    </form>
                </div>
            </dialog>
            <dialog id="ramp_exp_modal" class="modal">
                <div class="modal-box">
                    <form method="dialog">
                        <button class="btn btn-sm btn-circle btn-ghost absolute right-2 top-2">
                            <i class="ri-close-line"></i>
                        </button>
                    </form>
                    <h3 class="font-bold text-lg">Ramp up with release</h3>
                    <p class="py-4">Increase the traffic being redirected to the variants</p>
                    <form method="dialog" on:submit=on_submit>
                        <p>{move || traffic.get()}</p>
                        <input
                            type="range"
                            min="0"
                            max="100"
                            node_ref=input_element
                            value=move || experiment_rs.get().traffic_percentage
                            class="range"
                            on:input=move |et| {
                                let t = event_target_value(&et).parse::<u8>().unwrap();
                                log!("traffic value:{t}");
                                set_traffic.set(t);
                            }
                        />

                        <button class="btn btn-block text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 hover:bg-gradient-to-br focus:ring-4 focus:outline-none focus:ring-purple-300 dark:focus:ring-purple-800 shadow-lg shadow-purple-500/50 dark:shadow-lg dark:shadow-purple-800/80 font-medium rounded-lg text-sm px-5 py-2.5 text-center me-2 mb-2">
                            Set
                        </button>
                    </form>
                </div>
            </dialog>
        }.into_view(),
        ExperimentStatusType::CONCLUDED => view! {}.into_view(),
    }
}
