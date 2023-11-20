use leptos::*;
use leptos_router::use_params_map;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tracing::debug;
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, Copy, PartialEq, Deserialize, Serialize, strum_macros::Display)]
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
    pub(crate) created_by: String,
    pub(crate) created_at: DateTime<Utc>,
    pub(crate) last_modified: DateTime<Utc>,
    pub(crate) chosen_variant: Option<Variant>,
}

async fn get_experiment(exp_id: &String) -> Result<Experiment, String> {
    let client = reqwest::Client::new();
    match client
        .get(format!("http://localhost:8080/experiments/{}", exp_id))
        .header("x-tenant", "mjos")
        .send()
        .await
    {
        Ok(experiment) => {
            debug!("experiment response {:?}", experiment);
            Ok(experiment
            .json::<Experiment>()
            .await
            .map_err(|err| err.to_string())?)
        },
        Err(e) => Err(e.to_string()),
    }
}

#[component]
pub fn experiment_page() -> impl IntoView {
    let exp_params = use_params_map();
    let experiment_id = move || exp_params.with(|params| params.get("id").cloned().unwrap_or("1".into()));
    let experiment_info = create_resource(
        experiment_id,
        |exp_id: String| async move  {
            get_experiment(&exp_id).await
        },
    );
    view! {
        <Transition
        fallback= move || view! {<h1> Loading.... </h1>} >
        {move ||
            match experiment_info.get() {
                Some(Ok(experiment)) => experiment_detail_view(&experiment).into_view(),
                Some(Err(err)) => view! {<h1>{err.to_string()}</h1>}.into_view(),
                None => view! {<h1>No elements </h1>}.into_view(),
            }
        }
        </Transition>
    }
}

fn experiment_detail_view(exp: &Experiment) -> impl IntoView {
    view! {
        <div class="flex flex-col overflow-x-auto p-2">
        <h1 class="text-4xl pt-4 font-extrabold">
            {&exp.name}
            <span class="badge ml-3 mb-1 badge-primary badge-lg">{exp.status.to_string()}</span>
        </h1>

        <div class="divider"></div>

        <div class="join m-5">
            <button class="btn join-item"><i class="ri-edit-line"></i>Edit</button>
            <button class="btn join-item"><i class="ri-stop-circle-line"></i>Conclude</button>
            <button class="btn join-item"><i class="ri-guide-line"></i>Release</button>
            <button class="btn join-item"><i class="ri-flight-takeoff-line"></i>Ramp</button>
        </div>

        <div class="stats shadow-xl mt-5">
            <div class="stat">
                <div class="stat-title">Experiment ID</div>
                <div class="stat-value">{&exp.id}</div>
            </div>
            <div class="stat">
                <div class="stat-title">Current Traffic Percentage</div>
                <div class="stat-value">{exp.traffic_percentage}</div>
            </div>
            <div class="stat">
                <div class="stat-title">Created by</div>
                <div class="stat-value">{&exp.created_by}</div>
            </div>
            <div class="stat">
                <div class="stat-title">Created at</div>
                <div class="stat-value">{format!("{}", &exp.created_at.format("%d-%m-%Y %H:%M:%S"))}</div>
            </div>
            <div class="stat">
                <div class="stat-title">Last Modified</div>
                <div class="stat-value">{format!("{}", &exp.last_modified.format("%d-%m-%Y %H:%M:%S"))}</div>
            </div>
        </div>

        <div class="card bg-base max-w-screen shadow-xl mt-5">
            <div class="card-body">
                <h2 class="card-title">Context</h2>
                <div class="flex flex-row">
                    <div class="stat">
                        <div class="stat-title">Client ID</div>
                        <div class="stat-value">cac</div>
                    </div>
                    <div class="divider divider-horizontal">&&</div>
                    <div class="stat">
                        <div class="stat-title">OS</div>
                        <div class="stat-value">android</div>
                    </div>
                </div>
            </div>
            </div>


            <div class="card bg-base max-w-screen shadow-xl mt-5">
            <div class="card-body">
                <h2 class="card-title">Variants</h2>
                <div class="overflow-x-auto">
                <table class="table">
                    <thead>
                    <tr class="bg-base-200">
                        <th></th>
                        <th>Key</th>
                        <th>Variant-1</th>
                        <th>Variant-2</th>
                        <th>Variant-3</th>
                        <th>Variant-4</th>
                        <th>Variant-5</th>
                        <th>Control</th>
                    </tr>
                    </thead>
                    <tbody>
                    <tr>
                        <th>1</th>
                        <td>pmTestKey1</td>
                        <td>Quality Control Specialist</td>
                        <td>Quality Control Specialist</td>
                        <td>Quality Control Specialist</td>
                        <td>Quality Control Specialist</td>
                        <td>Blue</td>
                        <td>Blue</td>
                    </tr>
                    <tr>
                        <th>2</th>
                        <td>pmTestKey2</td>
                        <td>Desktop Support Technician</td>
                        <td>Desktop Support Technician</td>
                        <td>Desktop Support Technician</td>
                        <td>Desktop Support Technician</td>
                        <td>Desktop Support Technician</td>
                        <td>Purple</td>
                    </tr>
                    <tr>
                        <th>3</th>
                        <td>pmTestKey3</td>
                        <td>Tax Accountant</td>
                        <td>Tax Accountant</td>
                        <td>Tax Accountant</td>
                        <td>Tax Accountant</td>
                        <td>Tax Accountant</td>
                        <td>Red</td>
                    </tr>
                    </tbody>
                </table>
                </div>
            </div>
        </div>
        </div>
    }
}
