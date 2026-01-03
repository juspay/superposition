pub mod utils;

use std::rc::Rc;

use leptos::{logging::log, *};
use superposition_types::api::experiments::ExperimentResponse;
use web_sys::MouseEvent;

use self::utils::ramp_experiment;
use crate::{
    components::{alert::AlertType, button::Button},
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Workspace},
};

#[component]
pub fn experiment_ramp_form<NF>(
    experiment: ExperimentResponse,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let (traffic, set_traffic) = create_signal(*experiment.traffic_percentage);
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let range_max = 100 / experiment.variants.len();
    let experiment_rc = Rc::new(experiment);
    let handle_ramp_experiment = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();
        let experiment_clone = experiment_rc.clone();
        let handle_submit_clone = handle_submit.clone();
        spawn_local(async move {
            let workspace = workspace.get_untracked().0;
            let org = org.get_untracked().0;
            let traffic_value = traffic.get_untracked();
            let result = ramp_experiment(
                &experiment_clone.id,
                traffic_value,
                None,
                &workspace,
                &org,
            )
            .await;
            req_inprogress_ws.set(false);
            match result {
                Ok(_) => {
                    handle_submit_clone();
                    enqueue_alert(
                        String::from("Experiment ramped successfully!"),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        });
    };
    view! {
        <h3 class="font-bold text-lg">Ramp up with release</h3>
        <p class="py-4">Increase the traffic being redirected to the variants</p>
        <form>
            <p>{move || traffic.get()}</p>
            <input
                type="range"
                min="0"
                max=range_max.to_string()
                value=move || traffic.get()
                class="range"
                on:input=move |event| {
                    let traffic_value = event_target_value(&event).parse::<u8>().unwrap();
                    log!("traffic value:{traffic_value}");
                    set_traffic.set(traffic_value);
                }
            />

            {move || {
                let loading = req_inprogess_rs.get();
                view! {
                    <Button
                        text="Set".to_string()
                        on_click=handle_ramp_experiment.clone()
                        loading
                    />
                }
            }}

        </form>
    }
}
