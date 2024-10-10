pub mod utils;

use std::rc::Rc;

use leptos::{logging::log, *};
use web_sys::MouseEvent;

use self::utils::ramp_experiment;
use crate::{components::button::Button, types::Experiment};

#[component]
pub fn experiment_ramp_form<NF>(
    experiment: Experiment,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let (traffic, set_traffic) = create_signal(experiment.traffic_percentage);
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let range_max = 100 / experiment.variants.len();
    let experiment_rc = Rc::new(experiment);
    let handle_ramp_experiment = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();
        let experiment_clone = experiment_rc.clone();
        let handle_submit_clone = handle_submit.clone();
        spawn_local(async move {
            let tenant = tenant_rs.get();
            let traffic_value = traffic.get();
            let _ = ramp_experiment(&experiment_clone.id, traffic_value, &tenant).await;
            req_inprogress_ws.set(false);
            handle_submit_clone()
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
                    <Button text="Set".to_string() on_click=handle_ramp_experiment.clone() loading/>
                }
            }}

        </form>
    }
}
