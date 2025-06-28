pub mod utils;

use std::rc::Rc;

use leptos::*;
use superposition_types::api::experiments::ExperimentResponse;
use utils::discard_experiment;
use web_sys::MouseEvent;

use crate::{
    components::{alert::AlertType, button::Button, change_form::ChangeForm},
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Tenant},
};

#[component]
pub fn experiment_discard_form<NF>(
    experiment: ExperimentResponse,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let (change_reason, set_change_reason) = create_signal(String::new());
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let experiment_rc = Rc::new(experiment);

    let handle_discard_experiment = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();
        let experiment_id = experiment_rc.id.clone();
        let handle_submit_clone = handle_submit.clone();
        spawn_local(async move {
            let tenant = workspace.get().0;
            let org = org.get().0;
            let change_reason_value = change_reason.get();
            let result =
                discard_experiment(&experiment_id, &tenant, &org, change_reason_value)
                    .await;

            req_inprogress_ws.set(false);
            match result {
                Ok(_) => {
                    handle_submit_clone();
                    enqueue_alert(
                        String::from("Experiment discarded successfully!"),
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
        <h3 class="font-bold text-lg">Discard Experiment</h3>
        <p class="py-4">
            Safely discard the experiment without affecting any pre-existing overrides
        </p>
        <form class="flex flex-col gap-4">
            <ChangeForm
                title="Reason for Change".to_string()
                placeholder="Enter a reason for this change".to_string()
                value=change_reason.get_untracked()
                on_change=move |new_change_reason| { set_change_reason.set(new_change_reason) }
            />
            {move || {
                view! {
                    <Button
                        text="Discard".to_string()
                        on_click=handle_discard_experiment.clone()
                        loading=req_inprogess_rs.get()
                        class="w-fit self-end".to_string()
                    />
                }
            }}
        </form>
    }
}
