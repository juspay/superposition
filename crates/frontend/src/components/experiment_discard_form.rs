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
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let experiment_rc = Rc::new(experiment);

    let handle_discard_experiment = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();
        let experiment_clone = experiment_rc.clone();
        let handle_submit_clone = handle_submit.clone();
        spawn_local(async move {
            let tenant = tenant_rws.get().0;
            let org = org_rws.get().0;
            let change_reason_value = change_reason.get();
            let result = discard_experiment(
                &experiment_clone.id,
                &tenant,
                &org,
                change_reason_value,
            )
            .await;
            match result {
                Ok(_) => {
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
            req_inprogress_ws.set(false);
            handle_submit_clone()
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
                on_change=move |new_change_reason| {
                    set_change_reason.set(new_change_reason)
                }
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
