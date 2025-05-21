use leptos::*;
use web_sys::MouseEvent;

use crate::{
    api::{pause_experiment, resume_experiment},
    components::{
        alert::AlertType, button::Button, change_form::ChangeForm,
        experiment_ramp_form::utils::ramp_experiment,
    },
    pages::experiment::PopupType,
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Tenant},
};

fn change_reason(popup_type: &PopupType) -> String {
    match popup_type {
        PopupType::ExperimentStart => "Starting experiment",
        _ => "",
    }
    .to_string()
}

#[component]
pub fn experiment_action_form(
    experiment_id: String,
    popup_type: PopupType,
    #[prop(into)] handle_submit: Callback<(), ()>,
    #[prop(into)] handle_close: Callback<(), ()>,
) -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (change_reason_rs, change_reason_ws) = create_signal(change_reason(&popup_type));
    let (req_inprogress_rs, req_inprogress_ws) = create_signal(false);
    let popup_type = StoredValue::new(popup_type);
    let experiment_id = StoredValue::new(experiment_id);

    let (header, message, action, button_title, button_icon) =
        match popup_type.get_value() {
            PopupType::ExperimentPause => (
                "Pause Experiment",
                "pause",
                "paused",
                "Pause",
                "ri-pause-fill",
            ),
            PopupType::ExperimentResume => (
                "Resume Experiment",
                "resume",
                "resumed",
                "Resume",
                "ri-play-mini-fill",
            ),
            PopupType::ExperimentStart => (
                "Start Experiment",
                "start",
                "started",
                "Start",
                "ri-guide-fill",
            ),
            _ => ("", "", "", "", ""),
        };

    let handle_experiment_action = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();
        let popup_type = popup_type.get_value();
        let experiment_id = experiment_id.get_value();
        let change_reason_value = change_reason_rs.get();

        spawn_local(async move {
            let tenant = tenant_rws.get_untracked().0;
            let org = org_rws.get_untracked().0;

            let result = match popup_type {
                PopupType::ExperimentPause => {
                    pause_experiment(&experiment_id, change_reason_value, &tenant, &org)
                        .await
                }
                PopupType::ExperimentResume => {
                    resume_experiment(&experiment_id, change_reason_value, &tenant, &org)
                        .await
                }
                PopupType::ExperimentStart => {
                    ramp_experiment(
                        &experiment_id,
                        0,
                        Some(change_reason_value),
                        &tenant,
                        &org,
                    )
                    .await
                }
                _ => Err("".to_string()),
            };

            req_inprogress_ws.set(false);
            match result {
                Ok(_) => {
                    handle_submit.call(());
                    let message = format!("Experiment {action} successfully!",);
                    enqueue_alert(message, AlertType::Success, 5000);
                }
                Err(e) => {
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        });
    };

    view! {
        <Portal>
            <div class="fixed inset-0 bg-black bg-opacity-50 z-50 flex items-center justify-center">
                <div class="relative box-content w-full max-w-md p-6 flex flex-col gap-4 bg-white rounded-lg shadow-lg">
                    <button
                        class="absolute right-2 top-2 btn btn-sm btn-circle btn-ghost"
                        on:click=move |_| { handle_close.call(()) }
                    >
                        <i class="ri-close-line" />
                    </button>
                    <h3 class="font-bold text-lg">{header}</h3>
                    <p class="py-2">
                        {format!("Are you sure you want to {message} this experiment?")}
                    </p>
                    <form class="flex flex-col gap-4">
                        <ChangeForm
                            title="Reason for Change".to_string()
                            placeholder="Enter a reason for this change".to_string()
                            value=change_reason_rs.get_untracked()
                            on_change=move |new_change_reason| {
                                change_reason_ws.set(new_change_reason)
                            }
                        />
                        {move || {
                            view! {
                                <Button
                                    text=button_title.to_string()
                                    on_click=handle_experiment_action
                                    loading=req_inprogress_rs.get()
                                    class="w-fit self-end".to_string()
                                    icon_class=button_icon.to_string()
                                />
                            }
                        }}
                    </form>
                </div>
            </div>
        </Portal>
    }
}
