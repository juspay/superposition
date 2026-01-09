pub mod utils;

use leptos::*;
use superposition_types::{
    api::experiments::ExperimentResponse, database::models::experimentation::VariantType,
};
use utils::conclude_experiment;

use crate::{
    api::discard_experiment,
    components::{alert::AlertType, change_form::ChangeForm},
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Workspace},
};

#[component]
pub fn ExperimentConcludeForm(
    experiment: ExperimentResponse,
    #[prop(into)] handle_submit: Callback<(), ()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let experiment = StoredValue::new(experiment);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let req_inprogess_rws = RwSignal::new(false);
    let force_discard_rws = RwSignal::new(None as Option<String>);

    let handle_conclude_experiment = move |variant_id: String| {
        req_inprogess_rws.set(true);
        spawn_local(async move {
            let workspace = workspace.get_untracked().0;
            let org = org.get_untracked().0;
            let result = conclude_experiment(
                experiment.with_value(|e| e.id.clone()),
                variant_id,
                change_reason_rs.get_untracked(),
                &workspace,
                &org,
            )
            .await;

            req_inprogess_rws.set(false);
            match result {
                Ok(Ok(_)) => {
                    handle_submit.call(());
                    enqueue_alert(
                        String::from("Experiment concluded successfully!"),
                        AlertType::Success,
                        5000,
                    );
                }
                Ok(Err(e)) => {
                    change_reason_ws
                        .set("Discarding, as context no longer exists".to_string());
                    force_discard_rws.set(Some(e.clone()));
                    enqueue_alert(e, AlertType::Error, 5000);
                }
                Err(e) => {
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        })
    };

    let handle_discard_experiment = move |change_reason: String| {
        req_inprogess_rws.set(true);
        let workspace = workspace.get_untracked().0;
        let org = org.get_untracked().0;
        spawn_local(async move {
            let result = discard_experiment(
                &experiment.with_value(|e| e.id.clone()),
                change_reason,
                &workspace,
                &org,
            )
            .await;
            req_inprogess_rws.set(false);
            match result {
                Ok(_) => {
                    handle_submit.call(());
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
        <h3 class="font-bold text-lg">Conclude This Experiment</h3>
        <p class="py-4">
            Choose a variant to conclude with, this variant becomes
            the new default that is served to requests that match this context
        </p>
        <form method="dialog" class="flex flex-col gap-3">
            {move || {
                view! {
                    <ChangeForm
                        title="Reason for Change".to_string()
                        placeholder="Enter a reason for this change".to_string()
                        value=change_reason_rs.get()
                        disabled=force_discard_rws.get().is_some()
                        on_change=move |new_change_reason| {
                            change_reason_ws.set(new_change_reason)
                        }
                    />
                }
            }}
            {move || {
                if let Some(force_discard_message) = force_discard_rws.get() {
                    let disabled = req_inprogess_rws.get();
                    view! {
                        <span class="text-red-500">
                            {format!("Error: {force_discard_message}")}
                        </span>
                        <button
                            disabled=disabled
                            class="btn btn-block btn-outline btn-info max-w-md"
                            class=("cursor-disabled", disabled)
                            on:click=move |e| {
                                e.prevent_default();
                                handle_discard_experiment(change_reason_rs.get_untracked());
                            }
                        >
                            Discard
                        </button>
                    }
                        .into_view()
                } else {
                    experiment
                        .with_value(|e| e.variants.clone())
                        .iter()
                        .enumerate()
                        .map(move |(idx, variant)| {
                            let variant = variant.clone();
                            let disabled = req_inprogess_rws.get();
                            let (label, button_type_class) = match variant.variant_type {
                                VariantType::CONTROL => ("Control".to_string(), "btn-info"),
                                VariantType::EXPERIMENTAL => {
                                    (format!("Variant-{idx}"), "btn-success")
                                }
                            };
                            view! {
                                <button
                                    disabled=disabled
                                    class=format!(
                                        "max-w-md btn btn-block btn-outline {button_type_class}",
                                    )
                                    class=("cursor-disabled", disabled)
                                    on:click=move |e| {
                                        e.prevent_default();
                                        handle_conclude_experiment(variant.id.to_string());
                                    }
                                >
                                    {label}
                                </button>
                            }
                        })
                        .collect_view()
                }
            }}
        </form>
    }
}
