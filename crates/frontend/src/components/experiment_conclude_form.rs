pub mod utils;

use std::rc::Rc;

use leptos::*;
use superposition_types::{
    api::experiments::ExperimentResponse,
    database::models::experimentation::{Variant, VariantType},
};
use utils::conclude_experiment;

use crate::{
    components::alert::AlertType,
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Tenant},
};

#[component]
pub fn experiment_conclude_form<HS>(
    experiment: ExperimentResponse,
    handle_submit: HS,
) -> impl IntoView
where
    HS: Fn() + 'static + Clone,
{
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let experiment_rc = Rc::new(experiment);

    let experiment_clone = experiment_rc.clone();
    let handle_conclude_experiment = move |variant_id: String| {
        let handle_submit_clone = handle_submit.clone();
        spawn_local(async move {
            let experiment = experiment_clone.clone();
            let tenant = tenant_rws.get().0;
            let org = org_rws.get().0;
            let result =
                conclude_experiment(experiment.id.to_string(), variant_id, &tenant, &org)
                    .await;
            match result {
                Ok(_) => {
                    enqueue_alert(
                        String::from("Experiment concluded successfully!"),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
            handle_submit_clone();
        })
    };

    view! {
        <h3 class="font-bold text-lg">Conclude This Experiment</h3>
        <p class="py-4">
            Choose a variant to conclude with, this variant becomes
            the new default that is served to requests that match this context
        </p>
        <form method="dialog">
            <For
                each=move || {
                    experiment_rc
                        .variants
                        .iter()
                        .cloned()
                        .enumerate()
                        .collect::<Vec<(usize, Variant)>>()
                }

                key=|(_, variant)| variant.id.to_string()
                children=move |(idx, variant)| {
                    let variant = variant.clone();
                    let variant_type = variant.variant_type;
                    let variant_id = variant.id;
                    let handle_conclude_experiment_clone = handle_conclude_experiment.clone();
                    match variant_type {
                        VariantType::CONTROL => {
                            view! {
                                <button
                                    class="btn btn-block btn-outline btn-info m-2"
                                    on:click=move |_| {
                                        let handle_conclude_experiment_clone = handle_conclude_experiment_clone
                                            .clone();
                                        handle_conclude_experiment_clone(variant_id.to_string())
                                    }
                                >

                                    Control
                                </button>
                            }
                                .into_view()
                        }
                        VariantType::EXPERIMENTAL => {
                            {
                                view! {
                                    <button
                                        class="btn btn-block btn-outline btn-success m-2"
                                        on:click=move |_| {
                                            let handle_conclude_experiment_clone = handle_conclude_experiment_clone
                                                .clone();
                                            handle_conclude_experiment_clone(variant_id.to_string())
                                        }
                                    >

                                        {format!("Variant-{idx}")}
                                    </button>
                                }
                            }
                                .into_view()
                        }
                    }
                }
            />

        </form>
    }
}
