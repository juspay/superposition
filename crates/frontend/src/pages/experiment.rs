use leptos::*;
use leptos_router::{use_navigate, use_params_map};
use serde::{Deserialize, Serialize};
use superposition_types::api::experiments::ExperimentResponse;

use crate::{
    api::fetch_experiment,
    components::{
        experiment::Experiment,
        experiment_action_form::ExperimentActionForm,
        experiment_conclude_form::ExperimentConcludeForm,
        modal::Modal,
        skeleton::{Skeleton, SkeletonVariant},
    },
    types::{OrganisationId, Workspace},
    utils::{close_modal, show_modal},
};

use crate::components::experiment_ramp_form::ExperimentRampForm;

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    experiment: Option<ExperimentResponse>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum PopupType {
    ExperimentStart,
    ExperimentPause,
    ExperimentResume,
    ExperimentDiscard,
    None,
}

#[component]
pub fn ExperimentPage() -> impl IntoView {
    let exp_params = use_params_map();
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let source = move || {
        let t = workspace.get().0;
        let org = org.get().0;
        let exp_id =
            exp_params.with(|params| params.get("id").cloned().unwrap_or("1".into()));
        (exp_id, t, org)
    };
    let (show_popup, set_show_popup) = create_signal(PopupType::None);

    let combined_resource: Resource<(String, String, String), CombinedResource> =
        create_blocking_resource(source, |(exp_id, workspace, org_id)| async move {
            // Perform all fetch operations concurrently
            let experiments_result = fetch_experiment(exp_id, &workspace, &org_id).await;
            CombinedResource {
                experiment: experiments_result.ok(),
            }
        });

    let handle_start = move || set_show_popup.set(PopupType::ExperimentStart);
    let handle_ramp = move || show_modal("ramp_form_modal");
    let handle_conclude = move || show_modal("conclude_form_modal");
    let handle_edit = move || {
        let navigate = use_navigate();
        let workspace = workspace.get().0;
        let org = org.get().0;
        let exp_id =
            exp_params.with(|params| params.get("id").cloned().unwrap_or("1".into()));
        let url = format!("/admin/{}/{}/experiments/{}/edit", org, workspace, exp_id);
        navigate(&url, Default::default());
    };
    let handle_discard = move || set_show_popup.set(PopupType::ExperimentDiscard);
    let handle_pause = move || set_show_popup.set(PopupType::ExperimentPause);
    let handle_resume = move || set_show_popup.set(PopupType::ExperimentResume);

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let resource = match combined_resource.get() {
                    Some(res) => res,
                    None => return view! { <h1>Error fetching experiment</h1> }.into_view(),
                };
                let experiment = resource.experiment;
                match experiment {
                    Some(experiment) => {
                        let experiment_rf = experiment.clone();
                        let experiment_cf = experiment.clone();
                        view! {
                            <Experiment
                                experiment=experiment.clone()
                                handle_start
                                handle_ramp
                                handle_conclude
                                handle_edit
                                handle_discard
                                handle_pause
                                handle_resume
                            />
                            <Modal
                                id="ramp_form_modal".to_string()
                                handle_close=move || { close_modal("ramp_form_modal") }
                            >

                                <ExperimentRampForm
                                    experiment=experiment_rf
                                    handle_submit=move || { combined_resource.refetch() }
                                />

                            </Modal>
                            {match show_popup.get() {
                                PopupType::None => ().into_view(),
                                popup_type => {
                                    let experiment_id = experiment.id.clone();
                                    view! {
                                        <ExperimentActionForm
                                            experiment_id=experiment_id.clone()
                                            popup_type
                                            handle_submit=move |_| {
                                                set_show_popup.set(PopupType::None);
                                                combined_resource.refetch()
                                            }
                                            handle_close=move |_| set_show_popup.set(PopupType::None)
                                        />
                                    }
                                }
                            }}
                            <Modal
                                id="conclude_form_modal".to_string()
                                handle_close=move || { close_modal("conclude_form_modal") }
                            >
                                <ExperimentConcludeForm
                                    experiment=experiment_cf
                                    handle_submit=move |_| { combined_resource.refetch() }
                                />
                            </Modal>
                        }
                            .into_view()
                    }
                    None => view! { <h1>Error fetching experiment</h1> }.into_view(),
                }
            }}
        </Suspense>
    }
}
