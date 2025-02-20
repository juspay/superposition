use futures::join;
use leptos::*;
use leptos_router::use_params_map;
use serde::{Deserialize, Serialize};
use superposition_types::{
    custom_query::PaginationParams,
    database::{models::cac::DefaultConfig, types::DimensionWithMandatory},
};

use crate::{
    api::{fetch_default_config, fetch_dimensions, fetch_experiment},
    components::{
        experiment::Experiment,
        experiment_conclude_form::ExperimentConcludeForm,
        experiment_discard_form::ExperimentDiscardForm,
        experiment_form::ExperimentForm,
        experiment_ramp_form::utils::ramp_experiment,
        modal::Modal,
        skeleton::{Skeleton, SkeletonVariant},
    },
    providers::editor_provider::EditorProvider,
    types::{Experiment, OrganisationId, Tenant},
    utils::{close_modal, show_modal},
};

use crate::components::experiment_ramp_form::ExperimentRampForm;

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    experiment: Option<Experiment>,
    dimensions: Vec<DimensionWithMandatory>,
    default_config: Vec<DefaultConfig>,
}

#[component]
pub fn experiment_page() -> impl IntoView {
    let exp_params = use_params_map();
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let source = move || {
        let t = tenant_rws.get().0;
        let org = org_rws.get().0;
        let exp_id =
            exp_params.with(|params| params.get("id").cloned().unwrap_or("1".into()));
        (exp_id, t, org)
    };

    let combined_resource: Resource<(String, String, String), CombinedResource> =
        create_blocking_resource(source, |(exp_id, tenant, org_id)| async move {
            // Perform all fetch operations concurrently
            let experiments_future =
                fetch_experiment(exp_id.to_string(), tenant.to_string(), org_id.clone());
            let empty_list_filters = PaginationParams::all_entries();
            let dimensions_future =
                fetch_dimensions(&empty_list_filters, tenant.to_string(), org_id.clone());
            let config_future = fetch_default_config(
                &empty_list_filters,
                tenant.to_string(),
                org_id.clone(),
            );

            let (experiments_result, dimensions_result, config_result) =
                join!(experiments_future, dimensions_future, config_future);

            // Construct the combined result, handling errors as needed
            CombinedResource {
                experiment: experiments_result.ok().map(|v| v.into()),
                dimensions: dimensions_result
                    .unwrap_or_default()
                    .data
                    .into_iter()
                    .filter(|d| d.dimension != "variantIds")
                    .collect(),
                default_config: config_result.unwrap_or_default().data,
            }
        });

    let handle_start = move |experiment_id: String| {
        spawn_local(async move {
            let tenant = tenant_rws.get().0;
            let org = org_rws.get().0;
            let _ = ramp_experiment(&experiment_id, 0, &tenant, &org).await;
            combined_resource.refetch();
        })
    };

    let handle_ramp = move || show_modal("ramp_form_modal");
    let handle_conclude = move || show_modal("conclude_form_modal");
    let handle_edit = move || show_modal("experiment_edit_form_modal");
    let handle_discard = move || show_modal("experiment_discard_form_modal");

    view! {
        <Suspense fallback=move || {
            view! {
                <div class="m-4">
                    <Skeleton variant=SkeletonVariant::DetailPage />
                </div>
            }
        }>
            {move || {
                let resource = match combined_resource.get() {
                    Some(res) => res,
                    None => return view! { <h1>Error fetching experiment</h1> }.into_view(),
                };
                let experiment = resource.experiment;
                let default_config = resource.default_config;
                let dimensions = resource.dimensions;
                match experiment {
                    Some(experiment) => {
                        let experiment_rf = experiment.clone();
                        let experiment_cf = experiment.clone();
                        let experiment_ef = experiment.clone();
                        let experiment_df = experiment.clone();
                        view! {
                            <Experiment
                                experiment=experiment.clone()
                                handle_start=handle_start
                                handle_ramp=handle_ramp
                                handle_conclude=handle_conclude
                                handle_edit=handle_edit
                                handle_discard=handle_discard
                            />
                            <Modal
                                id="experiment_discard_form_modal".to_string()
                                handle_close=move || {
                                    close_modal("experiment_discard_form_modal")
                                }
                            >
                                <ExperimentDiscardForm
                                    experiment=experiment_df
                                    handle_submit=move || { combined_resource.refetch() }
                                />
                            </Modal>
                            <Modal
                                id="ramp_form_modal".to_string()
                                handle_close=move || { close_modal("ramp_form_modal") }
                            >

                                <ExperimentRampForm
                                    experiment=experiment_rf
                                    handle_submit=move || { combined_resource.refetch() }
                                />

                            </Modal>
                            <Modal
                                id="conclude_form_modal".to_string()
                                handle_close=move || { close_modal("conclude_form_modal") }
                            >

                                <ExperimentConcludeForm
                                    experiment=experiment_cf
                                    handle_submit=move || { combined_resource.refetch() }
                                />

                            </Modal>
                            <Modal
                                id="experiment_edit_form_modal".to_string()
                                classnames="w-12/12 max-w-5xl".to_string()
                                handle_close=move || { close_modal("experiment_edit_form_modal") }
                            >
                                <EditorProvider>
                                    <ExperimentForm
                                        edit=true
                                        id=experiment.id
                                        name=experiment_ef.name
                                        context=experiment_ef.context.clone()
                                        variants=FromIterator::from_iter(experiment_ef.variants)
                                        default_config=default_config
                                        dimensions=dimensions
                                        handle_submit=move || { combined_resource.refetch() }
                                    />
                                </EditorProvider>

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
