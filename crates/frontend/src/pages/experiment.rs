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
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();
    let experiment_id_s =
        Signal::derive(move || use_params_map().get().get("id").unwrap().to_owned());

    let combined_resource: Resource<(String, String, String), CombinedResource> =
        create_blocking_resource(
            move || (experiment_id_s.get(), tenant_s.get().0, org_s.get().0),
            |(experiment_id, tenant, org_id)| async move {
                let empty_list_filters = PaginationParams::all_entries();

                let experiments_future =
                    fetch_experiment(&experiment_id, &tenant, &org_id);
                let dimensions_future =
                    fetch_dimensions(&empty_list_filters, &tenant, &org_id);
                let config_future =
                    fetch_default_config(&empty_list_filters, &tenant, &org_id);

                let (experiments_result, dimensions_result, config_result) =
                    join!(experiments_future, dimensions_future, config_future);

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
            },
        );

    let handle_start = move |experiment_id: String| {
        spawn_local(async move {
            let tenant = tenant_s.get().0;
            let org = org_s.get().0;
            let _ = ramp_experiment(&experiment_id, 0, &tenant, &org).await;
            combined_resource.refetch();
        })
    };

    let handle_ramp = move || show_modal("ramp_form_modal");
    let handle_conclude = move || show_modal("conclude_form_modal");
    let handle_edit = move || show_modal("experiment_edit_form_modal");

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
                        view! {
                            <Experiment
                                experiment=experiment.clone()
                                handle_start=handle_start
                                handle_ramp=handle_ramp
                                handle_conclude=handle_conclude
                                handle_edit=handle_edit
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
                                        handle_submit=move |_| { combined_resource.refetch() }
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
