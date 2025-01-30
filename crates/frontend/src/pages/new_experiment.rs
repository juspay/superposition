use futures::join;

use leptos::*;
use leptos_router::use_navigate;
use serde::{Deserialize, Serialize};

use crate::{
    api::{fetch_default_config, fetch_dimensions},
    components::{
        experiment_form::ExperimentForm,
        skeleton::{Skeleton, SkeletonVariant},
    },
    logic::Conditions,
    providers::editor_provider::EditorProvider,
    types::{OrganisationId, Tenant, VariantFormTs},
    utils::{add_prefix, use_service_prefix},
};

use superposition_types::{
    custom_query::PaginationParams,
    database::{models::cac::DefaultConfig, types::DimensionWithMandatory},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PageResource {
    dimensions: Vec<DimensionWithMandatory>,
    default_config: Vec<DefaultConfig>,
}

#[component]
pub fn new_experiment() -> impl IntoView {
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let navigate = use_navigate();
    let service_prefix = use_service_prefix();

    let page_resource: Resource<(Tenant, OrganisationId), PageResource> =
        create_blocking_resource(
            move || (tenant_s.get(), org_s.get()),
            |(tenant, org)| async move {
                let d_filters = PaginationParams::all_entries();

                let dimensions_future = fetch_dimensions(&d_filters, &tenant, &org);
                let default_config_future =
                    fetch_default_config(&d_filters, &tenant, &org);

                let (dimensions_result, default_config_result) =
                    join!(dimensions_future, default_config_future);

                if dimensions_result.is_err() || default_config_result.is_err() {
                    // enqueue error, ask to reload
                }

                // Construct the combined result, handling errors as needed
                PageResource {
                    dimensions: dimensions_result
                        .unwrap_or_default()
                        .data
                        .into_iter()
                        .filter(|d| d.dimension != "variantIds")
                        .collect(),
                    default_config: default_config_result.unwrap_or_default().data,
                }
            },
        );

    let on_submit = Callback::new(move |experiment_id: String| {
        let url_prefix = add_prefix("", &service_prefix);
        let tenant = tenant_s.get().0;
        let org = org_s.get().0;
        navigate(
            format!("{url_prefix}/admin/{org}/{tenant}/experiments/{experiment_id}")
                .as_str(),
            Default::default(),
        );
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let resources = page_resource.get();
                if resources.is_none() {
                    return view! { <p>"Reload"</p> }.into_view();
                }
                let PageResource { dimensions, default_config } = resources.unwrap();
                view! {
                    <EditorProvider>
                        <ExperimentForm
                            name="".to_string()
                            context=Conditions::default()
                            variants=VariantFormTs::default()
                            class="w-full h-main-content p-8 rounded-2xl border bg-white overflow-y-auto"
                            dimensions=dimensions
                            default_config=default_config
                            handle_submit=on_submit
                        />
                    </EditorProvider>
                }
                    .into_view()
            }}
        </Suspense>
    }
}
