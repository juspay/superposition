use futures::join;

use leptos::*;
use leptos_router::{use_navigate, use_params_map, ParamsMap};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use superposition_types::{
    custom_query::PaginationParams,
    database::{
        models::cac::{Context, DefaultConfig},
        types::DimensionWithMandatory,
    },
};

use crate::{
    api::{fetch_context, fetch_default_config, fetch_dimensions},
    components::{
        contextual_override_form::ContextualOverrideForm,
        skeleton::{Skeleton, SkeletonVariant},
    },
    logic::Conditions,
    providers::editor_provider::EditorProvider,
    types::{OrganisationId, Tenant},
    utils::{add_prefix, use_service_prefix},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PageResource {
    dimensions: Vec<DimensionWithMandatory>,
    default_config: Vec<DefaultConfig>,
    condition: Conditions,
    overrides: Vec<(String, Value)>,
}

#[component]
pub fn update_contextual_override() -> impl IntoView {
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let navigate = use_navigate();
    let path_params = use_params_map();
    let service_prefix = use_service_prefix();

    let page_resource: Resource<(Tenant, ParamsMap, OrganisationId), PageResource> =
        create_blocking_resource(
            move || (tenant_s.get(), path_params.get(), org_s.get()),
            |(tenant, params, org)| async move {
                let id = params.get("id").unwrap();

                let d_filters = PaginationParams::all_entries();
                let dimensions_future = fetch_dimensions(&d_filters, &tenant, &org);
                let config_future = fetch_default_config(&d_filters, &tenant, &org);
                let context_future = fetch_context(&tenant, &id);

                let (dimensions_result, config_result, context_result) =
                    join!(dimensions_future, config_future, context_future);

                if dimensions_result.is_err()
                    || config_result.is_err()
                    || context_result.is_err()
                {
                    // enqueue error, ask to reload
                }

                let Context {
                    value, override_, ..
                } = context_result.unwrap();

                // Construct the combined result, handling errors as needed
                PageResource {
                    dimensions: dimensions_result
                        .unwrap_or_default()
                        .data
                        .into_iter()
                        .filter(|d| d.dimension != "variantIds")
                        .collect(),
                    default_config: config_result.unwrap_or_default().data,
                    condition: Conditions::from_context_json(&value).unwrap_or_default(),
                    overrides: override_.into_iter().collect::<Vec<(String, Value)>>(),
                }
            },
        );

    let on_submit = Callback::new(move |_| {
        let url_prefix = add_prefix("", &service_prefix);
        let tenant = tenant_s.get().0;
        navigate(
            format!("{url_prefix}/admin/{tenant}/overrides").as_str(),
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
                let PageResource { dimensions, default_config, condition, overrides } = resources
                    .unwrap();
                view! {
                    <EditorProvider>
                        <ContextualOverrideForm
                            edit=true
                            context=condition
                            overrides=overrides
                            width="w-[60%]"
                            class="h-main-content p-8 rounded-2xl border bg-white overflow-y-auto"
                            on_submit
                            dimensions
                            default_config
                        />
                    </EditorProvider>
                }
                    .into_view()
            }}
        </Suspense>
    }
}
