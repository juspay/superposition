use futures::join;

use leptos::*;
use leptos_router::{use_navigate, use_query_map, ParamsMap};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use superposition_types::{
    custom_query::PaginationParams,
    database::{models::cac::DefaultConfig, types::DimensionWithMandatory},
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
    condition: Option<Conditions>,
    overrides: Option<Vec<(String, Value)>>,
}

#[component]
pub fn new_contextual_override() -> impl IntoView {
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let navigate = use_navigate();
    let service_prefix = use_service_prefix();
    let query_map = use_query_map();

    let page_resource: Resource<(Tenant, ParamsMap, OrganisationId), PageResource> =
        create_blocking_resource(
            move || (tenant_s.get(), query_map.get(), org_s.get()),
            |(tenant, query_map, org)| async move {
                let d_filters = PaginationParams::all_entries();
                let context_id = query_map.get("clone_from");
                let context = if let Some(id) = context_id {
                    let fut_result = fetch_context(&tenant, &id).await;
                    if fut_result.is_err() {
                        // enqueue error, ask to reload
                    }
                    fut_result.ok()
                } else {
                    None
                };
                let dimensions_future = fetch_dimensions(&d_filters, &tenant, &org);
                let config_future = fetch_default_config(&d_filters, &tenant, &org);

                let (dimensions_result, config_result) =
                    join!(dimensions_future, config_future);

                if dimensions_result.is_err() || config_result.is_err() {
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
                    default_config: config_result.unwrap_or_default().data,
                    condition: context
                        .as_ref()
                        .and_then(|v| Conditions::from_context_json(&v.value).ok()),
                    overrides: context.map(|v| {
                        v.override_.into_iter().collect::<Vec<(String, Value)>>()
                    }),
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
                let (condition, overrides) = match (condition, overrides) {
                    (Some(condition), Some(overrides)) => (condition, overrides),
                    _ => (Conditions::default(), vec![]),
                };
                view! {
                    <EditorProvider>
                        <ContextualOverrideForm
                            edit=false
                            context=condition
                            overrides=overrides
                            width="w-full xl:w-[60%]"
                            class="h-main-content p-8 rounded-2xl border bg-white overflow-y-auto mx-auto"
                            on_submit
                            dimensions
                            default_config
                        />
                    </EditorProvider>
                }
            }}
        </Suspense>
    }
}
/***
 */
