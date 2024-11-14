use futures::join;

use leptos::*;
use leptos_router::{use_navigate, use_params_map, ParamsMap};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{
    api::{fetch_context, fetch_default_config, fetch_dimensions},
    components::{
        contextual_override_form::ContextualOverrideForm,
        skeleton::{Skeleton, SkeletonVariant},
    },
    logic::Conditions,
    providers::editor_provider::EditorProvider,
    types::{Context, DefaultConfig, Dimension},
    utils::{add_prefix, use_service_prefix},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PageResource {
    dimensions: Vec<Dimension>,
    default_config: Vec<DefaultConfig>,
    condition: Conditions,
    overrides: Vec<(String, Value)>,
}

#[component]
pub fn update_contextual_override() -> impl IntoView {
    let tenant_rs = use_context::<Signal<String>>().unwrap();
    let navigate = use_navigate();
    let path_params = use_params_map();
    let service_prefix = use_service_prefix();

    let page_resource: Resource<(String, ParamsMap), PageResource> =
        create_blocking_resource(
            move || (tenant_rs.get(), path_params.get()),
            |(tenant, params)| async move {
                let id = params.get("id");

                if id.is_none() {
                    // enqueue error
                }

                let dimensions_future = fetch_dimensions(&tenant);
                let config_future = fetch_default_config(&tenant);
                let context_future = fetch_context(&tenant, &id.unwrap());

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
                        .unwrap_or(vec![])
                        .into_iter()
                        .filter(|d| d.dimension != "variantIds")
                        .collect(),
                    default_config: config_result.unwrap_or(vec![]),
                    condition: Conditions::from_context_json(&value).unwrap_or_default(),
                    overrides: override_.into_iter().collect::<Vec<(String, Value)>>(),
                }
            },
        );

    let on_submit = Callback::new(move |_| {
        let url_prefix = add_prefix("", &service_prefix);
        let tenant = tenant_rs.get();
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
                            class="h-main-content p-8 rounded-2xl border bg-white overflow-y-auto mx-auto"
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
