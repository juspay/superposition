use leptos::*;
use leptos_router::use_params_map;
use serde_json::Value;

use crate::{
    api::fetch_dimension,
    components::{dimension_form::DimensionForm, skeleton::Skeleton},
};

#[component]
pub fn update_dimension() -> impl IntoView {
    let tenant_rs = use_context::<Signal<String>>().unwrap();
    let path_params = use_params_map();
    let page_resource = create_blocking_resource(
        move || (tenant_rs.get(), path_params.get()),
        |(t, path_params)| async move {
            if let Some(value) = path_params.get("name") {
                return fetch_dimension(&t, value).await.map_err(|e| e.to_string());
            }
            logging::log!("failed to get key name from params");
            return Err("failed to load, refresh the page".to_string());
        },
    );
    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }.into_view()
        }>
            {move || {
                let dimension = page_resource.get();
                if dimension.is_none() {
                    return view! { <p>"Failed to load type template to update"</p> }.into_view();
                }
                match dimension.unwrap() {
                    Ok(d) => {
                        view! {
                            <DimensionForm
                                edit=true
                                position=d.position as u32
                                dimension_name=d.dimension
                                dimension_schema=d.schema
                                function_name=d.function_name.map(|v| Value::String(v))
                                handle_submit=move || {}
                                class="w-2/3 h-main-content p-8 rounded-2xl border bg-white overflow-y-auto"
                            />
                        }
                    }
                    Err(e) => view! { <p>{e}</p> }.into_view(),
                }
            }}
        </Suspense>
    }
}
