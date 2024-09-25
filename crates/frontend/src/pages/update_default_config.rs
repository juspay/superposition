use leptos::*;
use leptos_router::{use_params_map, use_query_map};
use serde_json::Value;

use crate::{
    api::fetch_default_config_key,
    components::{default_config_form::DefaultConfigForm, skeleton::Skeleton},
};

#[component]
pub fn update_default_config() -> impl IntoView {
    let tenant_rs = use_context::<Signal<String>>().unwrap();
    let path_params = use_params_map();
    let query_params = use_query_map();
    let page_resource = create_blocking_resource(
        move || (tenant_rs.get(), path_params.get(), query_params.get()),
        |(t, path_params, query_params)| async move {
            let prefix = query_params.get("prefix").cloned().unwrap_or_default();
            if let Some(value) = path_params.get("key") {
                let key = prefix + value;
                return fetch_default_config_key(&t, &key)
                    .await
                    .map_err(|e| e.to_string());
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
                let default_config = page_resource.get();
                let prefix = query_params.get().get("prefix").cloned();
                if default_config.is_none() {
                    return view! { <p>"Failed to load type template to update"</p> }.into_view();
                }
                match default_config.unwrap() {
                    Ok(dc) => {
                        view! {
                            <DefaultConfigForm
                                edit=true
                                config_key=dc.key
                                config_value=dc.value
                                type_schema=dc.schema
                                function_name=dc.function_name.map(|v| Value::String(v))
                                prefix
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
